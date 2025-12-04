library(bskyr)
library(dplyr)
library(lubridate)
library(igraph)
library(stringr)
library(tidyr)
library(purrr)
library(tibble)
library(ggraph)

## Authenticate with BlueSky
bs_user <- bs_get_user()
bs_pass <- bs_get_pass()
bs_Auth <- bs_auth(bs_user, bs_pass, save_auth = TRUE)


# me <- bs_get_profile(actor = bs_user, auth = bs_Auth)
# print(me)

# Extract uri, author, created_at, text, likeCount, bookmarkCount, replyCount
# repostCount, likeCount, quoteCount, and reply parent uri using safe extractors
# This avoids errors when fields or nested elements are missing in the API response.

# Safe extractor helpers using purrr::pluck (returns NA on missing)
safe_chr <- function(x, ...) {
  val <- purrr::pluck(x, ..., .default = NA_character_)
  if (is.null(val)) NA_character_ else as.character(val)
}
safe_int <- function(x, ...) {
  val <- purrr::pluck(x, ..., .default = NA_integer_)
  if (is.null(val)) NA_integer_ else as.integer(val)
}

# Step 1: Define a function to get reposts for a single URI
get_reposts_df <- function(uri) {
  reposts <- bs_get_reposts(uri, auth = bs_Auth, clean = TRUE)

  # If it's NULL or not a data frame, return an empty tibble
  if (
    is.null(reposts) || !inherits(reposts, "data.frame") || nrow(reposts) == 0
  ) {
    return(tibble(
      original_uri = character(),
      handle = character(),
      uri = character()
    ))
  }

  reposts$original_uri <- uri
  reposts
}

get_thread_df <- function(uri) {
  thread <- bs_get_post_thread(uri, auth = bs_Auth, clean = FALSE)

  if (is.null(thread) || length(thread) == 0) {
    return(tibble(original_uri = character()))
  }

  # Convert list output into a tibble with the fields you care about
  tibble(
    original_uri = uri,
    author = purrr::map_chr(
      thread,
      ~ purrr::pluck(.x, "post", "author", "handle", .default = NA)
    ),
    text = purrr::map_chr(
      thread,
      ~ purrr::pluck(.x, "post", "record", "text", .default = NA)
    ),
    uri = purrr::map_chr(
      thread,
      ~ purrr::pluck(.x, "post", "uri", .default = NA)
    )
  )
}


resp <- bs_search_posts("Speirgorm", clean = FALSE, limit = 5000)
# posts <- bs_get_post_thread('at://did:plc:znrkwcuzdbhduyqrbdpkl7pe/app.bsky.feed.post/3m54urlsan22g', 5, clean = FALSE)

# Detect where the list of posts lives in the API response and extract it.
# Possible shapes:
# - resp$posts (named element)
# - resp[[1]]$posts (wrapped in another list)
# - resp itself is already a list of posts (each element has aView  uri/author etc)
if (is.list(resp) && !is.null(resp$posts)) {
  posts <- resp$posts
} else if (
  is.list(resp) &&
    length(resp) > 0 &&
    is.list(resp[[1]]) &&
    !is.null(resp[[1]]$posts)
) {
  posts <- resp[[1]]$posts
} else if (
  is.list(resp) &&
    length(resp) > 0 &&
    all(vapply(resp, function(x) !is.null(x$uri), logical(1)))
) {
  posts <- resp
} else {
  stop(
    "Unexpected structure returned by bs_search_posts(); inspect 'resp' to adapt parsing"
  )
}
post_uri <- map_chr(posts, ~ safe_chr(.x, "uri"))
post_reply_parent_uri <- map_chr(
  posts,
  ~ safe_chr(.x, "record", "reply", "parent", "uri")
)
post_reply_parent_uri_df <- data.frame(
  uri = post_reply_parent_uri[!is.na(post_reply_parent_uri)]
)
#reposts <- bskyr::bs_get_reposts('at://did:plc:kba5ra5zizxsey2nq4pwnove/app.bsky.feed.post/3m5fx3ycixc2j', clean = FALSE)
posts_df <- post_reply_parent_uri_df$uri %>% map_dfr(get_reposts_df)
# Get reply texts and authors
reposts_df <- post_uri %>% purrr::map_dfr(get_reposts_df)
threads_df <- post_uri %>% purrr::map_dfr(get_thread_df)

# a <- get_reposts_df('at://did:plc:kba5ra5zizxsey2nq4pwnove/app.bsky.feed.post/3m5fx3ycixc2j')

post_uri <- map_chr(posts, ~ safe_chr(.x, "uri"))
post_author <- map_chr(posts, ~ safe_chr(.x, "author", "handle"))
post_created <- map_chr(posts, ~ safe_chr(.x, "indexedAt"))
post_text <- map_chr(posts, ~ safe_chr(.x, "record", "text"))
post_likedCnt <- map_int(posts, ~ safe_int(.x, "likeCount"))
post_bookmrkCnt <- map_int(posts, ~ safe_int(.x, "bookmarkCount"))
post_rplyCnt <- map_int(posts, ~ safe_int(.x, "replyCount"))
post_rpstCount <- map_int(posts, ~ safe_int(.x, "repostCount"))


# Build a tidy posts dataframe for downstream analysis and graph building
# Columns: uri, author_handle, indexedAt, text, like_count, bookmark_count,
# reply_count, repost_count, reply_parent_uri
posts_df <- tibble(
  uri = post_uri,
  author_handle = post_author,
  indexedAt = post_created,
  text = post_text,
  like_count = post_likedCnt,
  bookmark_count = post_bookmrkCnt,
  reply_count = post_rplyCnt,
  repost_count = post_rpstCount,
  reply_parent_uri = post_reply_parent_uri
)

# Quick peek (first rows)
print(dplyr::select(
  posts_df,
  uri,
  author_handle,
  indexedAt,
  like_count,
  reply_count
))


# Robust hashtag extractor from record$facets
# Returns a character vector of tags (without #) or character(0) if none
extract_hashtags <- function(record) {
  if (is.null(record) || is.null(record$facets)) {
    return(character(0))
  }
  facets <- record$facets
  tags <- purrr::map(facets, function(f) {
    # facet may contain a features list or a single feature
    feat <- tryCatch(f$features[[1]], error = function(e) NULL)
    if (is.null(feat)) {
      feat <- f$feature
    }
    if (is.null(feat)) {
      return(NULL)
    }

    # type can be in `$type` or `type`
    ttype <- NULL
    if (!is.null(feat[["$type"]])) {
      ttype <- feat[["$type"]]
    }
    if (is.null(ttype) && !is.null(feat[["type"]])) {
      ttype <- feat[["type"]]
    }

    # If the facet indicates a tag, return the tag field if present
    if (!is.null(ttype) && grepl("facet#tag", ttype)) {
      if (!is.null(feat$tag)) {
        return(feat$tag)
      }
      return(NULL)
    }

    # Some facet shapes may expose tag directly on the feature
    if (!is.null(feat$tag)) {
      return(feat$tag)
    }
    NULL
  })
  tags <- purrr::compact(tags)
  if (length(tags) == 0) character(0) else unique(unlist(tags))
}

# Extract hashtags for each post (list-column)
post_hashtags <- purrr::map(posts, function(x) {
  rec <- x$record
  extract_hashtags(rec)
})

# Attach as a list-column to posts_df
posts_df$hashtags <- post_hashtags

## Quick check: show posts with hashtags
print(head(dplyr::select(posts_df, uri, author_handle, hashtags)))


# 5. Build edge list (who replied to whom)
# Build edges directly from posts_df by joining reply_parent_uri back to posts_df$uri
edges <- reposts_df %>%
  transmute(from = handle, to = original_uri) %>%
  distinct()


# 6. Build node list
nodes <- tibble(name = unique(c(edges$from, edges$to)))

g <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)

ggraph(g, layout = "fr") +
  geom_edge_link(alpha = 0.3) +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE)


edges <- reposts_df %>%
  left_join(
    posts_df %>% select(uri, author_handle),
    by = c("original_uri" = "uri")
  ) %>%
  transmute(
    from = handle,
    to = author_handle,
    repost_uri = uri,
    created_at = created_at
  ) %>%
  filter(!is.na(from) & !is.na(to)) %>%
  distinct()

nodes <- bind_rows(
  reposts_df %>% select(name = handle, display_name, avatar, did, text),
  posts_df %>% select(name = author_handle)
) %>%
  distinct(name, .keep_all = TRUE)

nodes <- nodes %>%
  mutate(repost_count = table(edges$to)[name] %>% as.integer())

g <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)

ggraph(g, layout = "fr") +
  geom_edge_link(alpha = 0.3) +
  geom_node_point(aes(size = repost_count, color = repost_count)) +
  geom_node_text(aes(label = display_name), repel = TRUE) +
  scale_size_continuous(range = c(3, 12)) +
  scale_color_gradient(low = "lightblue", high = "red") +
  theme_void()

library(visNetwork)

vis_nodes <- nodes %>%
  mutate(
    id = name,
    label = ifelse(
      is.na(display_name) | display_name == "",
      name,
      display_name
    ),
    title = description,
    value = ifelse(is.na(repost_count), 0, repost_count) # replace NA with 0
  ) %>%
  distinct(id, .keep_all = TRUE)

vis_edges <- edges %>%
  transmute(from = from, to = to)

# Sort by the label (display_name if present, otherwise name)
sorted_ids <- vis_nodes %>%
  arrange(label) %>%
  pull(id)

sorted_labels <- vis_nodes %>% arrange(label) %>% pull(id)


visNetwork(vis_nodes, vis_edges) %>%
  visOptions(
    highlightNearest = TRUE,
    nodesIdSelection = list(values = sorted_ids)
  )
