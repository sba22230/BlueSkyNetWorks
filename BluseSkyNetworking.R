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

# Safe extractor helpers (avoid errors if fields are missing)
safe_chr <- function(x, ...) {
    val <- purrr::pluck(x, ..., .default = NA_character_)
    if (is.null(val)) NA_character_ else as.character(val)
}
safe_int <- function(x, ...) {
    val <- purrr::pluck(x, ..., .default = NA_integer_)
    if (is.null(val)) NA_integer_ else as.integer(val)
}

# Step 1: Define helper functions to fetch reposts and threads
get_reposts_df <- function(uri) {
  reposts <- bs_get_reposts(uri, auth = bs_Auth, clean = TRUE)
  if (is.null(reposts) || !inherits(reposts, "data.frame") || nrow(reposts) == 0) {
    return(tibble(original_uri = character(), handle = character(), uri = character()))
  }
  reposts$original_uri <- uri
  reposts
}

get_thread_df <- function(uri) {
  thread <- bs_get_post_thread(uri, auth = bs_Auth, clean = FALSE)
  if (is.null(thread) || length(thread) == 0) {
    return(tibble(original_uri = character()))
  }
  tibble(
    original_uri = uri,
    author = purrr::map_chr(thread, ~ purrr::pluck(.x, "post", "author", "handle", .default = NA)),
    text   = purrr::map_chr(thread, ~ purrr::pluck(.x, "post", "record", "text", .default = NA)),
    uri    = purrr::map_chr(thread, ~ purrr::pluck(.x, "post", "uri", .default = NA))
  )
}

# Step 2: Search posts containing "Speirgorm"
resp <- bs_search_posts("Speirgorm", clean = FALSE, limit = 5000)

# Step 3: Normalize API response into a posts list
if (is.list(resp) && !is.null(resp$posts)) {
  posts <- resp$posts
} else if (is.list(resp) && length(resp) > 0 && is.list(resp[[1]]) && !is.null(resp[[1]]$posts)) {
  posts <- resp[[1]]$posts
} else if (is.list(resp) && length(resp) > 0 && all(vapply(resp, function(x) !is.null(x$uri), logical(1)))) {
  posts <- resp
} else {
  stop("Unexpected structure returned by bs_search_posts(); inspect 'resp'")
}

# Step 4: Extract key fields from posts
post_uri <- map_chr(posts, ~ safe_chr(.x, "uri"))
post_reply_parent_uri <- map_chr(posts, ~ safe_chr(.x, "record", "reply", "parent", "uri"))

# Step 5: Build posts_df with metadata
post_author <- map_chr(posts, ~ safe_chr(.x, "author", "handle"))
post_created <- map_chr(posts, ~ safe_chr(.x, "indexedAt"))
post_text <- map_chr(posts, ~ safe_chr(.x, "record", "text"))
post_likedCnt <- map_int(posts, ~ safe_int(.x, "likeCount"))
post_bookmrkCnt <- map_int(posts, ~ safe_int(.x, "bookmarkCount"))
post_rplyCnt <- map_int(posts, ~ safe_int(.x, "replyCount"))
post_rpstCount <- map_int(posts, ~ safe_int(.x, "repostCount"))

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

# Debug: inspect first few posts
print(head(posts_df))
cat("Posts dataframe rows:", nrow(posts_df), "\n")

# Step 6: Build reposts and threads data frames
reposts_df <- post_uri %>% map_dfr(get_reposts_df)
threads_df <- post_uri %>% map_dfr(get_thread_df)
cat("Reposts dataframe rows:", nrow(reposts_df), "\n")
cat("Threads dataframe rows:", nrow(threads_df), "\n")

# Debug: check reposts_df structure
print(head(reposts_df))

# Step 7: Build edge list (who reposted whom)
edges <- reposts_df %>%
  transmute(from = handle, to = original_uri) %>%
  distinct()
cat("Edges count:", nrow(edges), "\n")

# Debug: inspect edges
print(head(edges))

# Step 8: Build node list (unique actors and posts)
nodes <- tibble(name = unique(c(edges$from, edges$to)))
cat("Nodes count:", nrow(nodes), "\n")

# Debug: inspect nodes
print(head(nodes))

# Step 9: Build igraph object and plot basic network
g <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
cat("Graph summary:\n")
print(summary(g))

ggraph(g, layout = "fr") +
  geom_edge_link(alpha = 0.3) +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE)

# Step 10: Enrich edges with author info
edges <- reposts_df %>%
  left_join(posts_df %>% select(uri, author_handle),
            by = c("original_uri" = "uri")) %>%
  transmute(from = handle, to = author_handle, repost_uri = uri, created_at = created_at) %>%
  filter(!is.na(from) & !is.na(to)) %>%
  distinct()
cat("Enriched edges count:", nrow(edges), "\n")

# Debug: enriched edges
print(head(edges))

# Step 11: Enrich nodes with metadata
nodes <- bind_rows(
  reposts_df %>% select(name = handle, display_name, avatar, did),
  posts_df %>% select(name = author_handle, text)
) %>%
  distinct(name, .keep_all = TRUE)

# Add repost counts
nodes <- nodes %>%
  mutate(repost_count = table(edges$to)[name] %>% as.integer())
cat("Enriched nodes count:", nrow(nodes), "\n")

# Debug: enriched nodes
print(head(nodes))

# Step 12: Plot enriched network with ggraph
g <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
ggraph(g, layout = "fr") +
  geom_edge_link(alpha = 0.3) +
  geom_node_point(aes(size = repost_count, color = repost_count)) +
  geom_node_text(aes(label = display_name), repel = TRUE) +
  scale_size_continuous(range = c(3, 12)) +
  scale_color_gradient(low = "lightblue", high = "red") +
  theme_void()
cat("Final graph summary:\n")
print(summary(g))

# Step 13: Interactive visualization with visNetwork
library(visNetwork)

vis_nodes <- nodes %>%
  mutate(
    id = name,
    label = ifelse(is.na(display_name) | display_name == "", name, display_name),
    title = text,
    value = ifelse(is.na(repost_count), 0, repost_count)  # size by repost_count
  ) %>%
  distinct(id, .keep_all = TRUE)

vis_edges <- edges %>%
  transmute(
    from = from,
    to = to,
    arrows = "from"   # add arrow pointing to the 'to' node
  )


# Sort dropdown alphabetically by label
sorted_ids <- vis_nodes %>% arrange(label) %>% pull(id)

visNetwork(vis_nodes, vis_edges, width = "3000px", height = "1000px") %>%
  visOptions(highlightNearest = TRUE,
             nodesIdSelection = list(values = sorted_ids))
