library(bskyr)
library(furrr)
library(dplyr)
library(lubridate)
library(igraph)
library(stringr)
library(stringi)
library(tidyr)
library(purrr)
library(tibble)
library(ggraph)
library(visNetwork)
library(retry)
library(rgexf)
library(readr)
library(arrow)

## Authenticate
bs_user <- bs_get_user()
bs_pass <- bs_get_pass()
bs_auth <- bs_auth(bs_user, bs_pass, save_auth = TRUE)

## Safe extractors
safe_chr <- function(x, ...) purrr::pluck(x, ..., .default = NA_character_)
safe_int <- function(x, ...) purrr::pluck(x, ..., .default = NA_integer_)

## Deduplication
dedup_posts <- function(posts) distinct(as_tibble(posts), uri, .keep_all = TRUE)

## Retry search page
search_page <- function(query, cursor = NULL, limit = 100) {
  retry(
    bs_search_posts(query = query, limit = limit, cursor = cursor,
                    clean = FALSE, auth = bs_auth),
    when = "error", max_tries = 5, interval = runif(1, 1.0, 2.0)
  )
}

## Parse posts
get_posts_from_resp <- function(resp) {
  if (is.list(resp) && !is.null(resp$posts)) resp$posts
  else if (is.list(resp) && length(resp) > 0 &&
           all(vapply(resp, function(x) is.list(x) && !is.null(x$posts), logical(1))))
    flatten(map(resp, "posts"))
  else if (is.list(resp) && length(resp) > 0 &&
           all(vapply(resp, function(x) !is.null(x$uri), logical(1)))) resp
  else list()
}

## Deep search with until anchoring
deep_search_posts <- function(query, hard_limit = 50000, chunk_limit = 100,
                              checkpoint_path = "posts_checkpoint.parquet") {
  all_rows <- list(); cursor <- NULL; until <- NULL; fetched <- 0
  while (fetched < hard_limit) {
    q <- if (!is.null(until)) paste0(query, " until:", until) else query
    resp <- search_page(q, cursor, chunk_limit)
    posts <- get_posts_from_resp(resp)
    if (length(posts) == 0) break
    df <- tibble(
      uri = map_chr(posts, ~ safe_chr(.x, "uri")),
      indexedAt = map_chr(posts, ~ safe_chr(.x, "indexedAt")),
      author_handle = map_chr(posts, ~ safe_chr(.x, "author", "handle")),
      text = map_chr(posts, ~ safe_chr(.x, "record", "text")),
      like_count = map_int(posts, ~ safe_int(.x, "likeCount")),
      bookmark_count = map_int(posts, ~ safe_int(.x, "bookmarkCount")),
      reply_count = map_int(posts, ~ safe_int(.x, "replyCount")),
      repost_count = map_int(posts, ~ safe_int(.x, "repostCount")),
      reply_parent_uri = map_chr(posts, ~ safe_chr(.x, "record", "reply", "parent", "uri"))
    )
    all_rows <- append(all_rows, list(df))
    fetched <- nrow(bind_rows(all_rows))
    message(sprintf("Fetched %d rows", fetched))
    if (fetched %% 1000 < chunk_limit) {
      arrow::write_parquet(bind_rows(all_rows) %>% distinct(uri, .keep_all = TRUE),
                           checkpoint_path)
    }
    cursor <- resp$cursor %||% NULL
    if (is.null(cursor)) {
      oldest <- df$indexedAt[which.min(df$indexedAt)]
      if (!is.na(oldest) && oldest != until) until <- oldest else break
    }
    Sys.sleep(runif(1, 0.5, 1.5))
  }
  bind_rows(all_rows) %>% distinct(uri, .keep_all = TRUE)
}

## Hydration helpers
get_reposts_df <- function(uri) {
  Sys.sleep(runif(1, 0.2, 1.1))
  reposts <- retry(bs_get_reposts(uri, limit = 500, auth = bs_auth, clean = TRUE),
                   when = "error", max_tries = 4, interval = runif(1, 0.8, 1.8))
  if (is.null(reposts) || !inherits(reposts, "data.frame") || nrow(reposts) == 0)
    return(tibble(original_uri = uri, handle = character(), uri = character()))
  reposts$original_uri <- uri; as_tibble(reposts)
}

get_thread_df <- function(uri) {
  Sys.sleep(runif(1, 0.2, 0.7))
  thread <- retry(bs_get_post_thread(uri, auth = bs_auth, clean = FALSE),
                  when = "error", max_tries = 4, interval = runif(1, 0.8, 1.8))
  if (is.null(thread) || length(thread) == 0)
    return(tibble(original_uri = uri, author = character(), text = character(), uri = character()))
  tibble(
    original_uri = uri,
    author = map_chr(thread, ~ safe_chr(.x, "post", "author", "handle")),
    text   = map_chr(thread, ~ safe_chr(.x, "post", "record", "text")),
    uri    = map_chr(thread, ~ safe_chr(.x, "post", "uri"))
  )
}

## Batch hydration
hydrate_in_batches <- function(posts_df, batch_size = 400, tag = "query") {
  wrkrs <- max(1, floor(availableCores(constraints = "connections-16") * 0.3))
  plan(multisession, workers = wrkrs)
  uris_reposts <- posts_df %>% filter(repost_count > 0) %>% pull(uri) %>% unique()
  uris_threads <- posts_df %>% filter(reply_count > 0) %>% pull(uri) %>% unique()
  rep_chunks <- split(uris_reposts, ceiling(seq_along(uris_reposts)/batch_size))
  thr_chunks <- split(uris_threads, ceiling(seq_along(uris_threads)/batch_size))
  all_reposts <- map_dfr(rep_chunks, ~ future_map_dfr(.x, get_reposts_df, .progress = TRUE))
  all_threads <- map_dfr(thr_chunks, ~ future_map_dfr(.x, get_thread_df, .progress = TRUE))
  plan(sequential)
  list(reposts_df = distinct(all_reposts, original_uri, handle, uri, .keep_all = TRUE),
       threads_df = distinct(all_threads, original_uri, author, uri, .keep_all = TRUE))
}

## Run search + hydration
posts_df <- deep_search_posts("Speirgorm", hard_limit = 50000, chunk_limit = 100,
                              checkpoint_path = "speirgorm_posts.parquet")
hydrated <- hydrate_in_batches(posts_df, batch_size = 400, tag = "speirgorm")
reposts_df <- hydrated$reposts_df; threads_df <- hydrated$threads_df

## Build edges/nodes
edges <- reposts_df %>%
  left_join(posts_df %>% select(uri, author_handle), by = c("original_uri" = "uri")) %>%
  transmute(from = handle, to = author_handle, repost_uri = uri) %>%
  filter(!is.na(from), !is.na(to)) %>% distinct()

nodes <- bind_rows(
  reposts_df %>% select(name = handle, display_name, avatar, did),
  posts_df %>% select(name = author_handle, text)
) %>% distinct(name, .keep_all = TRUE) %>%
  mutate(repost_count = replace_na(as.integer(table(edges$to)[name]), 0L))

## Export
write_csv(posts_df, "speirgorm_posts.csv")
write_csv(reposts_df, "speirgorm_reposts.csv")
write_csv(threads_df, "speirgorm_threads.csv")
write_csv(edges, "speirgorm_edges.csv")
write_csv(nodes, "speirgorm_nodes.csv")

## Graph construction and visualization (your existing ggraph/visNetwork/Gephi code continues here)



