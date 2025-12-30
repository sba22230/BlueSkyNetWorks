library(bskyr)
library(dplyr)
library(purrr)
library(tibble)
library(stringr)
library(retry)
library(readr)
library(furrr)
library(tidyr)
library(arrow)
library(igraph)
library(ggraph)

bs_user <- bs_get_user()
bs_pass <- bs_get_pass()
bs_auth <- bs_auth(bs_user, bs_pass, save_auth = TRUE)

safe_chr <- function(x, ...) {
  val <- purrr::pluck(x, ..., .default = NA_character_)
  if (is.null(val)) NA_character_ else as.character(val)
}
safe_int <- function(x, ...) {
  val <- purrr::pluck(x, ..., .default = NA_integer_)
  if (is.null(val)) NA_integer_ else as.integer(val)
}

# Deduplicate by URI
dedup_posts <- function(posts) {
  posts |>
    tibble::as_tibble() |>
    distinct(uri, .keep_all = TRUE)
}

# Single page search with retry
search_page <- function(query, cursor = NULL, limit = 300) {
  retry(
    {
      bs_search_posts(
        query = query,
        limit = limit,
        cursor = cursor,
        clean = FALSE,
        auth = bs_auth
      )
    },
    when = "error",
    max_tries = 5,
    interval = runif(1, 1.0, 2.0)
  )
}

get_posts_from_resp <- function(resp) {
  if (is.list(resp) && !is.null(resp$posts)) {
    resp$posts
  } else if (
    is.list(resp) &&
      length(resp) > 0 &&
      all(vapply(resp, function(x) is.list(x) && !is.null(x$posts), logical(1)))
  ) {
    flatten(map(resp, "posts"))
  } else if (
    is.list(resp) &&
      length(resp) > 0 &&
      all(vapply(resp, function(x) !is.null(x$uri), logical(1)))
  ) {
    resp
  } else {
    list()
  }
}

# Deep pagination with "until" anchoring by last indexedAt
deep_search_posts <- function(
  query,
  hard_limit = 50000,
  chunk_limit = 100,
  checkpoint_path = "data/speirgorm_posts.parquet"
) {
  all_rows <- list()
  cursor <- NULL
  until <- NULL
  fetched <- 0
  iter <- 0
  # ✅ Load existing checkpoint if available
  if (file.exists(checkpoint_path)) {
    checkpoint <- tryCatch(
      arrow::read_parquet(checkpoint_path),
      error = function(e) {
        message("Failed to read checkpoint: ", e$message)
        NULL
      }
    )
    if (!is.null(checkpoint) && nrow(checkpoint) > 0) {
      all_rows <- list(checkpoint)
      fetched <- nrow(checkpoint)
      # anchor forward: only query posts newer than the latest indexedAt
      latest <- max(as.POSIXct(checkpoint$indexedAt, tz = "UTC"), na.rm = TRUE)
      until <- NULL
      since <- format(latest, "%Y-%m-%dT%H:%M:%SZ")
      message("Resuming from checkpoint, last post at: ", since)
      query <- paste0(query, " since:", since)
    }
  }

  while (fetched < hard_limit) {
    iter <- iter + 1
    q <- if (!is.null(until)) paste0(query, " until:", until) else query

    resp <- search_page(q, cursor = cursor, limit = chunk_limit)
    posts <- get_posts_from_resp(resp)

    if (length(posts) == 0) {
      # No posts: attempt to push further back if we have an anchor
      if (!is.null(until)) {
        # Break if repeated emptiness
        message("No more posts for query window: ", q)
        break
      } else {
        message("No posts returned; stopping.")
        break
      }
    }

    df <- tibble(
      uri = map_chr(posts, ~ safe_chr(.x, "uri")),
      indexedAt = map_chr(posts, ~ safe_chr(.x, "indexedAt")),
      author_handle = map_chr(posts, ~ safe_chr(.x, "author", "handle")),
      text = map_chr(posts, ~ safe_chr(.x, "record", "text")),
      like_count = map_int(posts, ~ safe_int(.x, "likeCount")),
      bookmark_count = map_int(posts, ~ safe_int(.x, "bookmarkCount")),
      reply_count = map_int(posts, ~ safe_int(.x, "replyCount")),
      repost_count = map_int(posts, ~ safe_int(.x, "repostCount")),
      reply_parent_uri = map_chr(
        posts,
        ~ safe_chr(.x, "record", "reply", "parent", "uri")
      )
    )

    all_rows <- append(all_rows, list(df))
    fetched <- nrow(bind_rows(all_rows))
    message(sprintf("[Iter %d] Fetched %d total rows", iter, fetched))

    # Persist checkpoint every ~1k rows
    if (fetched %% 1000 < chunk_limit) {
      out <- bind_rows(all_rows) |> distinct(uri, .keep_all = TRUE)
      tryCatch(
        {
          arrow::write_parquet(out, checkpoint_path)
          message("Checkpointed: ", checkpoint_path)
        },
        error = function(e) {
          message("Checkpoint failed: ", e$message)
        }
      )
    }

    # Update cursor and potential "until" anchor
    cursor <- resp$cursor %||% NULL

    if (is.null(cursor)) {
      # If the server ended this window,
      # push the anchor back using the oldest indexedAt
      # Convert indexedAt to POSIXct for proper comparison
      indexedAt_posix <- suppressWarnings(as.POSIXct(
        df$indexedAt,
        format = "%Y-%m-%dT%H:%M:%OSZ",
        tz = "UTC"
      ))
      if (all(is.na(indexedAt_posix))) {
        message("All indexedAt values are NA after conversion; stopping.")
        break
      }
      oldest <- df$indexedAt[which.min(indexedAt_posix)]
      if (!is.na(oldest) && !identical(oldest, until)) {
        until <- oldest
        # reset cursor to start a new anchored window
        cursor <- NULL
        message("Advancing until anchor to: ", until)
      } else {
        message("No cursor and no new anchor; stopping.")
        break
      }
    }

    # Gentle pacing
    Sys.sleep(runif(1, 0.5, 1.5))
  }

  bind_rows(all_rows) |> distinct(uri, .keep_all = TRUE)
}

get_reposts_df <- function(uri) {
  Sys.sleep(runif(1, 0.2, 1.1))
  reposts <- tryCatch(
    {
      retry(
        bs_get_reposts(uri, limit = 500, auth = bs_auth, clean = TRUE),
        when = "error",
        max_tries = 4,
        interval = runif(1, 0.8, 1.8)
      )
    },
    error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("400", msg)) {
        message("Skipping invalid/missing repost: ", uri, " (Bad Request)")
        return(tibble(
          original_uri = uri,
          handle = character(),
          uri = character()
        ))
      } else if (grepl("502", msg)) {
        message("Temporary gateway error for ", uri, "; will retry later.")
        return(tibble(
          original_uri = uri,
          handle = character(),
          uri = character()
        ))
      } else {
        message("Failed to get reposts for ", uri, ": ", msg)
        return(tibble(
          original_uri = uri,
          handle = character(),
          uri = character()
        ))
      }
    }
  )
  if (
    is.null(reposts) || !inherits(reposts, "data.frame") || nrow(reposts) == 0
  ) {
    return(tibble(original_uri = uri, handle = character(), uri = character()))
  }
  reposts$original_uri <- uri
  tibble::as_tibble(reposts)
}

get_thread_df <- function(uri) {
  Sys.sleep(runif(1, 0.2, 0.7))
  thread <- tryCatch(
    {
      retry(
        bs_get_post_thread(uri, auth = bs_auth, clean = FALSE),
        when = "error",
        max_tries = 4,
        interval = runif(1, 0.8, 1.8)
      )
    },
    error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("400", msg)) {
        message("Skipping invalid/missing thread: ", uri, " (Bad Request)")
        return(tibble(
          original_uri = uri,
          author = character(),
          text = character(),
          uri = character()
        ))
      } else if (grepl("502", msg)) {
        message("Temporary gateway error for ", uri, "; will retry later.")
        return(tibble(
          original_uri = uri,
          author = character(),
          text = character(),
          uri = character()
        ))
      } else {
        message("Failed to get thread for ", uri, ": ", msg)
        return(tibble(
          original_uri = uri,
          author = character(),
          text = character(),
          uri = character()
        ))
      }
    }
  )
  if (is.null(thread) || length(thread) == 0) {
    return(tibble(
      original_uri = uri,
      author = character(),
      text = character(),
      uri = character()
    ))
  }
  tibble(
    original_uri = uri,
    author = map_chr(thread, ~ safe_chr(.x, "post", "author", "handle")),
    text = map_chr(thread, ~ safe_chr(.x, "post", "record", "text")),
    uri = map_chr(thread, ~ safe_chr(.x, "post", "uri"))
  )
}

# Chunking helper
chunk_vec <- function(x, size) split(x, ceiling(seq_along(x) / size))

hydrate_in_batches <- function(posts_df, batch_size = 400, tag = "speirgorm") {
  uris_reposts <- posts_df |>
    filter(repost_count > 0) |>
    pull(uri) |>
    unique()
  uris_threads <- posts_df |>
    filter(reply_count > 0) |>
    pull(uri) |>
    unique()

  rep_chunks <- chunk_vec(uris_reposts, batch_size)
  thr_chunks <- chunk_vec(uris_threads, batch_size)

  all_reposts <- list()
  all_threads <- list()

  # Load existing partial results if resuming
  reposts_checkpoint_path <- sprintf("data/%s_hydrated_reposts.parquet", tag)
  threads_checkpoint_path <- sprintf("data/%s_hydrated_threads.parquet", tag)

  if (file.exists(reposts_checkpoint_path)) {
    all_reposts <- list(arrow::read_parquet(reposts_checkpoint_path))
    message("Loaded existing reposts checkpoint: ", reposts_checkpoint_path)
  }
  if (file.exists(threads_checkpoint_path)) {
    all_threads <- list(arrow::read_parquet(threads_checkpoint_path))
    message("Loaded existing threads checkpoint: ", threads_checkpoint_path)
  }

  for (i in seq_along(rep_chunks)) {
    message(sprintf(
      "Hydrating reposts batch %d/%d (%d uris)",
      i,
      length(rep_chunks),
      length(rep_chunks[[i]])
    ))
    part <- future_map_dfr(
      rep_chunks[[i]],
      get_reposts_df,
      .progress = TRUE,
      .options = furrr_options(seed = 22230)
    )
    all_reposts <- append(all_reposts, list(part))

    # Write batch CSV
    tryCatch(
      {
        readr::write_csv(
          part,
          sprintf("data/reposts_batch_%s_%03d.csv", tag, i)
        )
      },
      error = function(e) message("Failed to write reposts batch: ", e$message)
    )

    # Checkpoint combined reposts (for resume capability)
    combined <- bind_rows(all_reposts) |>
      distinct(original_uri, handle, uri, .keep_all = TRUE)
    tryCatch(
      {
        arrow::write_parquet(combined, reposts_checkpoint_path)
      },
      error = function(e) message("Failed to checkpoint reposts: ", e$message)
    )

    Sys.sleep(runif(1, 3, 6))
  }

  for (i in seq_along(thr_chunks)) {
    message(sprintf(
      "Hydrating threads batch %d/%d (%d uris)",
      i,
      length(thr_chunks),
      length(thr_chunks[[i]])
    ))
    part <- future_map_dfr(
      thr_chunks[[i]],
      get_thread_df,
      .progress = TRUE,
      .options = furrr_options(seed = 22230)
    )
    all_threads <- append(all_threads, list(part))

    # Write batch CSV
    tryCatch(
      {
        readr::write_csv(
          part,
          sprintf("data/threads_batch_%s_%03d.csv", tag, i)
        )
      },
      error = function(e) message("Failed to write threads batch: ", e$message)
    )

    # Checkpoint combined threads (for resume capability)
    combined <- bind_rows(all_threads) |>
      distinct(original_uri, author, uri, .keep_all = TRUE)
    tryCatch(
      {
        arrow::write_parquet(combined, threads_checkpoint_path)
      },
      error = function(e) message("Failed to checkpoint threads: ", e$message)
    )

    Sys.sleep(runif(1, 3, 6))
  }

  plan(sequential)

  reposts_df <- bind_rows(all_reposts) |>
    distinct(original_uri, handle, uri, .keep_all = TRUE)
  threads_df <- bind_rows(all_threads) |>
    distinct(original_uri, author, uri, .keep_all = TRUE)

  list(reposts_df = reposts_df, threads_df = threads_df)
}

wrkrs <- max(1, floor(availableCores(constraints = "connections-16") * 0.3))
plan(multisession, workers = wrkrs)
# Run deep search
posts_df <- deep_search_posts(
  "Speirgorm",
  hard_limit = 50000,
  chunk_limit = 100,
  checkpoint_path = "data/speirgorm_posts.parquet"
)


readr::write_csv(posts_df, "data/speirgorm_posts.csv")

# Load existing reposts/threads or start fresh
reposts_df <- tryCatch(
  arrow::read_parquet("data/speirgorm_reposts.parquet"),
  error = function(e) tibble(original_uri = character())
)
threads_df <- tryCatch(
  arrow::read_parquet("data/speirgorm_threads.parquet"),
  error = function(e) tibble(original_uri = character())
)

# Filter to only hydrate new posts
posts_to_hydrate <- posts_df |>
  filter(!(uri %in% c(reposts_df$original_uri, threads_df$original_uri)))

# Check for partial hydration checkpoints and load if available
hydrated_reposts_checkpoint <- "data/speirgorm_hydrated_reposts.parquet"
hydrated_threads_checkpoint <- "data/speirgorm_hydrated_threads.parquet"

if (file.exists(hydrated_reposts_checkpoint)) {
  message("Found existing hydrated reposts checkpoint; loading...")
  hydrated_reposts_partial <- arrow::read_parquet(hydrated_reposts_checkpoint)
  posts_to_hydrate <- posts_to_hydrate |>
    filter(!(uri %in% hydrated_reposts_partial$original_uri))
  message(
    "Filtered to ",
    nrow(posts_to_hydrate),
    " remaining posts to hydrate (reposts)"
  )
}

if (file.exists(hydrated_threads_checkpoint)) {
  message("Found existing hydrated threads checkpoint; loading...")
  hydrated_threads_partial <- arrow::read_parquet(hydrated_threads_checkpoint)
  posts_to_hydrate <- posts_to_hydrate |>
    filter(!(uri %in% hydrated_threads_partial$original_uri))
  message(
    "Filtered to ",
    nrow(posts_to_hydrate),
    " remaining posts to hydrate (threads)"
  )
}

# Hydrate in batches with error handling
hydrated <- tryCatch(
  {
    hydrate_in_batches(
      posts_to_hydrate,
      batch_size = 500,
      tag = "speirgorm"
    )
  },
  error = function(e) {
    message("Hydration failed with error: ", e$message)
    message("Continuing with existing reposts/threads data and checkpoints.")
    list(
      reposts_df = tibble(
        original_uri = character(),
        handle = character(),
        uri = character()
      ),
      threads_df = tibble(
        original_uri = character(),
        author = character(),
        uri = character(),
        text = character()
      )
    )
  }
)

reposts_df <- bind_rows(reposts_df, hydrated$reposts_df) |>
  distinct(original_uri, handle, uri, .keep_all = TRUE)
threads_df <- bind_rows(threads_df, hydrated$threads_df) |>
  distinct(original_uri, author, uri, .keep_all = TRUE)

arrow::write_parquet(reposts_df, "data/speirgorm_reposts.parquet")
arrow::write_parquet(threads_df, "data/speirgorm_threads.parquet")
readr::write_csv(reposts_df, "data/speirgorm_reposts.csv")
readr::write_csv(threads_df, "data/speirgorm_threads.csv")

# Build edges: author -> reposter (original)
edges <- reposts_df |>
  left_join(
    posts_df |> select(uri, author_handle),
    by = c("original_uri" = "uri")
  ) |>
  transmute(from = author_handle, to = handle, repost_uri = uri) |>
  filter(!is.na(from), !is.na(to)) |>
  distinct()

# Minimal DID/handle normalization if available
did_map <- reposts_df |>
  select(handle, did) |>
  filter(!is.na(handle), !is.na(did)) |>
  distinct() |>
  group_by(did) |>
  slice_tail(n = 1) |> # prefer the latest handle seen for DID
  ungroup()

# used to cleand up handles in edges
normalize_handle <- function(h) {
  if (length(h) > 1) {
    # Vectorized handling: replace NA values with NA_character_ safely
    h[is.na(h)] <- NA_character_
    return(h)
  }
  if (is.na(h)) {
    return(NA_character_)
  }
  h
}

# Nodes - we are building a combined nodes dataframe
post_nodes <- posts_df |>
  mutate(
    node_id = paste(author_handle, uri, sep = "::"),
    type = "post"
  ) |>
  select(
    node_id,
    type,
    author_handle,
    uri,
    text,
    like_count,
    repost_count
  )
# user nodes from reposts
user_nodes <- reposts_df |>
  transmute(
    node_id = normalize_handle(handle),
    type = "user",
    handle = normalize_handle(handle),
    did,
    display_name
  ) |>
  distinct(node_id, .keep_all = TRUE)
nodes <- bind_rows(post_nodes, user_nodes)

# Edges are now built
edges <- reposts_df |>
  left_join(
    posts_df |> select(uri, author_handle),
    by = c("original_uri" = "uri")
  ) |>
  mutate(
    from = paste(author_handle, original_uri, sep = "::"),
    to = normalize_handle(handle)
  ) |>
  select(from, to) |>
  distinct()


readr::write_csv(edges, "graphs/speirgorm_edges.csv")
readr::write_csv(nodes, "graphs/speirgorm_nodes.csv")

# Step 12: Plot enriched network with ggraph
g <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
ggraph::ggraph(g, layout = "drl") +
  geom_edge_link(alpha = 0.3) +
  geom_node_point(aes(size = like_count, color = repost_count)) +
  geom_node_text(aes(label = display_name), repel = TRUE) +
  scale_size_continuous(range = c(3, 12)) +
  scale_color_gradient(low = "lightblue", high = "red") +
  theme_void()
cat("Final graph summary:\n")
print(summary(g))
write_graph(
  g,
  "graphs/bluesky enriched Speirgorm Network.graphml",
  format = "graphml"
)
