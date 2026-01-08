# Connect to bluesky
bs_user <- bs_get_user()
bs_pass <- bs_get_pass()
bs_auth <- bs_auth(bs_user, bs_pass, save_auth = TRUE)

source("0_functions.R")

wrkrs <- max(1, floor(availableCores(constraints = "connections-16") * 0.3))
plan(multisession, workers = wrkrs)
# Run deep search
posts_df <- deep_search_posts(
  "Speirgorm",
  hard_limit = 100000,
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

arrow::write_parquet(posts_df, "data/speirgorm_posts.parquet")
arrow::write_parquet(reposts_df, "data/speirgorm_reposts.parquet")
arrow::write_parquet(threads_df, "data/speirgorm_threads.parquet")
#readr::write_csv(reposts_df, "data/speirgorm_reposts.csv") # this file is too big for git
readr::write_csv(threads_df, "data/speirgorm_threads.csv")

# Build edges: author -> reposter 
edges <- reposts_df |>
  left_join(
    posts_df |> select(uri, author_handle),
    by = c("original_uri" = "uri")
  ) |>
  transmute(from = author_handle, to = handle, repost_uri = uri ,
                created_at = created_at) |>
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

edges <- edges |>
  mutate(from = normalize_handle(from), to = normalize_handle(to))

# Nodes
nodes <- bind_rows(
  reposts_df |> select(name = handle, display_name, did, uri),
  posts_df   |> select(name = author_handle, text, uri)
) |>
  distinct(name, .keep_all = TRUE)
nodes <- nodes |>
  left_join(
    posts_df |> select(name = author_handle, uri, like_count, repost_count),
    by = c("name", "uri")
  )

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
