# 1_bluesky_ingest.R
source(here::here("0_functions.R"))

# Connect to bluesky
bs_user <- bs_get_user()
bs_pass <- bs_get_pass()
bs_auth <- bs_auth(bs_user, bs_pass, save_auth = TRUE)

plan(multisession, workers = wrkrs)
cat(
  "\n=== Step 1a: Deep search for posts ===\n"
)
# Run deep search
posts_df <- deep_search_posts(
  "Speirgorm",
  hard_limit = 100000,
  chunk_limit = 100,
  checkpoint_path = project_path("data", "speirgorm_posts.parquet")
)


# Load existing reposts/threads or start fresh
cat(
  "\n=== Step 1b: Loading existing reposts and threads ===\n"
)
reposts_df <- tryCatch(
  arrow::read_parquet(project_path("data", "speirgorm_reposts.parquet")),
  error = function(e) {
    tibble(
      original_uri = character(),
      did = character(),
      handle = character(),
      uri = character(),
      repost_created_at = character()
    )
  }
)
threads_df <- tryCatch(
  arrow::read_parquet(project_path("data", "speirgorm_threads.parquet")),
  error = function(e) {
    tibble(
      original_uri = character(),
      author = character(),
      uri = character()
    )
  }
)
validate_schema(reposts_df, "reposts", "reposts_df")
validate_schema(threads_df, "threads", "threads_df")

# Filter to only hydrate new posts
posts_to_hydrate <- posts_df %>%
  filter(!(uri %in% c(reposts_df$original_uri, threads_df$original_uri)))

# Check for partial hydration checkpoints and load if available
hydrated_reposts_checkpoint <- project_path(
  "data",
  "speirgorm_hydrated_reposts.parquet"
)
hydrated_threads_checkpoint <- project_path(
  "data",
  "speirgorm_hydrated_threads.parquet"
)

if (file.exists(hydrated_reposts_checkpoint)) {
  message("Found existing hydrated reposts checkpoint; loading...")
  hydrated_reposts_partial <- arrow::read_parquet(hydrated_reposts_checkpoint)
  posts_to_hydrate <- posts_to_hydrate %>%
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
  posts_to_hydrate <- posts_to_hydrate %>%
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
        did = character(),
        handle = character(),
        uri = character(),
        repost_created_at = character()
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

reposts_df <- bind_rows(reposts_df, hydrated$reposts_df) %>%
  distinct(original_uri, handle, uri, .keep_all = TRUE)
threads_df <- bind_rows(threads_df, hydrated$threads_df) %>%
  distinct(original_uri, author, uri, .keep_all = TRUE)

plan(orig_plan)
cat(
  "\n=== Step 1c: Write out posts, reposts and threads into parquet ===\n"
)
arrow::write_parquet(posts_df, project_path("data", "speirgorm_posts.parquet"))
arrow::write_parquet(
  reposts_df,
  project_path("data", "speirgorm_reposts.parquet")
)
arrow::write_parquet(
  threads_df,
  project_path("data", "speirgorm_threads.parquet")
)

cat("\n=== Step 1d Clean up of temporary objects ===\n")

tobermvd <- c(
  'hydrated',
  'hydrated_reposts_partial',
  'hydrated_threads_partial',
  'posts_to_hydrate'
)
rm(list = tobermvd)
plan(orig_plan)
gc()
