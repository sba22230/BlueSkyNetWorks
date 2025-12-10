library(dplyr)
library(lubridate)
library(intergraph)
library(igraph)
library(stringr)
library(stringi)
library(tidyr)
library(tibble)
library(ggraph)
library(statnet)

# Step 1: Load data
posts_df <- read.csv("data/speirgorm_posts.csv")
reposts_df <- read.csv("data/speirgorm_reposts.csv")


# --- Optional: sample reposted posts and filter reposts_df -----------------
# Filter posts that have been reposted, sample them reproducibly, then
# keep only repost records for those sampled original posts. Adjust
# `sample_n_posts` as desired (or set to NULL to keep all reposts).
set.seed(22230)
sample_n_posts <- 100
# change to desired sample size; set NULL to skip sampling# Posts that have one or more reposts according to metadata
reposted_posts <- posts_df %>% filter(repost_count > 0)

if (!is.null(sample_n_posts) && nrow(reposted_posts) > 0) {
  sampled_posts <- reposted_posts %>%
    slice_sample(n = min(sample_n_posts, nrow(reposted_posts)))
} else {
  sampled_posts <- reposted_posts
}

sampled_uris <- sampled_posts$uri

# Filter reposts_df to only include reposts of the sampled original posts
reposts_df_sampled <- reposts_df %>%
  filter(original_uri %in% sampled_uris)
posts_df_org <- posts_df
posts_df <- sampled_posts
reposts_df_org <- reposts_df
reposts_df <- reposts_df_sampled

# Step 2: Build edge list (who reposted whom)
edges <- reposts_df |>
  transmute(from = handle, to = original_uri) |>
  distinct()
cat("Edges count:", nrow(edges), "\n")

# Step 3: Build node list (unique actors and posts)
nodes <- tibble(name = unique(c(edges$from, edges$to)))
cat("Nodes count:", nrow(nodes), "\n")

# Step 4: Enrich edges with author info
edges <- reposts_df |>
  left_join(
    posts_df |> select(uri, author_handle),
    by = c("original_uri" = "uri")
  ) |>
  transmute(
    from = handle,
    to = author_handle,
    repost_uri = uri,
    created_at = created_at
  ) |>
  filter(!is.na(from) & !is.na(to)) |>
  distinct()
cat("Enriched edges count:", nrow(edges), "\n")

# Step 5: Enrich nodes with metadata
nodes <- bind_rows(
  reposts_df |> select(name = handle, display_name, avatar, did),
  posts_df |> select(name = author_handle, text)
) |>
  distinct(name, .keep_all = TRUE)

# Add repost counts
nodes <- nodes |>
  mutate(repost_count = table(edges$to)[name] |> as.integer())
cat("Enriched nodes count:", nrow(nodes), "\n")

# Step 6: Plot enriched network with ggraph
g2 <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
coords2 <- layout_with_drl(g2) # heavy step, do once
V(g2)$x <- coords2[, 1]
V(g2)$y <- coords2[, 2]
ggraph(g2, layout = "manual") +
  geom_edge_link(alpha = 0.3) +
  geom_node_point(aes(size = repost_count, color = repost_count)) +
  geom_node_text(aes(label = display_name), repel = TRUE) +
  scale_size_continuous(range = c(3, 12)) +
  scale_color_gradient(low = "lightblue", high = "red") +
  theme_void()
cat("Final graph summary:\n")
print(summary(g2))

# Start of Statnet analysis
# Convert igraph to statnet
library(network)
bluSkynet <- asNetwork(g2)
