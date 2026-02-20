# source("0_functions.R")
plan(multisession, workers = wrkrs)

# Step 1: Build edge list (who reposted whom)
edges <- read_parquet("graphs/speirgorm_edges.parquet")
cat("Edges count:", nrow(edges), "\n")

# Debug: inspect edges
print(head(edges))

# remove NAs
edges[is.na(edges)] <- "no text here"

na_per_column <- colSums(is.na(edges))
print("Number of NAs per column:")
print(na_per_column)

# Step 2: Build node list (unique actors and posts)
nodes <- read_parquet("graphs/speirgorm_nodes.parquet")
cat("Nodes count:", nrow(nodes), "\n")

# Debug: inspect nodes
print(head(nodes))

na_per_column <- colSums(is.na(nodes))
print("Number of NAs per column:")
print(na_per_column)

# Step 3: Build igraph object and plot basic network
g <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
cat("Graph summary:\n")
print(summary(g))

# Compute degree for each node
deg <- igraph::degree(g3, mode = "all")

# Add degree to vis_nodes
vis_nodes <- vis_nodes %>%
  mutate(degree = deg[id])

# Sort IDs by degree (descending)
sorted_ids <- vis_nodes %>%
  arrange(desc(degree)) %>%
  pull(id)

# --- Precompute layout coordinates with igraph ---
comps <- igraph::components(g3)
layouts <- future_map(
  unique(comps$membership),
  function(comp_id) {
    subg <- induced_subgraph(g3, which(comps$membership == comp_id))
    if (vcount(subg) > 100) layout_with_lgl(subg) else layout_with_kk(subg)
  },
  .progress = TRUE,
  .options = furrr_options(seed = 22230)
)
coords3 <- matrix(NA, nrow = vcount(g3), ncol = 2)
offset <- 3

comp_ids <- sort(unique(comps$membership))

for (i in seq_along(comp_ids)) {
  comp_id <- comp_ids[i]

  sub_nodes <- which(comps$membership == comp_id)
  sub_coords <- layouts[[i]]

  coords3[sub_nodes, ] <- sub_coords + offset
  offset <- offset + diff(range(sub_coords[, 1])) + 10
}
vis_nodes$x <- coords3[, 1]
vis_nodes$y <- coords3[, 2]

top_nodes <- vis_nodes %>% arrange(desc(degree)) %>% slice(1:50) %>% pull(id)
vis_nodes$label <- ifelse(vis_nodes$id %in% top_nodes, vis_nodes$label, NA)

# Community detection
comm <- cluster_walktrap(g3) # works on directed graphs

rxWriteObject(
  ds_Graphs,
  "g3_Graph - igraph - community detection",
  g3,
  overwrite = TRUE
)

# Add community membership to nodes
vis_nodes$community <- comm$membership


# ensure community is used as the visNetwork group and add colours
vis_nodes$group <- as.character(vis_nodes$community)
vis_nodes$isolated <- vis_nodes$degree == 0

# ========================================================================
# COMMUNITY CHARACTERIZATION & LABELING
# ========================================================================
# Analyze what defines each community to create meaningful labels

cat("\n=== ANALYZING COMMUNITIES ===\n")

# Get community member data
community_analysis <- vis_nodes %>%
  group_by(community) %>%
  summarise(
    # Size and structure
    size = n(),
    isolated_count = sum(isolated, na.rm = TRUE),
    active_members = sum(!isolated, na.rm = TRUE),

    # Connectivity metrics
    avg_degree = mean(degree, na.rm = TRUE),
    max_degree = max(degree, na.rm = TRUE),
    median_degree = median(degree, na.rm = TRUE),

    # Engagement metrics (from nodes attributes)
    avg_repost_count = mean(value, na.rm = TRUE),
    max_repost_count = max(value, na.rm = TRUE),

    # Temporal reach (if available)
    earliest_first_seen = min(earliestPost, na.rm = TRUE),
    latest_last_seen = max(latestPost, na.rm = TRUE),

    # Top influencers in this community
    top_member = paste(
      head(name[order(desc(degree))], 3),
      collapse = ", "
    ),

    .groups = "drop"
  ) %>%
  arrange(desc(size))

cat("Community Statistics:\n")
datatable(slice(community_analysis, 1:10))

# Define community types based on characteristics
community_labels <- community_analysis %>%
  mutate(
    # Label logic based on community characteristics
    community_type = case_when(
      # Large, highly connected communities (hubs)
      size >= 50 &
        avg_degree > median(community_analysis$avg_degree) * 1.5 ~ "Core Hub",

      # Medium-sized, moderately connected
      size >= 20 &
        size < 50 &
        avg_degree >= median(community_analysis$avg_degree) ~ "Active Circle",

      # Smaller, tightly knit groups
      size >= 10 &
        size < 20 &
        avg_degree > median(community_analysis$avg_degree) ~ "Tight Cluster",

      # Small but highly engaged (high repost count relative to size)
      size < 10 &
        avg_repost_count >
          quantile(
            community_analysis$avg_repost_count,
            0.75
          ) ~ "Engaged Micro-Group",

      # Isolated or low-engagement nodes
      isolated_count >= active_members * 0.5 ~ "Peripheral",

      # Default: catchall
      TRUE ~ "Discussion Group"
    ),

    # Create descriptive label with name and characteristics
    label = sprintf(
      "Community %d: %s\n(%d members, avg degree: %.1f)",
      community,
      community_type,
      size,
      avg_degree
    )
  ) %>%
  select(community, community_type, label, size, avg_degree, top_member)

cat("\n=== COMMUNITY LABELS & TYPES ===\n")
datatable(slice(community_labels, 1:10))

# Map labels back to vis_nodes
community_label_map <- setNames(
  community_labels$label,
  community_labels$community
)

community_type_map <- setNames(
  community_labels$community_type,
  community_labels$community
)

vis_nodes <- vis_nodes %>%
  mutate(
    community_label = community_label_map[as.character(community)],
    community_type = community_type_map[as.character(community)]
  )

# Print community members for inspection
cat("\n=== COMMUNITY MEMBERSHIP BREAKDOWN ===\n")
for (comm_id in sort(unique(vis_nodes$community))) {
  members <- vis_nodes %>%
    filter(community == comm_id) %>%
    arrange(desc(degree)) %>%
    slice(1:10) %>%
    pull(id)

  comm_type <- unique(vis_nodes$community_type[vis_nodes$community == comm_id])
  cat(sprintf("\nCommunity %d (%s) - Top 10 Members:\n", comm_id, comm_type))
  cat(paste(members, collapse = ", "))
  cat("\n")
}


posts_df <- read_parquet("data/speirgorm_network.parquet")
top_authors_reposted <- posts_df %>%
  group_by(PostedBy) %>%
  summarise(total_reposts = sum(repost_count, na.rm = TRUE)) %>%
  arrange(desc(total_reposts)) %>%
  slice(1:10)
top_reposters <- posts_df %>%
  group_by(RepostedBy) %>% # group by both
  summarise(total_reposts_made = n(), .groups = "drop") %>% # count reposts
  arrange(desc(total_reposts_made)) %>% # sort descending
  slice(1:10) # top 10

library(DT)

datatable(top_authors_reposted, caption = "Top 10 Authors by Reposts Received")
datatable(top_reposters, caption = "Top 10 Accounts by Reposts Made")

# End of script
