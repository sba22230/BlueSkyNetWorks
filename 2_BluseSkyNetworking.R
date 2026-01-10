plan(multisession, workers = wrkrs)
source("0_functions.R")

# Step 1: Build edge list (who reposted whom)
edges <- read_parquet("graphs/speirgorm_edges.parquet")
cat("Edges count:", nrow(edges), "\n")

# Debug: inspect edges
print(head(edges))

# Step 2: Build node list (unique actors and posts)
nodes <- read_parquet("graphs/speirgorm_nodes.parquet")
cat("Nodes count:", nrow(nodes), "\n")

# Debug: inspect nodes
print(head(nodes))

# Step 3: Build igraph object and plot basic network
g1 <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
cat("Graph summary:\n")
print(summary(g1))

coords <- layout_with_drl(g1) # heavy step, do once
V(g1)$x <- coords[, 1]
V(g1)$y <- coords[, 2]

# Select top N nodes to label (e.g., top 50 by degree)
deg <- igraph::degree(g1)
top_nodes <- names(sort(deg, decreasing = TRUE))[1:50]

# Faster ggraph call
gtn <- ggraph(g1, layout = "manual", x = V(g1)$x, y = V(g1)$y) +
  geom_edge_link(alpha = 0.3) +
  geom_node_point(size = 5) +
  geom_node_text(
    aes(label = ifelse(name %in% top_nodes, name, "")),
    repel = TRUE,
    max.overlaps = 1000
  )

save_graph_svg(gtn, "TopNodes_Speirgorm_Network.svg")
# Step 4: Enrich edges with author info
# this step is unnecessary - the edges already have author info - need to add created_at
# edges <- reposts_df |>
#   left_join(
#     posts_df |> select(uri, author_handle),
#     by = c("original_uri" = "uri")
#   ) |>
#   transmute(
#     from = handle,
#     to = author_handle,
#     repost_uri = uri,
#     created_at = created_at
#   ) |>
#   filter(!is.na(from) & !is.na(to)) |>
#   distinct()
# cat("Enriched edges count:", nrow(edges), "\n")

# Debug: enriched edges
print(head(edges))

# Step 11: Enrich nodes with metadata
# This doesn't need to be done either
# nodes <- bind_rows(
#  reposts_df |> select(name = handle, display_name, did),
#  posts_df |> select(name = author_handle, text, like_count, repost_count)
# ) |>
#  distinct(name, .keep_all = TRUE)

# Add repost counts
# nodes <- nodes |>
#   mutate(repost_count = table(edges$to)[name] |> as.integer())
# cat("Enriched nodes count:", nrow(nodes), "\n")

# Debug: enriched nodes
print(head(nodes))

# Step 5: Plot enriched network with ggraph
g2 <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
coords2 <- layout_with_graphopt(g2, niter = 20) # heavy step, do once
V(g2)$x <- coords2[, 1]
V(g2)$y <- coords2[, 2]
ggraph(g2, layout = "manual", x = V(g2)$x, y = V(g2)$y) +
  geom_edge_link(alpha = 0.3) +
  geom_node_point(aes(size = reposts_made, color = reposts_received)) +
  geom_node_text(aes(label = name), repel = TRUE) +
  scale_size_continuous(range = c(3, 12)) +
  scale_color_gradient(low = "lightblue", high = "red") +
  theme_void()
cat("Final graph summary:\n")
print(summary(g2))
write_graph(
  g2,
  "graphs/bluesky Speirgorm Network RepostsMade vs RepostsReceived.graphml",
  format = "graphml"
)

# Step 6: Interactive visualization with visNetwork
vis_nodes <- nodes |>
  mutate(
    id = name,
    label = name,
    title = paste0("Name: ", name, "\nTotal Likes: ", total_likes_on_posts), # hover tooltip
    value = ifelse(is.na(reposts_received), 0, reposts_received),
    # size by repost_count # nolint: line_length_linter.
    group = name
  ) |>
  distinct(id, .keep_all = TRUE)

vis_edges <- edges |>
  transmute(
    from = from,
    to = to,
    arrows = "from" # add arrow pointing to the reposter node
  )

# Build igraph object from edges
g3 <- graph_from_data_frame(vis_edges, vertices = vis_nodes, directed = TRUE)

# Compute degree for each node
deg <- igraph::degree(g3, mode = "all")

# Add degree to vis_nodes
vis_nodes <- vis_nodes |>
  mutate(degree = deg[id])

# Sort IDs by degree (descending)
sorted_ids <- vis_nodes |>
  arrange(desc(degree)) |>
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
offset <- 0

for (comp_id in unique(comps$membership)) {
  sub_nodes <- which(comps$membership == comp_id)
  sub_coords <- layouts[[which(unique(comps$membership) == comp_id)]]

  # Apply offset so components don’t overlap
  coords3[sub_nodes, ] <- sub_coords + offset
  offset <- offset + max(sub_coords[, 1]) + 10 # shift horizontally
}
vis_nodes$x <- coords3[, 1]
vis_nodes$y <- coords3[, 2]

top_nodes <- vis_nodes |> arrange(desc(degree)) |> slice(1:50) |> pull(id)
vis_nodes$label <- ifelse(vis_nodes$id %in% top_nodes, vis_nodes$label, NA)

# Community detection
comm <- cluster_walktrap(g3) # works on directed graphs

# Add community membership to nodes
vis_nodes$community <- comm$membership

# ensure community is used as the visNetwork group and add colours
vis_nodes$group <- as.character(vis_nodes$community)
vis_nodes$isolated <- vis_nodes$degree == 0

# assign a distinct colour per community
comm_levels <- sort(unique(vis_nodes$group))
pal <- grDevices::rainbow(length(comm_levels))
group_cols <- setNames(pal, comm_levels)
vis_nodes$color.background <- group_cols[vis_nodes$group]
vis_nodes$color.border <- "black"

# optional: shrink labels for non-top nodes (already computed earlier)
vis_nodes$label <- ifelse(vis_nodes$id %in% top_nodes, vis_nodes$label, NA)


# --- Use precomputed layout in visNetwork ---
vis_obj <- visNetwork(
  vis_nodes,
  vis_edges,
  width = "1440px",
  height = "800px"
) |>
  visNodes(fixed = TRUE) |>
  visOptions(
    highlightNearest = TRUE,
    nodesIdSelection = list(values = sorted_ids)
  ) |>
  visEdges(arrows = "to", smooth = FALSE) |>
  visLegend(useGroups = TRUE, position = "right") |>
  visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE) |>
  visPhysics(enabled = FALSE)
# save HTML and capture as SVG (requires webshot2 and
# a headless Chrome/Chromium)
htmlwidgets::saveWidget(
  vis_obj,
  "graphs/visnetwork_tmp.html",
  selfcontained = TRUE
)

## Export the Visnetwork into GEXF with visual encodings preserved
# Nodes table (basic)
ge_nodes <- data.frame(
  id = vis_nodes$id,
  label = ifelse(is.na(vis_nodes$label), vis_nodes$id, vis_nodes$label),
  x = vis_nodes$x,
  y = vis_nodes$y
)

# Node attributes (metrics + display flags)
ge_nodesAtt <- vis_nodes |>
  transmute(
    degree = ifelse(is.na(degree), 0L, as.integer(degree)),
    repost_count = ifelse(
      is.na(reposts_received),
      0L,
      as.integer(reposts_received)
    ),
    community = as.integer(community),
    isolated = ifelse(isolated, "TRUE", "FALSE"),
    label_shown = !is.na(label),
    value = ifelse(is.na(value), 1, as.numeric(value))
  )

# Normalize node colors (supporting hex or NA)
hex_cols <- ifelse(
  is.na(vis_nodes$color.background),
  "#878787",
  vis_nodes$color.background
)
node_rgba_df <- parse_color_to_rgba(hex_cols)
nodesVizAtt$color <- data.frame(
  r = as.integer(node_rgba_df$r),
  g = as.integer(node_rgba_df$g),
  b = as.integer(node_rgba_df$b),
  a = as.numeric(node_rgba_df$a)
)
# Nodes viz attributes for GEXF
nodesVizAtt <- list(
  position = data.frame(
    x = vis_nodes$x,
    y = vis_nodes$y,
    z = rep(0, nrow(vis_nodes))
  ),
  size = as.numeric(ifelse(
    is.na(vis_nodes$value),
    ge_nodesAtt$value,
    vis_nodes$value
  )),
  color = data.frame(
    r = as.integer(rgb_mat[, 1]),
    g = as.integer(rgb_mat[, 2]),
    b = as.integer(rgb_mat[, 3]),
    a = node_alpha
  )
)

# Edges table & attributes (carry width/weight and color)
ge_edges <- data.frame(source = vis_edges$from, target = vis_edges$to)

ge_edgesAtt <- vis_edges |>
  mutate(
    weight = ifelse(is.na(width), 1, as.numeric(width)),
    color_hex = ifelse(is.na(color), "#AAAAAA", color)
  ) |>
  select(-from, -to) |>
  mutate(across(
    where(is.character),
    ~ stri_replace_all_regex(., "[&<>]", " ")
  )) |>
  mutate(across(where(is.character), ~ stri_trans_general(., "Latin-ASCII"))) |>
  mutate(color_hex = ifelse(is.na(color_hex), "#AAAAAA", color_hex))

# Convert edge hex colors to RGB columns expected by GEXF writer
edge_hex <- ge_edgesAtt$color_hex
# replace the failing conversion with:
edge_rgba_df <- parse_color_to_rgba(ifelse(
  is.na(ge_edgesAtt$color_hex),
  "#AAAAAA",
  ge_edgesAtt$color_hex
))
edge_color_df <- data.frame(
  r = as.integer(edge_rgba_df$r),
  g = as.integer(edge_rgba_df$g),
  b = as.integer(edge_rgba_df$b),
  a = as.numeric(edge_rgba_df$a)
)

# Use write.gexf, passing nodes, edges, attributes and viz attributes
gexf_obj <- write.gexf(
  nodes = ge_nodes[, c("id", "label")],
  edges = ge_edges,
  nodesAtt = ge_nodesAtt,
  edgesAtt = ge_edgesAtt %>% select(-color_hex),
  nodesVizAtt = nodesVizAtt,
  edgesVizAtt = list(
    color = edge_color_df,
    thickness = as.numeric(ge_edgesAtt$weight)
  ),
  defaultedgetype = "directed"
)

# Save to file
home_dir <- here::here()
file_path <- file.path(home_dir, "graphs")
file_name <- file.path(file_path, "visnetwork_export.gexf")
if (!dir.exists(file_path)) {
  dir.create(file_path, recursive = TRUE)
}
rgexf::write.gexf(gexf_obj, output = file_name)

plot(gexf_obj)

cat("Interactive visualization ready with precomputed layout.\n")
top_authors_reposted <- posts_df |>
  group_by(author_handle) |>
  summarise(total_reposts = sum(repost_count, na.rm = TRUE)) |>
  arrange(desc(total_reposts)) |>
  slice(1:10)
top_reposters <- reposts_df |>
  group_by(handle, display_name) |> # group by both
  summarise(total_reposts_made = n(), .groups = "drop") |> # count reposts
  arrange(desc(total_reposts_made)) |> # sort descending
  slice(1:10) # top 10

library(DT)

datatable(top_authors_reposted, caption = "Top 10 Authors by Reposts Received")
datatable(top_reposters, caption = "Top 10 Accounts by Reposts Made")

# End of script
