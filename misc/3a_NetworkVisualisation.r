# source("3_StatnetAnalysis.R")

g2 <- rxReadObject(
  ds_Graphs,
  "g4_sampled_Graph - igraph - no posts: num_posts 5000"
)

# Network is already created in 3_StatnetAnalysis.R as 'g4_sample'
# --- this code bloc needs to go into the vis file
library(tictoc)
tic()
res <- rxExec(
  layout_exec,
  edges_data = edges,
  nodes_data = nodes,
  layout_type = rxElemArg(c(
    "drl",
    "drl_fast",
    "graphopt",
    "lgl",
    "kk",
    "tree"
  )),
  execObjects = c("connStr", "layout_exec")
)
toc()

# res is a list of 6 data.frames in the same order as layout_type
coords_drl <- res[[1]]
coords_drl_fast <- res[[2]]
coords_graphopt <- res[[3]]
coords_lgl <- res[[4]]
coords_kk <- res[[5]]
coords_tree <- res[[6]]

coords <- coords_tree # heavy step, do once
V(g)$x <- coords[, 1]
V(g)$y <- coords[, 2]

# Select top N nodes to label (e.g., top 50 by degree)
deg <- igraph::degree(g)
top_nodes <- names(sort(deg, decreasing = TRUE))[1:50]

# Faster ggraph call
gtn <- ggraph(g, layout = "manual", x = V(g)$x, y = V(g)$y) +
  geom_edge_link(alpha = 0.3) +
  geom_node_point(size = 5) +
  geom_node_text(
    aes(label = ifelse(name %in% top_nodes, name, "")),
    repel = TRUE,
    max.overlaps = 1000
  )

save_graph_svg(gtn, "gtn_TopNodes_Speirgorm_Network.svg")
rxWriteObject(
  ds_Graphs,
  "gtn_Graph - ggraph - layout_with_drl",
  gtn,
  overwrite = TRUE
)

# --- end of one visualisation

# Step 5: Plot enriched network with ggraph
g2 <- g
coords2 <- layout_with_graphopt(g2, niter = 400) # heavy step, do once
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
  "Report/_site/graphs/g2 bluesky Speirgorm Network RepostsMade vs RepostsReceived.graphml",
  format = "graphml"
)
rxWriteObject(
  ds_Graphs,
  "g2_Graph - igraph - layout_with_graphopt",
  g2,
  overwrite
)

# Step 6: Interactive visualization with visNetwork
vis_nodes <- nodes %>%
  mutate(
    id = name,
    label = name,
    # hover tooltip
    title = paste0("Name: ", name, "\nTotal Likes: ", total_likes_on_posts),
    value = ifelse(is.na(reposts_received), 0, reposts_received),
    # size by repost_count # nolint: line_length_linter.
    group = name
  ) %>%
  distinct(id, .keep_all = TRUE)

vis_edges <- edges %>%
  mutate(arrows = "from")
# add arrow pointing to the reposter node

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
  width = "1600px",
  height = "1200px"
) %>%
  visNodes(fixed = TRUE) %>%
  visOptions(
    highlightNearest = TRUE,
    nodesIdSelection = list(values = sorted_ids)
  ) %>%
  visEdges(arrows = "to", smooth = FALSE) %>%
  visLegend(useGroups = TRUE, position = "right") %>%
  visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE) %>%
  visPhysics(enabled = FALSE)
# save HTML and capture as SVG (requires webshot2 and
# a headless Chrome/Chromium)
htmlwidgets::saveWidget(
  vis_obj,
  "Report/_site/graphs/visnetwork_tmp.html",
  selfcontained = TRUE
)
rxWriteObject(
  ds_Graphs,
  "g3_Visnetwork - visNetwork - layout_with_lgl",
  vis_obj
)
## Export the Visnetwork into GEXF with visual encodings preserved
# Nodes (basic)
ge_nodes <- data.frame(
  id = vis_nodes$id,
  label = ifelse(is.na(vis_nodes$label), vis_nodes$id, vis_nodes$label),
  x = vis_nodes$x,
  y = vis_nodes$y,
  stringsAsFactors = FALSE
)

# Node attributes (metrics + flags) — preserve your existing metrics
ge_nodesAtt <- vis_nodes %>%
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

ge_nodesAtt$modularity_class <- as.integer(vis_nodes$community)
# Node visual attributes for GEXF
node_hex <- ifelse(
  is.na(vis_nodes$color.background),
  "#878787",
  vis_nodes$color.background
)
# parallel parse nodes
plan(multisession, workers = wrkrs)
node_rgba_df <- furrr::future_map_dfr(
  node_hex,
  function(cl) {
    v <- parse_color_single(cl)
    tibble::tibble(r = v["r"], g = v["g"], b = v["b"], a = v["a"])
  },
  .progress = TRUE,
  .options = furrr::furrr_options(seed = 1234)
)
ge_nodesVizAtt <- list(
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
    r = as.integer(node_rgba_df$r),
    g = as.integer(node_rgba_df$g),
    b = as.integer(node_rgba_df$b),
    a = as.numeric(node_rgba_df$a)
  )
)
plan(orig_plan)
# Edges and attributes (preserve width/weight and color)
ge_edges <- data.frame(
  source = vis_edges$from,
  target = vis_edges$to,
  stringsAsFactors = FALSE
)
# Safe edge attributes (handles missing width/weight/color)
if (any(c("width", "weight", "color") %in% names(vis_edges))) {
  ge_edgesAtt <- vis_edges %>%
    mutate(
      weight = if ("width" %in% names(.)) {
        ifelse(is.na(width), 1, as.numeric(width))
      } else if ("weight" %in% names(.)) {
        ifelse(is.na(weight), 1, as.numeric(weight))
      } else {
        1
      },
      color_raw = if ("color" %in% names(.)) {
        ifelse(is.na(color) | color == "", "#AAAAAA", color)
      } else {
        "#AAAAAA"
      },
      # NEW: keep dynamic info as attributes
      edge_start = as.character(edgeStarts),
      edge_end = as.character(edgeEnds)
    ) %>%
    select(-from, -to, -any_of(c("width", "weight", "color")))
} else {
  ge_edgesAtt <- vis_edges %>%
    mutate(
      weight = 1,
      color_raw = "#AAAAAA",
      edge_start = as.character(edgeStarts),
      edge_end = as.character(edgeEnds)
    ) %>%
    select(-from, -to)
}

# Build igraph object from edges
g3 <- graph_from_data_frame(vis_edges, vertices = vis_nodes, directed = TRUE)

# safe color parsing for edges
# parallel parse edges
plan(multisession, workers = wrkrs)
edge_rgba_df <- furrr::future_map_dfr(
  ge_edgesAtt$color_raw,
  function(cl) {
    v <- parse_color_single(cl)
    tibble::tibble(r = v["r"], g = v["g"], b = v["b"], a = v["a"])
  },
  .progress = TRUE,
  .options = furrr::furrr_options(seed = 1234)
)
ge_edgesVizColor <- data.frame(
  r = as.integer(edge_rgba_df$r),
  g = as.integer(edge_rgba_df$g),
  b = as.integer(edge_rgba_df$b),
  a = as.numeric(edge_rgba_df$a)
)

ge_nodes$id <- sanitize_xml(as.character(ge_nodes$id))
ge_nodes$label <- sanitize_xml(as.character(ge_nodes$label))
char_cols <- sapply(ge_nodesAtt, is.character)
ge_nodesAtt[char_cols] <- lapply(ge_nodesAtt[char_cols], sanitize_xml)
char_cols_e <- sapply(ge_edgesAtt, is.character)
ge_edgesAtt[char_cols_e] <- lapply(ge_edgesAtt[char_cols_e], sanitize_xml)

# Use write.gexf, passing nodes, edges, attributes and viz attributes
gexf_obj <- write.gexf(
  nodes = ge_nodes[, c("id", "label")],
  edges = ge_edges,
  nodesAtt = ge_nodesAtt,
  edgesAtt = ge_edgesAtt %>% select(-color_raw),
  nodesVizAtt = ge_nodesVizAtt,
  edgesVizAtt = list(
    color = ge_edgesVizColor,
    size = as.numeric(ge_edgesAtt$weight)
  ),
  defaultedgetype = "directed"
)

plan(orig_plan)
# Save to file
home_dir <- here::here()
file_path <- file.path(home_dir, "graphs")
file_name <- file.path(file_path, "g3 visnetwork_export.gexf")
if (!dir.exists(file_path)) {
  dir.create(file_path, recursive = TRUE)
}
rgexf::write.gexf(gexf_obj, output = file_name)

plot(gexf_obj)
rxWriteObject(
  ds_Graphs,
  "Gephi_Graph - Gephi - GEXF",
  gexf_obj,
  overwrite = TRUE
)

cat("Interactive visualization ready with precomputed layout.\n")


coords <- layout_with_drl(g2)
V(g2)$x <- coords[, 1]
V(g2)$y <- coords[, 2]

ggraph(g2, layout = "manual", x = V(g2)$x, y = V(g2)$y) +
  geom_edge_parallel0(
    aes(width = repost_count, colour = like_count),
    edge_alpha = 1,
    arrow = arrow(
      angle = 30,
      length = unit(0.15, "inches"),
      ends = "last",
      type = "closed"
    )
  ) +
  scale_edge_colour_gradient(low = "#87CEFF", high = "#27408B") +
  scale_edge_width(range = c(0.3, 1.2)) +
  geom_node_point(
    aes(fill = posts_authored, size = reposts_received),
    colour = "#000000",
    shape = 21,
    stroke = 0.3
  ) +
  scale_fill_gradient(low = "#87CEFF", high = "#27408B") +
  scale_size(range = c(3, 8)) +
  theme_graph() +
  theme(legend.position = "top")

coords2 <- layout_with_drl(
  g2,
  use.seed = FALSE,
  seed = matrix(runif(vcount(g2) * 2), ncol = 2),
  options = list(init.iterations = n_iter)
) # heavy step, do once
V(g2)$x <- coords2[, 1]
V(g2)$y <- coords2[, 2]
g3 <- ggraph(g2, layout = "manual", x = V(g2)$x, y = V(g2)$y) +
  geom_edge_link(alpha = 0.3) +
  geom_node_point(aes(size = total_likes_on_posts, color = reposts_received)) +
  geom_node_text(aes(label = name), repel = TRUE) +
  scale_size_continuous(range = c(3, 12)) +
  scale_color_gradient(low = "lightblue", high = "red") +
  theme_graph()

plot(g3)
save_graph_svg(g3, "g3_with_drl.svg")

save_graph_svg(
  plot_or_expr = function() {
    sna::gplot(bluSkynet)
  },
  filename = "1_bluSkynet_sna.svg"
)

save_graph_svg(
  plot_or_expr = function() {
    sna::gplot(
      bluSkynet,
      mode = "fruchtermanreingold",
      layout.par = list(niter = n_iter)
    ) # very slow
  },
  filename = "2_bluSkynet_sna.svg"
)

save_graph_svg(
  plot_or_expr = function() {
    plot(bluSkynet, displaylabels = FALSE)
  },
  filename = "3_bluSkynet_sna.svg"
)

save_graph_svg(
  plot_or_expr = function() {
    gplot(bluSkynet, label.cex - 0.2, label.col = "blue", displaylabels = FALSE)
    # graph is very messy with labels
  },
  filename = "4_bluSkynet_sna.svg"
)

save_graph_svg(
  plot_or_expr = function() {
    gplot(
      bluSkynet,
      vertex.cex = (ideg + odeg)^0.5,
      vertex.sides = 50,
      label.cex = 0.4,
      vertex.col = rgb(odeg / max(odeg), 0, ideg / max(ideg)),
      displaylabels = TRUE,
      displayisolates = FALSE
    )
  },
  filename = "5_bluSkynet.svg"
)

# Graph Layouts

library(graphlayouts)
# Nicely
sg0 <- ggraph(g2, layout = "nicely") +
  geom_edge_link(width = 0.2, colour = "grey") +
  geom_node_point(aes(size = total_likes_on_posts, color = reposts_received)) +
  geom_node_text(aes(label = name), repel = TRUE) +
  scale_size_continuous(range = c(3, 12)) +
  scale_color_gradient(low = "lightblue", high = "red") +
  theme_graph()
save_graph_svg(sg0, "graphLayout_1.svg")

# Stress
sg <- ggraph(g2, layout = "stress") +
  geom_edge_link(width = 0.2, colour = "grey") +
  geom_node_point(aes(size = total_likes_on_posts, color = reposts_received)) +
  scale_color_gradient(low = "lightblue", high = "red") +
  theme_graph()
plot(sg)
save_graph_svg(sg, "graphLayout_2.svg")

# Stress Majorization
sg1 <- ggraph(g2, layout = "stress", bbox = 15) +
  geom_node_point(aes(size = total_likes_on_posts, color = reposts_received)) +
  geom_node_text(aes(label = name), repel = TRUE) +
  geom_edge_link(width = 0.2, colour = "grey") +
  scale_size_continuous(range = c(3, 12)) +
  scale_color_gradient(low = "lightblue", high = "red") +
  theme_graph()
plot(sg1)
save_graph_svg(sg1, "graphLayout_3.svg")

gplot(bluSkynet, vertex.cex = sqrt(bet) / 25, gmode = "graph")
