# source("0_functions.R")

cat("\n=== igraph layouts: Computing different layouts for igraph ===\n")

layout_types <- c(
  "drl",
  "drl_fast",
  "fr",
  "graphopt",
  "lgl",
  "kk",
  "mds",
  "nicely",
  "tree"
)

n_comm <- 1
# replicate indices/layouts so rxExec runs every combo
graph_idx_rep <- rep(seq_len(n_comm), each = length(layout_types))
layout_rep <- rep(layout_types, times = n_comm)
# Use the main graph object (g) instead of community-specific graphs
graphs_arg <- lapply(graph_idx_rep, function(ii) g)

library(tictoc)
tic()
# run layout_exec in parallel across the cluster once
res_g <- rxExec(
  layout_exec,
  graph = rxElemArg(graphs_arg),
  layout_type = rxElemArg(layout_rep),
  execObjects = c("connStr", "layout_exec")
)

toc()

rxWriteObject(
  ds_Graphs,
  "layout_exec_results",
  res_g,
  overwrite = TRUE
)

# assemble nested list: community_layouts[[i]] = list(id, graph, layouts = named list(layout -> coord_matrix))
old_par <- par(no.readonly = TRUE)
pal <- viridis::viridis(max(V(g)$kcore, na.rm = TRUE) + 1)
community_layouts <- vector("list", n_comm)
names(community_layouts) <- paste0("Main Graph ", vcount(g), " nodes") # or use community names if available
for (i in seq_len(n_comm)) {
  community_layouts[[i]] <- list(
    id = g,
    graph = g,
    layouts = list()
  )

  for (k in seq_along(res_g)) {
    comm_i <- graph_idx_rep[k]
    lt <- as.character(attr(res_g[[k]], "layout_type"))
    coords_df <- res_g[[k]]
    coords_mat <- as.matrix(coords_df[, c("x", "y")])
    community_layouts[[comm_i]]$layouts[[lt]] <- coords_mat
  }

  entry <- community_layouts[[i]]
  subg <- entry$graph
  layouts_list <- entry$layouts
  m <- length(layouts_list)
  if (m == 0) {
    next
  }

  plot_nrow <- ceiling(sqrt(m))
  plot_ncol <- ceiling(m / plot_nrow)
  save_graph_svg(
    plot_or_expr = function() {
      par(mfrow = c(plot_nrow, plot_ncol), mar = c(1, 1, 2, 1))

      vsize <- if (!is.null(V(subg)$pagerank)) {
        V(subg)$pagerank + 1
      } else {
        rep(6, vcount(subg))
      }
      vcol <- if (!is.null(V(subg)$kcore)) {
        pal[as.integer(V(subg)$kcore) + 1]
      } else {
        "steelblue"
      }

      for (k in seq_len(m)) {
        layout_name <- names(layouts_list)[k]
        coords <- layouts_list[[k]]
        main_title <- paste0(
          "Main Graph ",
          vcount(subg),
          " nodes\n(",
          layout_name,
          ")"
        )
        plot.igraph(
          subg,
          layout = coords,
          main = main_title,
          vertex.size = vsize,
          vertex.label.cex = 0.2,
          edge.arrow.size = 0.3,
          vertex.color = vcol
        )
      }
    },
    filename = paste0("Main Graph_layouts.svg"),
    folder = "images"
  )
}
par(old_par)
# Select top N nodes to label (e.g., top 50 by degree)
deg <- bsn_degree
top_nodes <- names(sort(deg, decreasing = TRUE))[1:50]

# Faster ggraph call
coords_drl <- res_g[[1]]
gtn <- ggraph(
  g,
  layout = "manual",
  x = coords_drl[, 1],
  y = coords_drl[, 2]
) +
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
coords_graphopt <- res_g[[4]]
gto <- ggraph(
  g,
  layout = "manual",
  x = coords_graphopt[, 1],
  y = coords_graphopt[, 2]
) +
  geom_edge_link(alpha = 0.3) +
  geom_node_point(aes(size = reposts_made, color = reposts_received)) +
  geom_node_text(aes(label = name), repel = TRUE) +
  scale_size_continuous(range = c(3, 12)) +
  scale_color_gradient(low = "lightblue", high = "red") +
  theme_void()

write_graph(
  g,
  "graphs/g bluesky Speirgorm Network RepostsMade vs RepostsReceived.graphml",
  format = "graphml"
)
rxWriteObject(
  ds_Graphs,
  "g_Graph - igraph - layout_with_graphopt",
  gto,
  overwrite = TRUE
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

# Build igraph object from edges
vis_g <- graph_from_data_frame(vis_edges, vertices = vis_nodes, directed = TRUE)

# Compute degree for each node
deg <- igraph::degree(vis_g, mode = "all")

# Add degree to vis_nodes
vis_nodes <- vis_nodes %>%
  mutate(degree = deg[id])

# Sort IDs by degree (descending)
sorted_ids <- vis_nodes %>%
  arrange(desc(degree)) %>%
  pull(id)

# --- Precompute layout coordinates with igraph ---
plan(multisession, workers = wrkrs)
comps <- igraph::components(vis_g)
layouts <- future_map(
  unique(comps$membership),
  function(comp_id) {
    subg <- induced_subgraph(vis_g, which(comps$membership == comp_id))
    if (vcount(subg) > 100) layout_with_lgl(subg) else layout_with_kk(subg)
  },
  .progress = TRUE,
  .options = furrr_options(seed = 22230)
)

coords3 <- matrix(NA, nrow = vcount(vis_g), ncol = 2)
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
plan(orig_plan)
# Community detection
comm <- cluster_walktrap(vis_g) # works on directed graphs

rxWriteObject(
  ds_Graphs,
  "vis_g_Graph - igraph - community detection",
  vis_g,
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
# TBD needs to use the communities already computed

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
  visNodes(fixed = FALSE) %>%
  visOptions(
    highlightNearest = list(enabled = TRUE, degree = 2),
    nodesIdSelection = list(values = sorted_ids),
    selectedBy = "community_label",
    manipulation = TRUE
  ) %>%
  visEdges(arrows = "to", smooth = FALSE) %>%
  visLegend(useGroups = TRUE, position = "right") %>%
  visInteraction(
    navigationButtons = TRUE,
    dragNodes = TRUE,
    dragView = TRUE,
    zoomView = TRUE
  ) %>%
  visPhysics(
    enabled = TRUE,
    solver = "forceAtlas2Based",
    forceAtlas2Based = list(gravitationalConstant = -50)
  )
# save HTML and capture as SVG (requires webshot2 and
# a headless Chrome/Chromium)
htmlwidgets::saveWidget(
  vis_obj,
  "graphs/visnetwork_tmp.html",
  selfcontained = TRUE
)
rxWriteObject(
  ds_Graphs,
  "vis_g_Visnetwork - visNetwork - layout_with_lgl",
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

# safe color parsing for edges
# parallel parse edges
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

plan(orig_plan)
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


# Save to file
home_dir <- here::here()
file_path <- file.path(home_dir, "graphs")
file_name <- file.path(file_path, "vis_g visnetwork_export.gexf")
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
