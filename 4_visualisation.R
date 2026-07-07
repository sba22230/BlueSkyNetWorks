source("0_functions.R")

cat("\n===  Step 4a: Interactive visualization with visNetwork ===")
vis_nodes <- nodes |>
  mutate(
    id = name,
    label = name,
    # hover tooltip
    title = paste0("Name: ", name, "\nTotal Likes: ", total_likes_on_posts),
    value = ifelse(is.na(reposts_received), 0, reposts_received),
    # size by repost_count # nolint: line_length_linter.
    group = name
  ) |>
  distinct(id, .keep_all = TRUE)

vis_nodes <- vis_nodes |>
  mutate(
    title = paste0(
      "<b>",
      name,
      "</b><br/>",
      "Community: ",
      community,
      "<br/>",
      "PageRank: ",
      round(pagerank, 4),
      "<br/>",
      "Betweenness: ",
      round(betweenness_norm, 3),
      "<br/>",
      "k-core: ",
      kcore
    )
  )

vis_edges <- edges |>
  mutate(arrows = "from")
# add arrow pointing to the reposter node

# Build igraph object from edges
vis_g <- graph_from_data_frame(vis_edges, vertices = vis_nodes, directed = TRUE)

# Compute degree for each node # TBD: use the precomputed degree from SQL if available
deg <- igraph::degree(vis_g, mode = "all")

# Add degree to vis_nodes
vis_nodes <- vis_nodes |>
  mutate(degree = deg[id])

# Sort IDs by degree (descending)
sorted_ids <- vis_nodes |>
  arrange((name)) |>
  pull(id)

# --- Load precomputed 'nicely' layout coordinates from SQL ---
res_g <- tryCatch(
  rxReadObject(ds_Graphs, "layout_exec_results"),
  error = function(e) {
    warning("Could not read layout_exec_results from SQL: ", e$message)
    NULL
  }
)

if (is.null(res_g) || length(res_g) == 0) {
  stop(
    "No layout results found in SQL. Ensure layout_exec_results exists in ds_Graphs."
  )
}

nicely_idx <- which(vapply(
  res_g,
  function(x) {
    identical(as.character(attr(x, "layout_type")), "nicely")
  },
  logical(1)
))

if (length(nicely_idx) != 1L) {
  stop(
    "Expected exactly one 'nicely' layout result in layout_exec_results, found ",
    length(nicely_idx),
    "."
  )
}

coords_df <- res_g[[nicely_idx]]
coords_df <- coords_df[match(vis_nodes$id, coords_df$name), c("x", "y")]

if (any(is.na(coords_df))) {
  stop(
    "Mismatch between vis_nodes IDs and layout coordinates; check node names in layout_exec_results."
  )
}

vis_nodes$x <- coords_df[, 1]
vis_nodes$y <- coords_df[, 2]

top_nodes <- vis_nodes |> arrange(desc(degree)) |> slice(1:50) |> pull(id)
vis_nodes$label <- ifelse(vis_nodes$id %in% top_nodes, vis_nodes$label, NA)
plan(orig_plan)
# Community detection -- now done in SQL code
# comm <- cluster_walktrap(vis_g) # works on directed graphs

# Add community membership to nodes
# vis_nodes$community <- comm$membership

# Redefine isolated as degree below 10th percentile, since degree==0 is impossible
# in an edge-list-derived graph (all nodes have at least one edge by definition)
degree_p10 <- quantile(vis_nodes$degree, 0.10, na.rm = TRUE)
vis_nodes$isolated <- vis_nodes$degree <= degree_p10

rxWriteObject(
  ds_Graphs,
  "vis_g_Graph - igraph - community detection",
  vis_g,
  overwrite = TRUE
)

edge_comm_df <- edges |>
  left_join(nodes |> select(name, community), by = c("from" = "name")) |>
  rename(comm_from = community) |>
  left_join(nodes |> select(name, community), by = c("to" = "name")) |>
  rename(comm_to = community)

comm_matrix <- edge_comm_df |>
  count(comm_from, comm_to) |>
  pivot_wider(names_from = comm_to, values_from = n, values_fill = 0)
comm_matrix

library(lattice)
# Focus on top 20 communities by total outgoing interactions
top_comms <- comm_matrix |>
  mutate(total = rowSums(across(-comm_from))) |>
  slice_max(total, n = 20) |>
  pull(comm_from) |>
  as.character()
# Subset rows and matching columns, then convert to matrix
mat <- comm_matrix |>
  filter(comm_from %in% top_comms) |>
  select(comm_from, any_of(top_comms)) |>
  tibble::column_to_rownames("comm_from") |>
  as.matrix()
# Log-transform (add 1 to handle zeros)
mat_log <- log1p(mat)
plotobj <- levelplot(
  mat_log,
  xlab = "Community (to)",
  ylab = "Community (from)",
  main = "Cross-community interactions (log scale)",
  col.regions = viridis::viridis(100),
  scales = list(x = list(rot = 45))
) # this is a good view of community interaction

save_graph_svg(
  plotobj,
  filename = "Cross Community interaction.svg",
  folder = "docs/images"
)

# ========================================================================
# COMMUNITY CHARACTERIZATION & LABELING
# ========================================================================
# Analyze what defines each community to create meaningful labels
# TBD needs to use the communities already computed

cat("\n=== ANALYZING COMMUNITIES ===\n")

# Get community member data
community_analysis <- vis_nodes |>
  group_by(community) |>
  summarise(
    # Size and structure
    size = n(),
    isolated_count = sum(isolated, na.rm = TRUE),
    active_members = sum(!isolated, na.rm = TRUE),
    isolated_pct = isolated_count / size,

    # Connectivity metrics - degree measures the number of
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
  ) |>
  arrange(desc(size))

cat("Community Statistics:\n")
datatable(slice(community_analysis, 1:10))

# Pre-compute thresholds once to avoid repeated evaluation inside mutate()
# and to avoid fragile self-referencing via community_analysis$ inside a pipe
med_degree <- median(community_analysis$avg_degree)
q75_repost <- quantile(community_analysis$avg_repost_count, 0.75)

# Define community types based on characteristics
community_labels <- community_analysis |>
  mutate(
    community_type = case_when(
      # Peripheral check first: communities where 50%+ are low-degree nodes
      # (degree <= 10th percentile). This is a meaningful measure in edge-list-derived graphs
      isolated_pct >= 0.5 ~ "Peripheral",

      # Large, highly connected communities (hubs)
      size >= 50 & avg_degree > med_degree * 1.5 ~ "Core Hub",

      # Large but not highly connected — avoid falling to Discussion Group
      size >= 50 ~ "Large Community",

      # Medium-sized, moderately connected
      size >= 20 & avg_degree >= med_degree ~ "Active Circle",

      # Smaller, tightly knit groups
      size >= 10 & avg_degree > med_degree ~ "Tight Cluster",

      # Small but highly engaged (high repost count relative to size)
      size < 10 & avg_repost_count > q75_repost ~ "Engaged Micro-Group",

      # Default: catchall
      TRUE ~ "Discussion Group"
    ),

    # Note: \n renders in plot labels; use a flat string for table display
    label = sprintf(
      "Community %d: %s\n(%d members, %.1f%% low-degree)",
      community,
      community_type,
      size,
      isolated_pct * 100
    )
  ) |>
  select(
    community,
    community_type,
    label,
    size,
    avg_degree,
    top_member,
    isolated_pct
  )

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

vis_nodes <- vis_nodes |>
  mutate(
    community_label = community_label_map[as.character(community)],
    community_type = community_type_map[as.character(community)],
    # Use community_label as the group so legend displays full community descriptions
    group = community_label
  )

# Print community members for inspection
cat("\n=== COMMUNITY MEMBERSHIP BREAKDOWN ===\n")
for (comm_id in sort(unique(vis_nodes$community))) {
  members <- vis_nodes |>
    filter(community == comm_id) |>
    arrange(desc(degree)) |>
    slice(1:10) |>
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
# Filter edges to reduce file size: keep only edges above the median weight
# (adjust the quantile threshold if the graph is still too large/small)
edge_weight_col <- if ("weight" %in% names(vis_edges)) {
  "weight"
} else if ("value" %in% names(vis_edges)) {
  "value"
} else {
  NULL
}

vis_edges_filtered <- if (!is.null(edge_weight_col)) {
  threshold <- quantile(vis_edges[[edge_weight_col]], 0.5, na.rm = TRUE)
  vis_edges[vis_edges[[edge_weight_col]] >= threshold, ]
} else {
  vis_edges
}

# --- Node selection highlight handlers ---
# Original attributes are pre-cached in onRender and stored on the container
# element (el._visOrigAttrs) so state is scoped per widget, not global.

js_select_node <- htmlwidgets::JS(
  "
  function(params) {
    if (!params.nodes || params.nodes.length === 0) return;
    var nid      = params.nodes[0];
    var origAttrs = this.body.container._visOrigAttrs || {};
    var orig     = origAttrs[nid] || {};
    var newSize  = orig.size ? Math.min(orig.size * 1.2, orig.size + 8) : 18;
    this.body.data.nodes.update([{ id: nid, size: newSize, color: { border: '#FF4444' } }]);
    try {
      this.focus(nid, { scale: 1.0, animation: { duration: 300, easingFunction: 'easeInOutQuad' } });
    } catch(e) { console.warn('visNetwork focus error:', e); }
  }
"
)

# Only restores the nodes that were previously selected (O(k), not O(n)).
js_deselect_node <- htmlwidgets::JS(
  "
  function(params) {
    var prev = params.previousSelection && params.previousSelection.nodes;
    if (!prev || prev.length === 0) return;
    var origAttrs = this.body.container._visOrigAttrs || {};
    var updates = prev.map(function(nid) {
      var o = origAttrs[nid] || {};
      return { id: nid, size: o.size || 10, color: o.color || { border: '#000000' } };
    });
    this.body.data.nodes.update(updates);
  }
"
)

vis_obj <- visNetwork(
  vis_nodes,
  vis_edges_filtered,
  width = "100%",
  height = "100vh"
) |>
  # Coordinates are precomputed — physics is not needed and is the main
  # cause of browser sluggishness on large graphs
  visPhysics(enabled = FALSE) |>
  visNodes(fixed = TRUE) |>
  visOptions(
    highlightNearest = list(enabled = TRUE, degree = 1), # degree 2 is expensive
    nodesIdSelection = list(values = sorted_ids),
    selectedBy = "community_label",
    manipulation = FALSE # disable edit toolbar unless needed
  ) |>
  visEdges(arrows = "to", smooth = TRUE) |>
  visLegend(useGroups = TRUE, position = "right") |>
  visInteraction(
    navigationButtons = TRUE,
    dragNodes = FALSE, # fixed layout — dragging nodes is misleading
    dragView = TRUE,
    zoomView = TRUE
  ) |>
  # Enlarge and re-border the selected node; restore on deselect.
  visEvents(selectNode = js_select_node, deselectNode = js_deselect_node) |>
  htmlwidgets::onRender(htmlwidgets::JS(
    "
    function(el, x) {

      // Pre-cache original node attributes on the container element so the
      // selectNode / deselectNode handlers can read and restore them cheaply.
      // Scoped to el (not window) to support multiple widgets on the same page.
      el._visOrigAttrs = {};
      x.nodes.forEach(function(n) {
        el._visOrigAttrs[n.id] = { size: n.size, color: n.color };
      });

      // Index nodes by community for fast dropdown filtering
      var nodesByCommunity = {};
      var allNodeIds = [];
      x.nodes.forEach(function(n) {
        allNodeIds.push(n.id);
        var comm = String(n.community || 'unknown');
        if (!nodesByCommunity[comm]) nodesByCommunity[comm] = [];
        nodesByCommunity[comm].push(n.id);
      });

      // Find the community and node ID dropdowns in the DOM.
      // NOTE: these selectors rely on visNetwork's internal DOM naming and may
      // need updating if the package changes its generated HTML structure.
      var communitySelect = el.querySelector('select[id$=\"_selectVar\"]');
      var nodeIdSelect    = el.querySelector('select[id$=\"nodesIdSelection\"]');
      if (!communitySelect || !nodeIdSelect) return;

      var commLabelMap = {};
      x.nodes.forEach(function(n) {
        commLabelMap[n.community_label || ''] = n.community;
      });

      // Rebuild the node ID dropdown to show only nodes in the selected community
      function updateNodeIdOptions() {
        var selectedLabel = communitySelect.value;
        var selectedComm  = commLabelMap[selectedLabel];
        var nodesForComm  = selectedComm !== undefined
          ? (nodesByCommunity[String(selectedComm)] || [])
          : allNodeIds;

        nodeIdSelect.innerHTML = '';
        nodesForComm.forEach(function(nid) {
          var opt = document.createElement('option');
          opt.value       = nid;
          opt.textContent = nid;
          nodeIdSelect.appendChild(opt);
        });

        // Auto-select and focus the first node in the community
        if (nodesForComm.length > 0) {
          var firstNodeId = nodesForComm[0];
          nodeIdSelect.value = firstNodeId;
          setTimeout(function() {
            try {
              var network = el.vislibrary;
              if (network) {
                network.selectNodes([firstNodeId], false);
                var node = network.body.data.nodes.get(firstNodeId);
                if (node && node.x !== undefined && node.y !== undefined) {
                  network.focus(firstNodeId, { scale: 1.0, animation: { duration: 500, easingFunction: 'easeInOutQuad' } });
                }
              }
            } catch(e) { console.warn('visNetwork focus error:', e); }
          }, 100);
        }
      }

      communitySelect.addEventListener('change', updateNodeIdOptions);
      updateNodeIdOptions();
    }
  "
  ))
# save HTML and capture as SVG (requires webshot2 and
# a headless Chrome/Chromium)
htmlwidgets::saveWidget(
  vis_obj,
  "docs/graphs/visnetwork_tmp.html",
  selfcontained = FALSE
)
rxWriteObject(
  ds_Graphs,
  "vis_g_Visnetwork - visNetwork - layout_with_lgl",
  vis_obj,
  overwrite = TRUE
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

# Edges and attributes (preserve width/weight and color)
ge_edges <- data.frame(
  source = vis_edges$from,
  target = vis_edges$to,
  stringsAsFactors = FALSE
)
# Safe edge attributes (handles missing width/weight/color)
if (any(c("width", "weight", "color") %in% names(vis_edges))) {
  ge_edgesAtt <- vis_edges |>
    mutate(
      weight = if ("width" %in% names(vis_edges)) {
        ifelse(is.na(width), 1, as.numeric(width))
      } else if ("weight" %in% names(vis_edges)) {
        ifelse(is.na(weight), 1, as.numeric(weight))
      } else {
        1
      },
      color_raw = if ("color" %in% names(vis_edges)) {
        ifelse(is.na(color) | color == "", "#AAAAAA", color)
      } else {
        "#AAAAAA"
      },
      # NEW: keep dynamic info as attributes
      edge_start = as.character(edgeStarts),
      edge_end = as.character(edgeEnds)
    ) |>
    select(-from, -to, -any_of(c("width", "color")))
} else {
  ge_edgesAtt <- vis_edges |>
    mutate(
      weight = 1,
      color_raw = "#AAAAAA",
      edge_start = as.character(edgeStarts),
      edge_end = as.character(edgeEnds)
    ) |>
    select(-from, -to)
}

ge_edgeDynamic <- if (
  "edge_start" %in% names(ge_edgesAtt) && "edge_end" %in% names(ge_edgesAtt)
) {
  data.frame(
    start = ge_edgesAtt$edge_start,
    end = ge_edgesAtt$edge_end
  )
} else {
  NULL
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
  edgesAtt = ge_edgesAtt |> select(-color_raw),
  nodesVizAtt = ge_nodesVizAtt,
  edgeDynamic = ge_edgeDynamic,
  edgesVizAtt = list(
    color = ge_edgesVizColor,
    size = as.numeric(ge_edgesAtt$weight)
  ),
  encoding = "UTF-8",
  defaultedgetype = "directed",
  rescale.node.size = TRUE,
  relsize = max(0.01, 1 / nrow(nodes)),
  radius = 500
)

# Save to file
home_dir <- here::here()
file_path <- file.path(home_dir, "docs/graphs")
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

# ============================================================================
# SECTION 5: COMMUNITY STRUCTURE ANALYSIS
# ============================================================================

cat("\n=== Step 4b: Computing community structure... ===\n")
# Re-compute if necessary
#comm_louvain <- igraph::cluster_louvain(as_undirected(g)) #-- computed in SQL
#comm_louvain <- igraph::cluster_leiden(as_undirected(g), objective_function = 'modularity', n_iterations = 45, initial_membership = V(g)$community , resolution = 1)
num_communities <- length(unique(V(g)$community))
modularity_louvain <- modularity(g, V(g)$community)
cat(sprintf(
  "\n=== Step 4c: Leiden detection: %d communities, modularity = %.4f ===\n",
  num_communities,
  modularity_louvain
))

cat("\n=== Step 4c: Extracting Community Subgraphs ===\n")
# Assuming you have your community detection results
#communities <- comm_louvain # or your preferred method
membership <- setNames(as.numeric(V(g)$community), V(g)$name)

# Get top communities and subgraphs (unchanged)
comm_sizes <- sort(table(membership), decreasing = TRUE)
top_10_ids <- as.numeric(names(comm_sizes[1:12]))

community_graphs <- lapply(top_10_ids, function(id) {
  nodes <- which(membership == id)
  induced_subgraph(g, nodes)
})


cat("\n=== Step 4d: Computing layouts for each community in parallel ===\n")
# compute layouts in parallel for all community x layout combinations
layout_types <- c(
  "drl_fast",
  #"drl",
  #,"fr"
  #"graphopt",
  #,"lgl"
  #"kk",
  #,"mds"
  "nicely"
  #,"tree"
)
n_comm <- length(community_graphs)
# replicate indices/layouts so rxExec runs every combo
graph_idx_rep <- rep(seq_len(n_comm), each = length(layout_types))
layout_rep <- rep(layout_types, times = n_comm)
graphs_arg <- lapply(graph_idx_rep, function(ii) community_graphs[[ii]])

# run layout_exec in parallel across the cluster once
res_all <- rxExec(
  layout_exec,
  graph = rxElemArg(graphs_arg),
  layout_type = rxElemArg(layout_rep),
  execObjects = c("connStr", "layout_exec")
)

# Load the res_all object to SQL
rxWriteObject(ds_Graphs, "Res All", res_all, overwrite = TRUE)

# assemble nested list: community_layouts[[i]] = list(id, graph, layouts = named list(layout -> coord_matrix))
community_layouts <- vector("list", n_comm)
names(community_layouts) <- paste0("Community_", top_10_ids)
for (i in seq_len(n_comm)) {
  community_layouts[[i]] <- list(
    id = top_10_ids[i],
    graph = community_graphs[[i]],
    layouts = list()
  )
}

for (k in seq_along(res_all)) {
  comm_i <- graph_idx_rep[k]
  lt <- as.character(attr(res_all[[k]], "layout_type"))
  coords_df <- res_all[[k]]
  coords_mat <- as.matrix(coords_df[, c("x", "y")])
  community_layouts[[comm_i]]$layouts[[lt]] <- coords_mat
}

cat(
  "\n=== Step 4e: Layouts computed for each community and plotting the graphs ===\n"
)

pal <- viridis::viridis(max(V(g)$kcore, na.rm = TRUE) + 1)

# Worker function: renders all layouts for one community into an SVG file
plot_community_svg <- function(entry, pal, save_graph_svg) {
  subg <- entry$graph
  layouts_list <- entry$layouts
  m <- length(layouts_list)
  if (m == 0) {
    return(invisible(NULL))
  }

  plot_nrow <- ceiling(sqrt(m))
  plot_ncol <- ceiling(m / plot_nrow)

  save_graph_svg(
    plot_or_expr = function() {
      par(mfrow = c(plot_nrow, plot_ncol), mar = c(1, 1, 2, 1))
      vsize <- if (!is.null(igraph::V(subg)$pagerank)) {
        igraph::V(subg)$pagerank + 1
      } else {
        rep(6, igraph::vcount(subg))
      }
      vcol <- if (!is.null(igraph::V(subg)$kcore)) {
        pal[as.integer(igraph::V(subg)$kcore) + 1]
      } else {
        "steelblue"
      }
      for (k in seq_len(m)) {
        layout_name <- names(layouts_list)[k]
        coords <- layouts_list[[k]]
        main_title <- paste0(
          "Community ",
          entry$id,
          " ",
          layout_name,
          ")"
        )
        igraph::plot.igraph(
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
    filename = paste0("Community_", entry$id, "_layouts.svg"),
    folder = "docs/images"
  )
  invisible(NULL)
}

# Plot all communities in parallel — one SVG per community
rxExec(
  FUN = plot_community_svg,
  entry = rxElemArg(community_layouts),
  pal = pal,
  save_graph_svg = save_graph_svg,
  packagesToLoad = c("igraph", "viridis")
)

# Write the Community Layouts to SQL (sequential — shared resource)
rxWriteObject(
  ds_Graphs,
  "Top 12 Sub Graphs",
  community_layouts,
  overwrite = TRUE
)
par(old_par)

# Name them for easy reference
names(community_graphs) <- paste0("Community_", top_10_ids)
rxWriteObject(ds_Graphs, "Community Graphs", community_graphs, overwrite = TRUE)

cat(
  "\n=== Step 4f: Analyze Internal Structure, 
For each community subgraph, examine: ===\n"
)
# Batch Analysis
# Analyze all top 10 at once
cat("\n=== Step 4g: Batch analysis of community metrics ===\n")
community_metrics <- data.frame(
  community = names(community_graphs),
  size = sapply(community_graphs, vcount),
  density = sapply(community_graphs, edge_density),
  diameter = sapply(community_graphs, diameter),
  avg_path = sapply(community_graphs, mean_distance),
  transitivity = sapply(community_graphs, function(g) {
    transitivity(g, type = "global")
  }),
  betweenness_avg = sapply(community_graphs, function(g) {
    mean(igraph::betweenness(g))
  }),
  degree_avg = sapply(community_graphs, function(g) {
    mean(igraph::degree(g))
  })
)

# Label propagation (can detect overlapping communities)
comm_labelprop <- cluster_label_prop(g)
num_communities_lp <- length(unique(comm_labelprop$membership))
modularity_labelprop <- modularity(g, comm_labelprop$membership)
cat(sprintf(
  "Label propagation: %d communities, modularity = %.4f\n",
  num_communities_lp,
  modularity_labelprop
))

# Fast greedy (for directed, may treat as undirected)
#comm_fastgreedy <- cluster_fast_greedy(as_undirected(g))
#num_communities_fg <- length(unique(comm_fastgreedy$membership))
#modularity_fastgreedy <- modularity(
# as_undirected(g),
#  comm_fastgreedy$membership
#)
#cat(sprintf(
#  "Fast greedy: %d communities, modularity = %.4f\n",
#  num_communities_fg,
#  modularity_fastgreedy
#))

# Add best community assignment to nodes
#best_comm <- comm_louvain$membership
#nodes_with_metrics <- nodes_with_metrics |>
#  dplyr::mutate(
#    community = best_comm[name],
#    modularity = modularity_louvain
#  )

# Community statistics (size, internal density, external connections)
community_stats <- nodes |>
  group_by(community) |>
  summarise(
    community_size = n(),
    # rough estimate
    internal_edges = sum(in_degree[name %in% name], na.rm = TRUE) / 2,
    avg_internal_degree = mean(in_degree + out_degree, na.rm = TRUE),
    avg_pagerank = mean(pagerank, na.rm = TRUE),
    avg_authority = mean(authority_score, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(community_size))

cat(sprintf(" Community analysis complete:\n"))
datatable(community_stats)

cat("Interactive visualization ready with precomputed layout.\n")

posts_df <- read_parquet("data/speirgorm_network.parquet")
top_authors_reposted <- posts_df |>
  group_by(PostedBy) |>
  summarise(total_reposts = sum(repost_count, na.rm = TRUE)) |>
  arrange(desc(total_reposts)) |>
  slice(1:10)
top_reposters <- posts_df |>
  group_by(RepostedBy) |> # group by both
  summarise(total_reposts_made = n(), .groups = "drop") |> # count reposts
  arrange(desc(total_reposts_made)) |> # sort descending
  slice(1:10) # top 10

library(DT)

datatable(top_authors_reposted, caption = "Top 10 Authors by Reposts Received")
datatable(top_reposters, caption = "Top 10 Accounts by Reposts Made")

tobermvd <- c(
  'bluSkynet',
  'community_layouts',
  'desc_df',
  'desc_df_chr',
  'edge_comm_df',
  'edge_rgba_df',
  'ge_edgeDynamic',
  'ge_edges',
  'ge_edgesAtt',
  'ge_edgesVizColor',
  'ge_nodes',
  'ge_nodesAtt',
  'ge_nodesVizAtt',
  'graphs_arg',
  'layouts',
  'node_rgba_df',
  'nodes_with_metrics',
  'vis_edges',
  'vis_g',
  'vis_nodes',
  'gtn',
  'gto'
)
rm(list = tobermvd)
gc()

# End of script
