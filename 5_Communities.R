# ============================================================================
# SECTION 5: COMMUNITY STRUCTURE ANALYSIS
# ============================================================================
### TODO: add the NLP analysis of the top communities  ###
### TODO: graph the communities according to rules in MISC folder
### TODO: add scatter to the image of communities
cat("\n=== Step 5a: Computing community structure... ===\n")
# Re-compute if necessary
#comm_louvain <- igraph::cluster_louvain(as_undirected(g)) #-- computed in SQL
#comm_louvain <- igraph::cluster_leiden(as_undirected(g), objective_function = 'modularity', n_iterations = 45, initial_membership = V(g)$community , resolution = 1)
num_communities <- length(unique(V(g)$community))
modularity_louvain <- modularity(g, V(g)$community)
cat(sprintf(
  "\n=== Step 5b: Leiden detection: %d communities, modularity = %.4f ===\n",
  num_communities,
  modularity_louvain
))

cat("\n=== Step 5c: Extracting Community Subgraphs ===\n")
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


cat("\n=== Step 5d: Computing layouts for each community in parallel ===\n")
# compute layouts in parallel for all community x layout combinations
layout_types <- c(
  "drl_fast",
  "drl",
  "fr",
  #"graphopt",
  "lgl",
  "kk",
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
  "\n=== Step 5e: Layouts computed for each community and plotting the graphs ===\n"
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
          " (",
          layout_name,
          ") size: ",
          vcount(subg)
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
    folder = "images"
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
if (sql_server_available()) {
  # Write the Community Layouts to SQL (sequential — shared resource)
  rxWriteObject(
    ds_Graphs,
    "Top 12 Sub Graphs",
    community_layouts,
    overwrite = TRUE
  )
}
par(old_par)

# Name them for easy reference
names(community_graphs) <- paste0("Community_", top_10_ids)
if (sql_server_available()) {
  rxWriteObject(
    ds_Graphs,
    "Community Graphs",
    community_graphs,
    overwrite = TRUE
  )
}

cat(
  "\n=== Step 5f: Analyze Internal Structure, 
For each community subgraph, examine: ===\n"
)
# Batch Analysis
# Analyze all top 10 at once
cat("\n=== Step 5g: Batch analysis of community metrics ===\n")
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

### TODO ; tidy up the community analysis - too many datatables with similiar infromation ###

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
