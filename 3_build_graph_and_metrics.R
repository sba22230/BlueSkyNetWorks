# source("0_functions.R")

# Step 1: Load data
edges_df <- read_parquet("graphs/speirgorm_edges.parquet")
nodes_df <- read_parquet("graphs/speirgorm_nodes.parquet")

# plan(multisession, workers = wrkrs)  ## to be moved closer to its use
# ============================================================================
# SECTION 1: Create the Graph
# ============================================================================
cat("\n=== Step 3a: Build the initial graph... ===\n")
# Step 1: Build edge list (who reposted whom)
set.seed(22230)
num_posts <- nrow(edges_df)
#num_posts <- 7000
# if tthe num_posts is set, then we sample that many posts
# else we keep all reposts
if (!is.null(num_posts) && nrow(edges_df) > num_posts) {
  # 1. Sample edges from edges_df
  sampled_edges <- edges_df %>% dplyr::sample_n(num_posts)
  # 2. Identify nodes appearing in sampled edges
  nodes_set <- union(sampled_edges$from, sampled_edges$to)
  # 3. Filter nodes_df to keep only relevant nodes
  sampled_nodes <- nodes_df %>% dplyr::filter(name %in% nodes_set)
  # 4. Assign to your expected output variables
  edges <- sampled_edges
  nodes <- sampled_nodes
  cat("Sampled", num_posts, "edges; resulting nodes:", nrow(nodes), "\n")
} else {
  edges <- edges_df
  nodes <- nodes_df
  cat("Keeping all reposts; total reposts count:", nrow(edges), "\n")
}

n_iter <- round(nrow(edges) / 100)

cat("Edges count:", nrow(edges), "\n")

# Debug: inspect edges
print(head(edges))

# remove NAs
#edges[is.na(edges)] <- "no text here"

na_per_column <- colSums(is.na(edges))
print("Number of NAs per column:")
print(na_per_column)

cat("\n=== Step 3b: Build node list (unique actors and posts ===\n")

# Step 2: Build node list (unique actors and posts)
cat("Nodes count:", nrow(nodes), "\n")

# Debug: inspect nodes
print(head(nodes))

na_per_column <- colSums(is.na(nodes))
print("Number of NAs per column:")
print(na_per_column)

cat("\n=== Step 3c: Build igraph object and plot basic network ===\n")
# Step 3: Build igraph object and plot basic network
g <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
cat("Graph summary:\n")
print(summary(g))

# save the graph object for posterity
rxWriteObject(
  ds_Graphs,
  paste0("g igraph object - all data ", nrow(nodes)),
  g,
  overwrite = TRUE
)

# ============================================================================
# SECTION 2: Network-LEVEL METRICS
# ============================================================================
cat("\n=== Step 3d: Build out the metrics using igraph... ===\n")

ig_network_size <- vcount(g)
ig_comp <- igraph::components(g)
# Connected components
ig_num_components <- ig_comp$no
ig_largest_component_size <- max(ig_comp$csize)
ig_giant_component_pct <- (ig_largest_component_size / ig_network_size) *
  100
# Average local clustering
ig_avg_local_clustering <- mean(V(g)$local_clustering, na.rm = TRUE)
# Centralization (degree based)

ig_centralization_in <- (sum(max(V(g)$in_degree) - V(g)$in_degree)) /
  ((ig_network_size - 1) * (ig_network_size - 2))
ig_cen <- igraph::dyad_census(g)
ig_summary_df <- tibble(
  # DATEADD(DAY, CAST([analysis_timestamp] AS int), '1970-01-01')  AS analysis_timestamp
  analysis_timestamp = Sys.Date(),
  method = "igraph",
  network_size = ig_network_size,
  edge_count = ecount(g),
  dyad_count = sum(ig_cen$mut, ig_cen$asym, ig_cen$null),
  density = edge_density(g),
  mutual_pairs = ig_cen$mut,
  asymetric_pairs = ig_cen$asym,
  isolated_nodes = ig_cen$null,
  diameter = diameter(g, directed = TRUE, weights = NA),
  avg_path_length = mean_distance(g, directed = TRUE),
  neighbours_average = mean(neighbors(g, V(g), mode = "all")),
  reciprocity_default = reciprocity(g, mode = "default"),
  reciprocity_ratio = reciprocity(g, mode = "ratio"),
  average_in_degree = mean(V(g)$in_degree),
  average_out_degree = mean(V(g)$out_degree),
  most_replied_to = V(g)$name[which.max(V(g)$in_degree)],
  most_active_replier = V(g)$name[which.max(V(g)$out_degree)],
  num_components = ig_num_components,
  largest_component_size = ig_largest_component_size,
  giant_component_pct = ig_giant_component_pct,
  transitivity_val = transitivity(g, type = "global"),
  avg_local_clustering = ig_avg_local_clustering,
  centralization_in = ig_centralization_in
)

str(ig_summary_df)
# Write metrics back into network metric table
ig_sql <- RxSqlServerData(
  table = "dbo.NetworkLevelMetrics",
  connectionString = connStr,
  colInfo = list('analysis_timestamp' = list(type = "Date"))
)
rxDataStep(ig_summary_df, ig_sql, append = "rows")

cat(
  "\n=== Step 3e: BlueSkyNetWorks: Computing Network Metrics using StatNet ===\n"
)

# Convert igraph to statnet
library(network)
bluSkynet <- asNetwork(g)
rxWriteObject(
  ds_Graphs,
  paste0("bluSkynet_Graph - network - no posts: ", num_posts),
  bluSkynet,
  overwrite = TRUE
)

bsn_network_size <- network.size(bluSkynet)
bsn_degree <- sna::degree(bluSkynet)
bsn_ideg <- sna::degree(bluSkynet, cmode = "indegree")
bsn_odeg <- sna::degree(bluSkynet, cmode = "outdegree")

cat("\n=== Step 3f: Plotting degree distributions and scatter plot ===\n")

save_graph_svg(
  plot_or_expr = function() {
    plot(bsn_ideg, bsn_odeg, type = "n", xlab = "Incoming", ylab = "Outgoing")
    abline(0, 1, lty = 3)
    text(
      jitter(bsn_ideg),
      jitter(bsn_odeg),
      labels = network.vertex.names(bluSkynet),
      cex = 0.5,
      col = 2
    )
  },
  filename = "degree_scatter.svg"
)

save_graph_svg(
  plot_or_expr = function() {
    hist(
      bsn_ideg,
      xlab = "Indegree",
      main = "Indgree Distribution",
      prob = TRUE
    )
  },
  filename = "indegree_dist.svg"
)

save_graph_svg(
  plot_or_expr = function() {
    hist(
      bsn_odeg,
      xlab = "Outdegree",
      main = "Outdgree Distribution",
      prob = TRUE
    )
  },
  filename = "outdegree_dist.svg"
)

cat("\n=== Step 3g: Compute network metrics using StatNet ===\n")
bsn_dyadcensus <- sna::dyad.census(bluSkynet)
bsn_dyadcount <- sum(
  bsn_dyadcensus[[1]],
  bsn_dyadcensus[[2]],
  bsn_dyadcensus[[3]]
)
bsn_cen <- sna::centralization(bluSkynet, sna::degree, cmode = "indegree")
bsn_ceneig <- sna::centralization(bluSkynet, sna::evcent)
dist_matrix <- geodist(bluSkynet)$gdist
gc()
bsn_diameter <- max(dist_matrix[is.finite(dist_matrix)])
bsn_comp <- component.dist(bluSkynet, connected = "weak")
bsn_num_components <- length(bsn_comp$csize)
bsn_largest_component_size <- max(bsn_comp$csize)
bsn_largest_component_size_pct <- (bsn_largest_component_size /
  bsn_network_size) *
  100
# Local Clustering Coefficient (for directed graphs, variants exist)
bsn_local_clustering <- ig_avg_local_clustering # Placeholder: SNA's clustering is more complex for directed graphs
bsn_Global_clustering <- gtrans(bluSkynet, mode = "digraph", measure = "weak")
# Centralization (degree based)
bsn_centralization_in <- (sum(max(bsn_ideg) - bsn_ideg)) /
  ((bsn_network_size - 1) * (bsn_network_size - 2))
bsn_summary_df <- tibble(
  #DATEADD(DAY, CAST([analysis_timestamp] AS int), '1970-01-01')  AS analysis_timestamp
  analysis_timestamp = Sys.Date(),
  method = "network/sna",
  network_size = bsn_network_size,
  edge_count = network.edgecount(bluSkynet),
  dyad_count = bsn_dyadcount,
  density = gden(bluSkynet),
  mutual_pairs = bsn_dyadcensus[[1]],
  asymetric_pairs = bsn_dyadcensus[[2]],
  isolated_nodes = bsn_dyadcensus[[3]],
  diameter = bsn_diameter,
  avg_path_length = mean(dist_matrix[is.finite(dist_matrix)]),
  neighbours_average = mean(neighbors(g, V(g), mode = "all")),
  reciprocity_default = grecip(bluSkynet, measure = "edgewise"),
  reciprocity_ratio = grecip(bluSkynet, measure = "dyadic.nonnull"),
  average_in_degree = mean(bsn_ideg),
  average_out_degree = mean(bsn_odeg),
  most_replied_to = network.vertex.names(bluSkynet)[which.max(bsn_ideg)],
  most_active_replier = network.vertex.names(bluSkynet)[which.max(bsn_odeg)],
  num_components = bsn_num_components,
  largest_component_size = bsn_largest_component_size,
  giant_component_pct = bsn_largest_component_size_pct,
  transitivity_val = bsn_Global_clustering,
  avg_local_clustering = bsn_local_clustering,
  centralization_in = bsn_centralization_in
)

str(bsn_summary_df)
# Write metrics back into network metric table
ig_sql <- RxSqlServerData(
  table = "dbo.NetworkLevelMetrics",
  connectionString = connStr,
  colInfo = list("analysis_timestamp" = list(type = "Date"))
)
rxDataStep(bsn_summary_df, ig_sql, append = "rows")

cat("\n=== Step 3h: Show both Network Metrics in one table ===\n")

desc_df <- tibble(
  analysis_timestamp = "Date that the metrics were generated",
  method = "description",
  network_size = "Number of nodes (users) in network",
  edge_count = "Number of directed edges (reposts)",
  dyad_count = "Total number of possible dyadic pairs",
  density = "Proportion of possible edges present",
  mutual_pairs = "Number of mutual dyads (both directions present)",
  asymetric_pairs = "Number of asymmetric dyads (one direction only)",
  isolated_nodes = "Number of dyads with no connection",
  diameter = "Longest shortest path between any two nodes",
  avg_path_length = "Average shortest path across all node pairs",
  neighbours_average = "Average number of adjacent nodes per node",
  reciprocity_default = "Proportion of edges that are reciprocated (edgewise reciprocity)",
  reciprocity_ratio = "Proportion of non-null dyads that are mutual (dyadic reciprocity)",
  average_in_degree = "Average number of incoming ties per node",
  average_out_degree = "Average number of outgoing ties per node",
  most_replied_to = "Node with highest in-degree (most replies received)",
  most_active_replier = "Node with highest out-degree (most replies/reposts sent)",
  num_components = "Number of disconnected components",
  largest_component_size = "Nodes in largest connected component",
  giant_component_pct = "Percentage of nodes in largest component",
  transitivity_val = "Global clustering coefficient (transitivity)",
  avg_local_clustering = "Mean of local clustering coefficients",
  centralization_in = "Degree centralization index (in-degree)"
)

# Convert all metric columns to character BEFORE binding
desc_df_chr <- desc_df %>% mutate(across(-method, as.character))
bsn_summary_df_chr <- bsn_summary_df %>% mutate(across(-method, as.character))
ig_summary_df_chr <- ig_summary_df %>% mutate(across(-method, as.character))

comparison_table <- bind_rows(
  desc_df_chr,
  bsn_summary_df_chr,
  ig_summary_df_chr
) %>%
  pivot_longer(
    cols = -method,
    names_to = "metric",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = method,
    values_from = value
  ) %>%
  mutate(run_date = Sys.Date())
datatable(comparison_table)

# ============================================================================
# SECTION 3: Ploting Network - Components
# ============================================================================
old_par <- par(no.readonly = TRUE) # save current par settings
cat("\n=== Step 3i: Plot the small components of the Graph ===\n")

cat(sprintf(
  "Components: %d (largest: %d = %.1f%%)\n",
  ig_num_components,
  ig_largest_component_size,
  ig_giant_component_pct
))

cat("\n=== Step 3j: Plot the small components using the igraph ===\n")
# Identify small components (all except the largest)
small_ids <- which(ig_comp$csize < max(ig_comp$csize))
small_ids <- sort(small_ids, decreasing = TRUE)
# Work out a sensible grid layout
if (length(small_ids) > 6) {
  n <- 6
} else {
  n <- length(small_ids)
}
nrow <- ceiling(sqrt(n))
ncol <- ceiling(n / nrow)

par(mfrow = c(nrow, ncol), mar = c(1, 1, 2, 1))

for (i in seq(1, n)) {
  verts <- which(ig_comp$membership == i)
  subg <- induced_subgraph(g, verts)

  plot(
    subg,
    main = paste("Component", i, "-", length(verts), "nodes"),
    vertex.size = 12,
    vertex.label.cex = 0.7,
    edge.arrow.size = 0.3
  )
}
par(old_par) # restore original par settings
cat("\n=== Step 3k: Plot the small components using the sna ===\n")
cat(sprintf(
  "Components: %d (largest: %d = %.1f%%)\n",
  bsn_num_components,
  bsn_largest_component_size,
  bsn_largest_component_size_pct
))

# Identify small components (all except the largest)
bsn_small_ids <- which(bsn_comp$csize < max(bsn_comp$csize))

# Work out a sensible grid layout
if (length(bsn_small_ids) > 6) {
  n <- 6
} else {
  n <- length(bsn_small_ids)
}
nrow <- ceiling(sqrt(n))
ncol <- ceiling(n / nrow)

par(mfrow = c(nrow, ncol), mar = c(1, 1, 2, 1))

for (j in seq(1, n)) {
  verts <- which(bsn_comp$membership == j)
  subg <- get.inducedSubgraph(bluSkynet, verts)

  gplot(
    subg,
    gmode = "digraph",
    mode = "fruchtermanreingold",
    main = paste("Component", i, "-", length(verts), "nodes"),
    vertex.cex = 1.2,
    label.cex = 0.7,
    arrowhead.cex = 0.3
  )
}

par(old_par) # restore original par settings

# ============================================================================
# SECTION 4: Computing remaining node Metrics using Statnet
# - igraph is already in the Person table
# ============================================================================

# Betweenness Centrality
# 1. Social networks
# A person with high betweenness:
# - Connects different communities.
# - Has influence because they control who talks to whom.
# - Often plays a gatekeeper or broker role.

# A simple intuition
# - Betweenness: People have to go through me.
# - Closeness: I can get to anyone quickly.
# They often diverge.
# A person can have:
# - High betweenness but low closeness at a bridge between two distant groups
# - High closeness but low betweenness at centrally embedded in a dense cluster

betweenness_vals <- sna::betweenness(
  bluSkynet,
  gmode = "digraph",
  diag = FALSE,
  ignore.eval = FALSE
)
cat("\n=== Step 3l: Betweenness centrality computed ===\n")

# Closeness Centrality (suminvdir accounts for directed graphs)
closeness_vals <- sna::closeness(
  bluSkynet,
  cmode = "suminvdir",
  ignore.eval = FALSE
)
cat("\n=== Step 3m: Closeness centrality computed ===\n")

gc()

# Eigenvector Centrality
eigen_vals <- sna::evcent(bluSkynet, gmode = "digraph", ignore.eval = FALSE)
cat("\n=== Step 3n: Eigenvector centrality computed ===\n")

# Degree metrics (already computed but ensuring consistency)
bsn_ideg <- sna::degree(bluSkynet, cmode = "indegree")
bsn_odeg <- sna::degree(bluSkynet, cmode = "outdegree")
total_degree <- bsn_ideg + bsn_odeg
cat("\n=== Step 3o: Degree metrics (in/out/total) computed ===\n")

### == these are already computed - no need to do it again == ###
# PageRank (weighted by influence)
# pagerank_vals <- page_rank(g_igraph, directed = TRUE)$vector ## not available in SNA
# cat("PageRank computed\n")

# HITS (Hub and Authority scores)
# hits <- hits_scores(g_igraph, scale = TRUE) ## not available in SNA
# hub_score <- hits$hub ## not available in SNA
# authority_score <- hits$authority ## not available in SNA
# cat("HITS authority/hub scores computed\n") ## not available in SNA

# k-Core Decomposition
kcore_vals <- sna::kcores(bluSkynet)
cat("\n=== Step 3p: k-core decomposition computed ===\n")

# align numeric vectors to node order
node_keys <- as.character(V(g)$name)

nodes_with_metrics <- nodes %>%
  dplyr::mutate(
    betweenness = betweenness_vals,
    closeness = closeness_vals,
    eigenvector_centrality = eigen_vals,
    kcore = as.numeric(kcore_vals),
    in_degree = in_degree,
    out_degree = out_degree,
    total_degree = total_degree,
    betweenness_norm = safe_rescale(betweenness),
    closeness_norm = safe_rescale(closeness),
    pagerank_norm = safe_rescale(pagerank),
    authority_norm = safe_rescale(authority_score)
  )

cat(
  "\n=== Step 3q: Node-level metrics compiled into 'nodes_with_metrics' tibble ===\n"
)
cat(sprintf(
  "  Rows: %d nodes | Cols: %d features\n",
  nrow(nodes_with_metrics),
  ncol(nodes_with_metrics)
))

# Top influential nodes (various measures)
cat("\n  Top 10 nodes by different centrality measures:\n")
cat("  --- By PageRank ---\n")
print(
  nodes_with_metrics %>%
    arrange(desc(pagerank)) %>%
    select(name, pagerank, in_degree, out_degree) %>%
    head(10)
)
cat("  --- By Betweenness ---\n")
print(
  nodes_with_metrics %>%
    arrange(desc(betweenness)) %>%
    select(name, betweenness, total_degree) %>%
    head(10)
)
cat("  --- By Authority Score (most reposted) ---\n")
print(
  nodes_with_metrics %>%
    arrange(desc(authority_score)) %>%
    select(name, authority_score, in_degree, reposts_received) %>%
    head(10)
)

# Write node metrics back into node metric table
nodes_sql <- RxSqlServerData(
  table = "dbo.SNA_Node_LevelMetrics",
  connectionString = connStr
)
rxDataStep(nodes_with_metrics, nodes_sql, overwrite = TRUE)


### These will be useful for a notebook === FINISH
cat(
  "\n=== Step 4a: igraph layouts: Computing different layouts for igraph ===\n"
)

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

res_g <- rxReadObject(ds_Graphs, "layout_exec_results")

# assemble nested list: community_layouts[[i]] = list(id, graph,
# layouts = named list(layout -> coord_matrix))
old_par <- par(no.readonly = TRUE)
pal <- viridis::viridis(max(V(g)$kcore, na.rm = TRUE) + 1)

community_layouts <- vector("list", n_comm)
names(community_layouts) <- paste0("Main Graph ", vcount(g), " nodes")

for (i in seq_len(n_comm)) {
  community_layouts[[i]] <- list(
    id = g,
    graph = g,
    layouts = list()
  )

  # Fill layout matrices
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

  # Precompute vertex aesthetics
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

  # ---- NEW: Save one SVG per layout ----
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

    out_file <- paste0("MainGraph_", vcount(subg), "_", layout_name, ".svg")

    save_graph_svg(
      plot_or_expr = function() {
        par(mar = c(1, 1, 2, 1))
        plot.igraph(
          subg,
          layout = coords,
          main = main_title,
          vertex.size = vsize,
          vertex.label.cex = 0.2,
          edge.arrow.size = 0.3,
          vertex.color = vcol
        )
      },
      filename = out_file,
      folder = "images"
    )
  }
}

par(old_par)
# Select top N nodes to label (e.g., top 50 by degree)
deg <- degree(g, mode = "all")

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


# ============================================================================
# SECTION 1.5: Create Subgraphs by Year and Month
# ============================================================================
cat(
  "\n=== Step 3c.5: Creating subgraphs filtered by year and month of edgeStarts ===\n"
)

# Assuming edgeStarts is a Date or can be converted to Date
edge_dates <- E(g)$edgeStarts
if (!inherits(edge_dates, "Date")) {
  edge_dates <- as.Date(edge_dates)
}

# Extract year-month as string "YYYY-MM"
year_month <- format(edge_dates, "%Y-%m")
unique_ym <- unique(year_month)
unique_ym <- sort(unique_ym) # Sort for consistency

subgraphs_igraph <- list()
subgraphs_statnet <- list()

for (ym in unique_ym) {
  eids <- which(year_month == ym)
  if (length(eids) > 0) {
    # Create igraph subgraph
    sub_g <- subgraph.edges(g, eids)
    subgraphs_igraph[[ym]] <- sub_g

    # Convert to statnet network
    sub_net <- asNetwork(sub_g)
    subgraphs_statnet[[ym]] <- sub_net

    cat(
      "Created subgraph for",
      ym,
      "with",
      vcount(sub_g),
      "nodes and",
      ecount(sub_g),
      "edges\n"
    )

    main_title <- paste0(
      "Sub Graph ",
      vcount(sub_g),
      " nodes\n(",
      ym,
      ")"
    )

    # Precompute vertex aesthetics
    vsize <- if (!is.null(V(sub_g)$pagerank)) {
      V(sub_g)$pagerank + 1
    } else {
      rep(6, vcount(subg))
    }
    vcol <- if (!is.null(V(sub_g)$kcore)) {
      pal[as.integer(V(sub_g)$kcore) + 1]
    } else {
      "steelblue"
    }

    out_file <- paste0("SubGraph_", vcount(sub_g), "_", ym, ".svg")

    save_graph_svg(
      plot_or_expr = function() {
        par(mar = c(1, 1, 2, 1))
        plot.igraph(
          sub_g,
          layout = layout_nicely(sub_g, dim = 2),
          main = main_title,
          vertex.size = vsize,
          vertex.label.cex = 0.2,
          edge.arrow.size = 0.3,
          vertex.color = vcol
        )
      },
      filename = out_file,
      folder = "images"
    )

    # Optionally save each subgraph
    rxWriteObject(
      ds_Graphs,
      paste0("subgraph_igraph_", ym),
      sub_g,
      overwrite = TRUE
    )
    rxWriteObject(
      ds_Graphs,
      paste0("subgraph_statnet_", ym),
      sub_net,
      overwrite = TRUE
    )
  }
}

# Save the lists of subgraphs
rxWriteObject(
  ds_Graphs,
  "subgraphs_igraph_by_ym",
  subgraphs_igraph,
  overwrite = TRUE
)
rxWriteObject(
  ds_Graphs,
  "subgraphs_statnet_by_ym",
  subgraphs_statnet,
  overwrite = TRUE
)

cat("Subgraphs created and saved.\n")

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
  transition_states(edge_dates)
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

# ============================================================================
# SECTION 6: SAVE RESULTS
# ============================================================================

# Summary report
cat("\n" %+% strrep("=", 70) %+% "\n")
cat("METRICS COMPUTATION SUMMARY\n")
cat(strrep("=", 70) %+% "\n")
cat(sprintf(
  "Network Size:        %d nodes, %d edges\n",
  ig_summary_df$network_size,
  ecount(g)
))
cat(sprintf(
  "Density:             %.4f (%.2f%% of possible edges)\n",
  ig_summary_df$density,
  ig_summary_df$density * 100
))
cat(sprintf(
  "Clustering:          Global: %.4f | Local mean: %.4f\n",
  ig_summary_df$transitivity_val,
  ig_summary_df$avg_local_clustering
))
comm_louvain <- igraph::cluster_louvain(as_undirected(g))
modularity_louvain <- modularity(g, comm_louvain$membership)
cat(sprintf(
  "Communities:         %d (Louvain, modularity = %.4f)\n",
  length(unique(nodes$community)),
  modularity_louvain
))
cat(sprintf(
  "Reciprocity:         %.4f (edgewise)\n",
  ig_summary_df$reciprocity_default
))
cat(sprintf(
  "Path Length:         Avg: %.2f | Diameter: %f\n",
  mean_distance(g, directed = TRUE),
  bsn_diameter
))
cat(sprintf(
  "Centralization:      %.4f (in-degree based)\n",
  ig_summary_df$centralization_in
))
cat(sprintf(
  "Components:          %d (giant component: %.1f%%)\n",
  ig_num_components,
  ig_giant_component_pct
))
cat(strrep("=", 70), "\n")

cat("\n=== Metrics computation complete. Results saved to graphs/ ===\n")
cat("  Use nodes_with_metrics for per-user analysis\n")
cat("  Use network_metrics for global statistics\n")
cat("  Use community_stats to understand cluster structure\n\n")

gc()
