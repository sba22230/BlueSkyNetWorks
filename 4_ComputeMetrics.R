# BlueSkyNetWorks: Comprehensive Network Metrics Computation
# Prerequisites: 3_StatnetAnalysis.R must be sourced first
# This script computes node-level and network-level statistics

# Load libraries and functions (assumes 0_functions.
# R was already sourced in 3_StatnetAnalysis.R)
source("0_functions.R")

# Source 3_StatnetAnalysis.R if not already loaded
# (comment out if already running in same session)
source("2_BluseSkyNetworking.R")
source("3_StatnetAnalysis.R")

cat("\n=== BlueSkyNetWorks: Computing Network Metrics ===\n")

# ============================================================================
# SECTION 1: NODE-LEVEL METRICS
# ============================================================================

cat("\n[1/4] Computing node-level centrality metrics...\n")

# Betweenness Centrality
betweenness_vals <- betweenness(
  bluSkynet,
  gmode = "digraph",
  diag = FALSE,
  ignore.eval = FALSE
)
cat("  ✓ Betweenness centrality computed\n")

# Closeness Centrality (suminvdir accounts for directed graphs)
closeness_vals <- closeness(bluSkynet, cmode = "suminvdir", ignore.eval = FALSE)
cat("  ✓ Closeness centrality computed\n")

# Eigenvector Centrality
eigen_vals <- evcent(bluSkynet, gmode = "digraph", ignore.eval = FALSE)
cat("  ✓ Eigenvector centrality computed\n")

# Convert to igraph for additional metrics
g_igraph <- g3

# Degree metrics (already computed but ensuring consistency)
in_degree <- degree(bluSkynet, cmode = "indegree")
out_degree <- degree(bluSkynet, cmode = "outdegree")
total_degree <- in_degree + out_degree
cat("  ✓ Degree metrics (in/out/total) computed\n")

# PageRank (weighted by influence)
pagerank_vals <- page_rank(g_igraph, directed = TRUE)$vector
cat("  ✓ PageRank computed\n")

# HITS (Hub and Authority scores)
hits <- hits_scores(g_igraph, scale = TRUE)
hub_score <- hits$hub
authority_score <- hits$authority
cat("  ✓ HITS authority/hub scores computed\n")

# Local Clustering Coefficient (for directed graphs, variants exist)
local_clustering <- transitivity(g_igraph, type = "local", isolates = "zero")
cat("  ✓ Local clustering coefficient computed\n")

# k-Core Decomposition
kcore_vals <- coreness(g_igraph)
cat("  ✓ k-core decomposition computed\n")

# Betweenness in igraph (verify against statnet)
betweenness_ig <- igraph::betweenness(
  g_igraph,
  V(g_igraph),
  directed = TRUE,
  weights = NULL
)
cat("  ✓ Cross-verified betweenness (igraph)\n")
# align numeric vectors to node order
node_keys <- as.character(nodes$name)
betw_vec <- betweenness_vals[match(node_keys, names(betweenness_vals))]
clos_vec <- closeness_vals[match(node_keys, names(closeness_vals))]
pagerank_vec <- pagerank_vals[match(node_keys, names(pagerank_vals))]
auth_vec <- authority_score[match(node_keys, names(authority_score))]
in_vec <- in_degree[match(node_keys, names(in_degree))]
out_vec <- out_degree[match(node_keys, names(out_degree))]
kcore_vec <- kcore_vals[match(node_keys, names(kcore_vals))]
local_clust_vec <- local_clustering[match(node_keys, names(local_clustering))]


nodes_with_metrics <- nodes %>%
  dplyr::mutate(
    betweenness = betw_vec,
    closeness = clos_vec,
    eigenvector_centrality = eigen_vals[match(node_keys, names(eigen_vals))],
    pagerank = pagerank_vec,
    hub_score = hub_score[match(node_keys, names(hub_score))],
    authority_score = auth_vec,
    local_clustering = local_clust_vec,
    kcore = kcore_vec,
    in_degree = in_vec,
    out_degree = out_vec,
    total_degree = (in_vec + out_vec),
    betweenness_norm = safe_rescale(betweenness),
    closeness_norm = safe_rescale(closeness),
    pagerank_norm = safe_rescale(pagerank),
    authority_norm = safe_rescale(authority_score)
  )

cat("\n✓ Node-level metrics compiled into 'nodes_with_metrics' tibble\n")
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

# ============================================================================
# SECTION 2: NETWORK-LEVEL METRICS
# ============================================================================

cat("\n[2/4] Computing network-level metrics...\n")

# Density
density_val <- gden(bluSkynet)
cat(sprintf("  ✓ Density: %.4f\n", density_val))

# Reciprocity (edgewise = mutual reposts)
reciprocity_val <- grecip(bluSkynet, measure = "edgewise")
cat(sprintf("  ✓ Reciprocity (edgewise): %.4f\n", reciprocity_val))

# Reciprocity dyadic
reciprocity_dyadic <- grecip(bluSkynet, measure = "dyadic.nonnull")

cat(sprintf("  ✓ Reciprocity (dyadic): %.4f\n", reciprocity_dyadic))

# Network size and edges
network_size <- network.size(bluSkynet)
edge_count <- network.edgecount(bluSkynet)
dyad_count <- network.dyadcount(bluSkynet)
cat(sprintf(
  "  ✓ Network size: %d nodes, %d edges, %d dyads\n",
  network_size,
  edge_count,
  dyad_count
))

# Diameter (longest shortest path)
diameter_val <- diameter(g_igraph, directed = TRUE, unconnected = TRUE)
cat(sprintf("  ✓ Diameter: %d\n", diameter_val))

# Average path length
avg_path_length <- mean_distance(g_igraph, directed = TRUE)
cat(sprintf("  ✓ Average path length: %.2f\n", avg_path_length))

# Connected components
num_components <- igraph::components(g_igraph)$no
largest_component_size <- max(igraph::components(g_igraph)$csize)
giant_component_pct <- (largest_component_size / network_size) * 100
cat(sprintf(
  "  ✓ Components: %d (largest: %d = %.1f%%)\n",
  num_components,
  largest_component_size,
  giant_component_pct
))

# Transitivity (global clustering coefficient)
transitivity_val <- transitivity(g_igraph, type = "global")
cat(sprintf(
  "  ✓ Global clustering coefficient (transitivity): %.4f\n",
  transitivity_val
))

# Average local clustering
avg_local_clustering <- mean(local_clustering, na.rm = TRUE)
cat(sprintf("  ✓ Average local clustering: %.4f\n", avg_local_clustering))

# Centralization (degree based)
in_degree_vec <- igraph::degree(g_igraph, mode = "in")
centralization_in <- (sum(max(in_degree_vec) - in_degree_vec)) /
  ((network_size - 1) * (network_size - 2))
cat(sprintf("  ✓ Centralization (in-degree): %.4f\n", centralization_in))

# Triad Census (for directed graphs)
triad_census_vals <- triad_census(g_igraph)
cat(sprintf("  ✓ Triad census computed (16 types of triads)\n"))

# Create network metrics summary table
network_metrics <- tibble(
  metric_name = c(
    "density",
    "reciprocity_edgewise",
    "reciprocity_dyadic",
    "network_size",
    "edge_count",
    "dyad_count",
    "diameter",
    "avg_path_length",
    "num_components",
    "giant_component_size",
    "giant_component_pct",
    "global_clustering",
    "avg_local_clustering",
    "centralization_indegree"
  ),
  value = c(
    density_val,
    reciprocity_val,
    reciprocity_dyadic,
    network_size,
    edge_count,
    dyad_count,
    diameter_val,
    avg_path_length,
    num_components,
    largest_component_size,
    giant_component_pct,
    transitivity_val,
    avg_local_clustering,
    centralization_in
  ),
  description = c(
    "Proportion of possible edges present",
    "Proportion of mutual reposts (edgewise)",
    "Proportion of dyads with reciprocated edges",
    "Number of nodes (users) in network",
    "Number of directed edges (reposts)",
    "Total number of possible dyadic pairs",
    "Longest shortest path between any two nodes",
    "Average shortest path across all node pairs",
    "Number of disconnected components",
    "Nodes in largest connected component",
    "Percentage of nodes in largest component",
    "Global clustering coefficient (transitivity)",
    "Mean of local clustering coefficients",
    "Degree centralization index (in-degree)"
  )
)

cat("\n✓ Network metrics compiled into 'network_metrics' tibble\n")
cat(sprintf("  %d network-level metrics computed\n", nrow(network_metrics)))

# ============================================================================
# SECTION 3: COMMUNITY STRUCTURE ANALYSIS
# ============================================================================

cat("\n[3/4] Computing community structure...\n")
# Louvain community detection (already computed in 3_StatnetAnalysis.R)
# Re-compute if necessary
comm_louvain <- igraph::cluster_louvain(as_undirected(g2))
num_communities <- length(unique(comm_louvain$membership))
modularity_louvain <- modularity(g_igraph, comm_louvain$membership)
cat(sprintf(
  "  ✓ Louvain detection: %d communities, modularity = %.4f\n",
  num_communities,
  modularity_louvain
))


cat("\n Extract Community Subgraphs \n")
# Assuming you have your community detection results
communities <- comm_louvain # or your preferred method
membership <- membership(communities)

# Get top 10 communities by size
comm_sizes <- sort(table(membership), decreasing = TRUE)
top_10_ids <- as.numeric(names(comm_sizes[1:10]))

# Create list of subgraphs
community_graphs <- lapply(top_10_ids, function(id) {
  nodes <- which(membership == id)
  induced_subgraph(g2, nodes)
})

# Name them for easy reference
names(community_graphs) <- paste0("Community_", top_10_ids)
cat(
  "\n Analyze Internal Structure
For each community subgraph, examine: \n"
)
# For a single community
comm_graph <- community_graphs[[1]]

# Density
edge_density(comm_graph)

# Key nodes within community
igraph::betweenness(comm_graph)
igraph::closeness(comm_graph)
igraph::degree(comm_graph)

# Clustering coefficient
transitivity(comm_graph, type = "local")

# Diameter and average path length
diameter(comm_graph)
mean_distance(comm_graph)

# Sub-communities within
sub_communities <- cluster_louvain(as_undirected(comm_graph))
# Batch Analysis
# Analyze all top 10 at once
community_metrics <- data.frame(
  community = names(community_graphs),
  size = sapply(community_graphs, vcount),
  density = sapply(community_graphs, edge_density),
  diameter = sapply(community_graphs, diameter),
  avg_path = sapply(community_graphs, mean_distance)
)

# Label propagation (can detect overlapping communities)
comm_labelprop <- cluster_label_prop(g_igraph)
num_communities_lp <- length(unique(comm_labelprop$membership))
modularity_labelprop <- modularity(g_igraph, comm_labelprop$membership)
cat(sprintf(
  "  ✓ Label propagation: %d communities, modularity = %.4f\n",
  num_communities_lp,
  modularity_labelprop
))

# Fast greedy (for directed, may treat as undirected)
comm_fastgreedy <- cluster_fast_greedy(as_undirected(g_igraph))
num_communities_fg <- length(unique(comm_fastgreedy$membership))
modularity_fastgreedy <- modularity(
  as_undirected(g_igraph),
  comm_fastgreedy$membership
)
cat(sprintf(
  "  ✓ Fast greedy: %d communities, modularity = %.4f\n",
  num_communities_fg,
  modularity_fastgreedy
))

# Add best community assignment to nodes
best_comm <- comm_louvain$membership
nodes_with_metrics <- nodes_with_metrics %>%
  dplyr::mutate(
    community = best_comm[name],
    modularity = modularity_louvain
  )

# Community statistics (size, internal density, external connections)
community_stats <- nodes_with_metrics %>%
  group_by(community) %>%
  summarise(
    community_size = n(),
    # rough estimate
    internal_edges = sum(in_degree[name %in% name], na.rm = TRUE) / 2,
    avg_internal_degree = mean(in_degree + out_degree, na.rm = TRUE),
    avg_pagerank = mean(pagerank, na.rm = TRUE),
    avg_authority = mean(authority_score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(community_size))

cat("\n✓ Community analysis complete:\n")
print(community_stats)

# ============================================================================
# SECTION 4: SAVE RESULTS
# ============================================================================

cat("\n[4/4] Saving results...\n")

# Save node metrics
arrow::write_parquet(nodes_with_metrics, "graphs/nodes_with_metrics.parquet")
readr::write_csv(nodes_with_metrics, "graphs/nodes_with_metrics.csv")
cat(
  "✓ Saved: graphs/nodes_with_metrics.parquet | graphs/nodes_with_metrics.csv\n"
)

# Save network metrics
arrow::write_parquet(network_metrics, "graphs/network_metrics.parquet")
readr::write_csv(network_metrics, "graphs/network_metrics.csv")
cat("  ✓ Saved: graphs/network_metrics.parquet | graphs/network_metrics.csv\n")

# Save community stats
arrow::write_parquet(community_stats, "graphs/community_stats.parquet")
readr::write_csv(community_stats, "graphs/community_stats.csv")
cat("  ✓ Saved: graphs/community_stats.parquet | graphs/community_stats.csv\n")

# Save triad census results
triad_census_df <- tibble(
  triad_type = 0:15,
  count = as.integer(triad_census_vals),
  description = c(
    "003: no edges",
    "012: single directed edge",
    "102: single undirected edge",
    "021D: two asymmetric edges",
    "021U: single undirected + one directed",
    "021C: path of length 2",
    "111D: three asymmetric edges",
    "111U: two undirected + one directed",
    "030T: transitive triple",
    "030C: cyclic triple",
    "201: bidirectional edge + isolated",
    "120D: one mutual + one asymmetric",
    "120U: one mutual + two directed",
    "120C: one mutual + path",
    "210: mutual edge + two directed",
    "300: all mutual edges"
  )
)
arrow::write_parquet(triad_census_df, "graphs/triad_census.parquet")
readr::write_csv(triad_census_df, "graphs/triad_census.csv")
cat("  ✓ Saved: graphs/triad_census.parquet | graphs/triad_census.csv\n")

# Summary report
cat("\n" %+% strrep("=", 70) %+% "\n")
cat("METRICS COMPUTATION SUMMARY\n")
cat(strrep("=", 70) %+% "\n")
cat(sprintf(
  "Network Size:        %d nodes, %d edges\n",
  network_size,
  edge_count
))
cat(sprintf(
  "Density:             %.4f (%.2f%% of possible edges)\n",
  density_val,
  density_val * 100
))
cat(sprintf(
  "Clustering:          Global: %.4f | Local mean: %.4f\n",
  transitivity_val,
  avg_local_clustering
))
cat(sprintf(
  "Communities:         %d (Louvain, modularity = %.4f)\n",
  num_communities,
  modularity_louvain
))
cat(sprintf("Reciprocity:         %.4f (edgewise)\n", reciprocity_val))
cat(sprintf(
  "Path Length:         Avg: %.2f | Diameter: %d\n",
  avg_path_length,
  diameter_val
))
cat(sprintf("Centralization:      %.4f (in-degree based)\n", centralization_in))
cat(sprintf(
  "Components:          %d (giant component: %.1f%%)\n",
  num_components,
  giant_component_pct
))
cat(strrep("=", 70) %+% "\n")

cat("\n✓ Metrics computation complete. Results saved to graphs/\n")
cat("  Use nodes_with_metrics for per-user analysis\n")
cat("  Use network_metrics for global statistics\n")
cat("  Use community_stats to understand cluster structure\n\n")
