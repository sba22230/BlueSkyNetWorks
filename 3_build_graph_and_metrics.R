# source("0_functions.R")

# Source 3_StatnetAnalysis.R if not already loaded
# (comment out if already running in same session)
# source("2_BluseSkyNetworking.R")
# source("3_StatnetAnalysis.R")

# plan(multisession, workers = wrkrs)  ## to be moved closer to its use

cat("\n=== Step 1: Build the initial graph... ===\n")

# Step 1: Build edge list (who reposted whom)
edges <- read_parquet("graphs/speirgorm_edges.parquet")
cat("Edges count:", nrow(edges), "\n")

# Debug: inspect edges
print(head(edges))

# remove NAs
#edges[is.na(edges)] <- "no text here"

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


# ============================================================================
# SECTION 1: Network-LEVEL METRICS
# ============================================================================
ig_cen <- igraph::dyad_census(g)
ig_summary_df <- tibble(
  # DATEADD(DAY, CAST([analysis_timestamp] AS int), '1970-01-01')  AS analysis_timestamp
  analysis_timestamp = Sys.Date(),
  method = "igraph",
  network_size = vcount(g),
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
  average_in_degree = mean(nodes$in_degree),
  average_out_degree = mean(nodes$out_degree),
  most_replied_to = nodes$name[which.max(nodes$in_degree)],
  most_active_replier = nodes$name[which.max(nodes$out_degree)]
)

str(ig_summary_df)
# Write metrics back into network metric table
ig_sql <- RxSqlServerData(
  table = "dbo.NetworkLevelMetrics",
  connectionString = connStr,
  colInfo = list('analysis_timestamp' = list(type = "Date"))
)
rxDataStep(ig_summary_df, ig_sql,  append = 'rows')

cat("\n=== BlueSkyNetWorks: Computing Network Metrics using StatNet ===\n")

# Convert igraph to statnet
library(network)
bluSkynet <- asNetwork(g)

cat("\n[1/4] Computing node-level centrality metrics for bluSkynet..\n")
bsN_degree <- sna::degree(bluSkynet)
bsN_ideg <- sna::degree(bluSkynet, cmode = "indegree")
bsN_odeg <- sna::degree(bluSkynet, cmode = "outdegree")
bsN_dyadcensus <- sna::dyad.census(bluSkynet)
bsN_dyadcount <- sum(
  bsN_dyadcensus[[1]],
  bsN_dyadcensus[[2]],
  bsN_dyadcensus[[3]]
)
bsN_cen <- sna::centralization(bluSkynet, sna::degree, cmode = "indegree")
bsN_ceneig <- sna::centralization(bluSkynet, sna::evcent)
dist_matrix <- geodist(bluSkynet)$gdist
gc()
bsN_diameter <- max(dist_matrix[is.finite(dist_matrix)])

bsN_summary_df <- tibble(
  #DATEADD(DAY, CAST([analysis_timestamp] AS int), '1970-01-01')  AS analysis_timestamp
  analysis_timestamp = Sys.Date(), 
  method = "network/sna",
  network_size = network.size(bluSkynet),
  edge_count = network.edgecount(bluSkynet),
  dyad_count = bsN_dyadcount,
  density = gden(bluSkynet),
  mutual_pairs = bsN_dyadcensus[[1]],
  asymetric_pairs = bsN_dyadcensus[[2]],
  isolated_nodes = bsN_dyadcensus[[3]],
  diameter = bsN_diameter,
  avg_path_length = mean(dist_matrix[is.finite(dist_matrix)]),
  neighbours_average = mean(neighbors(g, V(g), mode = "all")),
  reciprocity_default = grecip(bluSkynet, measure = "edgewise"),
  reciprocity_ratio = grecip(bluSkynet, measure = "dyadic.nonnull"),
  average_in_degree = mean(bsN_ideg),
  average_out_degree = mean(bsN_odeg),
  most_replied_to = network.vertex.names(bluSkynet)[which.max(bsN_ideg)],
  most_active_replier = network.vertex.names(bluSkynet)[which.max(bsN_odeg)]
)

str(bsN_summary_df)
# Write metrics back into network metric table
ig_sql <- RxSqlServerData(
  table = "dbo.NetworkLevelMetrics",
  connectionString = connStr,
  colInfo = list('analysis_timestamp' = list(type = "Date"))
)
rxDataStep(bsN_summary_df, ig_sql,  append = 'rows')

comparison_table <- bind_rows(bsN_summary_df, ig_summary_df) %>%
  mutate(across(-method, as.character)) %>% 
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

# Connected components
num_components <- igraph::components(g)$no
largest_component_size <- max(igraph::components(g)$csize)
giant_component_pct <- (largest_component_size / bsN_summary_df$network_size) * 100
cat(sprintf(
  "  ✓ Components: %d (largest: %d = %.1f%%)\n",
  num_components,
  largest_component_size,
  giant_component_pct
))

# Transitivity (global clustering coefficient)
transitivity_val <- transitivity(g, type = "global")
cat(sprintf(
  "  ✓ Global clustering coefficient (transitivity): %.4f\n",
  transitivity_val
))

# Average local clustering
avg_local_clustering <- mean(local_clustering, na.rm = TRUE)
cat(sprintf("  ✓ Average local clustering: %.4f\n", avg_local_clustering))
# Centralization (degree based)
in_degree_vec <- igraph::degree(g, mode = "in")
centralization_in <- (sum(max(in_degree_vec) - in_degree_vec)) /
  ((bsN_summary_df$network_size - 1) * (bsN_summary_df$network_size - 2))
cat(sprintf("  ✓ Centralization (in-degree): %.4f\n", centralization_in))

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
    bsN_summary_df$density,
    bsN_summary_df$reciprocity_default,
    bsN_summary_df$reciprocity_ratio,
    bsN_summary_df$network_size,
    bsN_summary_df$edge_count,
    bsN_summary_df$dyad_count,
    bsN_summary_df$diameter,
    bsN_summary_df$avg_path_length,
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

## what is this code - these seem to be node level metrics calculated by SNA and network 

# Betweenness Centrality
betweenness_vals <- sna::betweenness(
  bluSkynet,
  gmode = "digraph",
  diag = FALSE,
  ignore.eval = FALSE
)
cat("  ✓ Betweenness centrality computed\n")

# Closeness Centrality (suminvdir accounts for directed graphs)
closeness_vals <- sna::closeness(
  bluSkynet,
  cmode = "suminvdir",
  ignore.eval = FALSE
)
cat("  ✓ Closeness centrality computed\n")

# Eigenvector Centrality
eigen_vals <- sna::evcent(bluSkynet, gmode = "digraph", ignore.eval = FALSE)
cat("  ✓ Eigenvector centrality computed\n")

# Convert to igraph for additional metrics
# g_igraph <- g3

# Degree metrics (already computed but ensuring consistency)
bsN_ideg <- sna::degree(bluSkynet, cmode = "indegree")
bsN_odeg <- sna::degree(bluSkynet, cmode = "outdegree")
total_degree <- bsN_ideg + bsN_odeg
cat("  ✓ Degree metrics (in/out/total) computed\n")

### == these are already computed - no need to do it again == ###
# PageRank (weighted by influence)
# pagerank_vals <- page_rank(g_igraph, directed = TRUE)$vector ## not available in SNA
# cat("  ✓ PageRank computed\n")

# HITS (Hub and Authority scores)
# hits <- hits_scores(g_igraph, scale = TRUE) ## not available in SNA
# hub_score <- hits$hub ## not available in SNA
# authority_score <- hits$authority ## not available in SNA
# cat("  ✓ HITS authority/hub scores computed\n") ## not available in SNA

# Local Clustering Coefficient (for directed graphs, variants exist)
local_clustering <- gtrans(bluSkynet, mode = "digraph", measure = "weak")

cat("  ✓ Local clustering coefficient computed\n")

# k-Core Decomposition
kcore_vals <- sna::kcores(bluSkynet)
cat("  ✓ k-core decomposition computed\n")


# align numeric vectors to node order
node_keys <- as.character(nodes$name)

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




# Network size and edges

dyad_count <- network.dyadcount(bluSkynet)







# Triad Census (for directed graphs)
triad_census_vals <- triad_census(g)
cat(sprintf("  ✓ Triad census computed (16 types of triads)\n"))



# ============================================================================
# SECTION 3: COMMUNITY STRUCTURE ANALYSIS
# ============================================================================

cat("\n[3/4] Computing community structure...\n")
# Louvain community detection (already computed in 3_StatnetAnalysis.R)
# Re-compute if necessary
comm_louvain <- igraph::cluster_louvain(as_undirected(g))
num_communities <- length(unique(comm_louvain$membership))
modularity_louvain <- modularity(g, comm_louvain$membership)
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
  induced_subgraph(g, nodes)
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
comm_labelprop <- cluster_label_prop(g)
num_communities_lp <- length(unique(comm_labelprop$membership))
modularity_labelprop <- modularity(g, comm_labelprop$membership)
cat(sprintf(
  "  ✓ Label propagation: %d communities, modularity = %.4f\n",
  num_communities_lp,
  modularity_labelprop
))

# Fast greedy (for directed, may treat as undirected)
comm_fastgreedy <- cluster_fast_greedy(as_undirected(g))
num_communities_fg <- length(unique(comm_fastgreedy$membership))
modularity_fastgreedy <- modularity(
  as_undirected(g),
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
  "Path Length:         Avg: %.2f | Diameter: %f\n",
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
