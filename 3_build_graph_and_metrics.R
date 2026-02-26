# source("0_functions.R")

# Source 3_StatnetAnalysis.R if not already loaded
# (comment out if already running in same session)
# source("2_BluseSkyNetworking.R")
# source("3_StatnetAnalysis.R")

# plan(multisession, workers = wrkrs)  ## to be moved closer to its use
# ============================================================================
# SECTION 1: Create the Graph
# ============================================================================
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
cat("\n=== Step 2a: Build out the metrics using igraph... ===\n")

ig_network_size = vcount(g)
ig_comp <- igraph::components(g)
# Connected components
ig_num_components <- ig_comp$no
ig_largest_component_size <- max(ig_comp$csize)
ig_giant_component_pct <- (ig_largest_component_size / ig_network_size) *
  100
# Average local clustering
ig_avg_local_clustering <- mean(nodes$local_clustering, na.rm = TRUE)
# Centralization (degree based)

ig_centralization_in <- (sum(max(nodes$in_degree) - nodes$in_degree)) /
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
  average_in_degree = mean(nodes$in_degree),
  average_out_degree = mean(nodes$out_degree),
  most_replied_to = nodes$name[which.max(nodes$in_degree)],
  most_active_replier = nodes$name[which.max(nodes$out_degree)],
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
rxDataStep(ig_summary_df, ig_sql, append = 'rows')

cat(
  "\n=== Step 2b: BlueSkyNetWorks: Computing Network Metrics using StatNet ===\n"
)

# Convert igraph to statnet
library(network)
bluSkynet <- asNetwork(g)

bsN_network_size = network.size(bluSkynet)
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
bsN_comp <- component.dist(bluSkynet, connected = "weak")
bsN_num_components <- length(bsN_comp$csize)
bsN_largest_component_size <- max(bsN_comp$csize)
bsN_largest_component_size_pct <- (bsN_largest_component_size /
  bsN_network_size) *
  100
# Local Clustering Coefficient (for directed graphs, variants exist)
bsN_local_clustering <- ig_avg_local_clustering # Placeholder: SNA's clustering is more complex for directed graphs
bsN_Global_clustering <- gtrans(bluSkynet, mode = "digraph", measure = "weak")
# Centralization (degree based)
bsN_centralization_in <- (sum(max(bsN_ideg) - bsN_ideg)) /
  ((bsN_network_size - 1) * (bsN_network_size - 2))
bsN_summary_df <- tibble(
  #DATEADD(DAY, CAST([analysis_timestamp] AS int), '1970-01-01')  AS analysis_timestamp
  analysis_timestamp = Sys.Date(),
  method = "network/sna",
  network_size = bsN_network_size,
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
  most_active_replier = network.vertex.names(bluSkynet)[which.max(bsN_odeg)],
  num_components = bsN_num_components,
  largest_component_size = bsN_largest_component_size,
  giant_component_pct = bsN_largest_component_size_pct,
  transitivity_val = bsN_Global_clustering,
  avg_local_clustering = bsN_local_clustering,
  centralization_in = bsN_centralization_in
)

str(bsN_summary_df)
# Write metrics back into network metric table
ig_sql <- RxSqlServerData(
  table = "dbo.NetworkLevelMetrics",
  connectionString = connStr,
  colInfo = list("analysis_timestamp" = list(type = "Date"))
)
rxDataStep(bsN_summary_df, ig_sql, append = 'rows')

cat("\n=== Step 2c: Show both Network Metrics in one table ===\n")

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
bsN_summary_df_chr <- bsN_summary_df %>% mutate(across(-method, as.character))
ig_summary_df_chr <- ig_summary_df %>% mutate(across(-method, as.character))

comparison_table <- bind_rows(
  desc_df_chr,
  bsN_summary_df_chr,
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
cat("\n=== Step 3: Plot the small components of the Graph ===\n")


cat(sprintf(
  "  ✓ Components: %d (largest: %d = %.1f%%)\n",
  ig_num_components,
  ig_largest_component_size,
  ig_giant_component_pct
))

cat("\n=== Step 3a: Plot the small components using the igraph ===\n")
# Identify small components (all except the largest)
small_ids <- which(ig_comp$csize < max(ig_comp$csize))

# Work out a sensible grid layout
n <- length(small_ids)
nrow <- ceiling(sqrt(n))
ncol <- ceiling(n / nrow)

par(mfrow = c(nrow, ncol), mar = c(1, 1, 2, 1))

for (i in small_ids) {
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
cat("\n=== Step 3b: Plot the small components using the sna ===\n")
cat(sprintf(
  "  ✓ Components: %d (largest: %d = %.1f%%)\n",
  bsN_num_components,
  bsN_largest_component_size,
  bsN_largest_component_size_pct
))

# Identify small components (all except the largest)
bsN_small_ids <- which(bsN_comp$csize < max(bsN_comp$csize))

# Work out a sensible grid layout
n <- length(bsN_small_ids)
nrow <- ceiling(sqrt(n))
ncol <- ceiling(n / nrow)

par(mfrow = c(nrow, ncol), mar = c(1, 1, 2, 1))

for (i in bsN_small_ids) {
  verts <- which(bsN_comp$membership == i)
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
# SECTION 4: Computing node Metrics using Statnet
# - igraph is already in the Person table
# ============================================================================

# Betweenness Centrality
# 1. Social networks
# A person with high betweenness:
# - Connects different communities.
# - Has influence because they control who talks to whom.
# - Often plays a “gatekeeper” or “broker” role.

# A simple intuition
# - Betweenness: “People have to go through me.”
# - Closeness: “I can get to anyone quickly.”
# They often diverge.
# A person can have:
# - High betweenness but low closeness → a bridge between two distant groups
# - High closeness but low betweenness → centrally embedded in a dense cluster

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

gc()

# Eigenvector Centrality
eigen_vals <- sna::evcent(bluSkynet, gmode = "digraph", ignore.eval = FALSE)
cat("  ✓ Eigenvector centrality computed\n")

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

# Write node metrics back into node metric table
nodes_sql <- RxSqlServerData(
  table = "dbo.SNA_Node_LevelMetrics",
  connectionString = connStr
)
rxDataStep(nodes_with_metrics, nodes_sql, overwrite = TRUE)


### These will be useful for a notebook === FINISH

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
top_10_ids <- as.numeric(names(comm_sizes[1:12]))

community_graphs <- lapply(top_10_ids, function(id) {
  nodes <- which(membership == id)
  induced_subgraph(g, nodes)
})

# Work out a sensible grid layout
n <- length(top_10_ids)
nrow <- ceiling(sqrt(n))
ncol <- ceiling(n / nrow)

par(mfrow = c(nrow, ncol), mar = c(1, 1, 2, 1))
pal <- viridis::viridis(max(V(g)$kcore) + 1)

for (i in seq(1, n)) {
  subg <- community_graphs[[i]]
  res <- rxExec(
    layout_exec,
    graph = subg,
    layout_type = rxElemArg(c(
      "drl",
      "drl_fast",
      "fr",
      "graphopt",
      "lgl",
      "kk",
      "mds",
      "nicely",
      "tree"
    )),
    execObjects = c("connStr", "layout_exec")
  )

  m <- length(res)
  for (j in seq(1, m)) {
    layout_type <- base::attr(res[[j]], "layout_type")
    coords <- res[[j]]
    coords <- as.matrix(coords[, c("x", "y")])
    save_graph_svg(
      plot_or_expr = function() {
        plot.igraph(
          subg,
          layout = coords,
          main = paste(
            "Sub Graph",
            i,
            "-",
            vcount(subg),
            "nodes. Layout: ",
            layout_type
          ),
          vertex.size = V(subg)$pagerank + 1,
          vertex.label.cex = 0.2,
          edge.arrow.size = 0.3,
          vertex.color = pal[V(subg)$kcore + 1] # colour by k-core
        )
      },
      filename = "degree_scatter.svg"
    )
  }
}


# Name them for easy reference
names(community_graphs) <- paste0("Community_", top_10_ids)
cat(
  "\n Analyze Internal Structure
For each community subgraph, examine: \n"
)
# For a single community
for (i in length(community_graphs)) {
  comm_graph <- community_graphs[[i]]

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
}


par(old_par) # restore original par settings

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
