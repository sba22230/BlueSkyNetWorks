source("0_functions.R")

cat("\n=== Step 3a: Build igraph object and plot basic network ===\n")
set.seed(22230)
num_posts <- nrow(read_parquet("docs/graphs/speirgorm_edges.parquet"))
if (!exists("num_posts") || is.null(num_posts)) {
  num_posts <- 5000
}
# Step 3: Build igraph object and plot basic network
g <- get_graph_data(num_posts)
cat("Graph summary:\n")
print(summary(g))
nodes <- igraph::as_data_frame(g, what = "vertices")
edges <- igraph::as_data_frame(g, what = "edges")
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
cat("\n=== Step 3b: Build out the metrics using igraph... ===\n")

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
  neighbours_average = mean(neighbors(
    g,
    diameter(g, directed = TRUE, weights = NA),
    mode = "all"
  )),
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
  "\n=== Step 3c: BlueSkyNetWorks: Computing Network Metrics using StatNet ===\n"
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

cat("\n=== Step 3d: Plotting degree distributions and scatter plot ===\n")

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
  folder = "docs/images",
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
  folder = "images",
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
  folder = "images",
  filename = "outdegree_dist.svg"
)

cat("\n=== Step 3e: Compute network metrics using StatNet ===\n")
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
bsn_Global_clustering <- gtrans(
  bluSkynet,
  mode = "digraph",
  measure = "weak",
  use.adjacency = FALSE
)
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
  neighbours_average = mean(neighbors(g, bsn_diameter, mode = "all")),
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

cat("\n=== Step 3f: Show both Network Metrics in one table ===\n")

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
# SECTION 3: Ploting Network - Components, islands of nodes in the overall network
# ============================================================================
# Ensure output directory exists before saving SVGs
if (!dir.exists("images")) {
  dir.create("images", recursive = TRUE)
}

cat("\n=== Step 3g: Plot the small components of the Graph ===\n")

cat(sprintf(
  "Components: %d (largest: %d = %.1f%%)\n",
  ig_num_components,
  ig_largest_component_size,
  ig_giant_component_pct
))

cat("\n=== Step 3h: Plot the small components using the igraph ===\n")
# Identify small components (all except the largest)
small_ids <- which(ig_comp$csize < max(ig_comp$csize))
small_ids <- sort(small_ids, decreasing = TRUE)
# Work out a sensible grid layout
if (length(small_ids) > 6) {
  n <- 6
} else {
  n <- length(small_ids)
}

save_graph_svg(
  plot_or_expr = function() {
    old_par <- par(no.readonly = TRUE)
    nrow <- ceiling(sqrt(n))
    ncol <- ceiling(n / nrow)
    par(mfrow = c(nrow, ncol), mar = c(1, 1, 2, 1))

    for (idx in seq_len(n)) {
      comp_id <- small_ids[idx]
      verts <- which(ig_comp$membership == comp_id)
      subg <- induced_subgraph(g, verts)

      plot(
        subg,
        main = paste("Component", comp_id, "-", length(verts), "nodes"),
        vertex.size = 12,
        vertex.label.cex = 0.7,
        edge.arrow.size = 0.3
      )
    }

    par(old_par)
  },
  folder = "images",
  filename = "igraph_small_components.svg"
)

cat("\n=== Step 3i: Plot the small components using the sna ===\n")
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

save_graph_svg(
  plot_or_expr = function() {
    old_par <- par(no.readonly = TRUE)
    nrow <- ceiling(sqrt(n))
    ncol <- ceiling(n / nrow)
    par(mfrow = c(nrow, ncol), mar = c(1, 1, 2, 1))

    for (idx in seq_len(n)) {
      comp_id <- bsn_small_ids[idx]
      verts <- which(bsn_comp$membership == comp_id)
      subg <- get.inducedSubgraph(bluSkynet, verts)

      gplot(
        subg,
        gmode = "digraph",
        mode = "fruchtermanreingold",
        main = paste("Component", comp_id, "-", length(verts), "nodes"),
        vertex.cex = 1.2,
        label.cex = 0.7,
        arrowhead.cex = 0.3
      )
    }

    par(old_par)
  },
  folder = "images",
  filename = "sna_small_components.svg"
)

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
cat("\n=== Step 3j: Betweenness centrality computed ===\n")

# Closeness Centrality (suminvdir accounts for directed graphs)
closeness_vals <- sna::closeness(
  bluSkynet,
  cmode = "suminvdir",
  ignore.eval = FALSE
)
cat("\n=== Step 3k: Closeness centrality computed ===\n")

gc()

# Eigenvector Centrality
eigen_vals <- sna::evcent(bluSkynet, gmode = "digraph", ignore.eval = FALSE)
cat("\n=== 3l: Eigenvector centrality computed ===\n")

# Degree metrics (already computed but ensuring consistency)
bsn_ideg <- sna::degree(bluSkynet, cmode = "indegree")
bsn_odeg <- sna::degree(bluSkynet, cmode = "outdegree")
total_degree <- bsn_ideg + bsn_odeg
cat("\n=== Step 3m: Degree metrics (in/out/total) computed ===\n")

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
cat("\n=== Step 3n: k-core decomposition computed ===\n")

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
  "\n=== Step 3o: Node-level metrics compiled into 'nodes_with_metrics' tibble ===\n"
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
rxSetComputeContext("localpar")
cat(
  "\n=== Step 3p: igraph layouts: Computing different layouts for igraph ===\n"
)

layout_types <- c(
  "drl_fast",
  #,"drl"
  #,"fr"
  #"graphopt",
  #,"lgl"
  #,"kk"
  #,"mds"
  "nicely"
  #,"tree"
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

# ── 1. Initialise entries ────────────────────────────────────────────────────
for (i in seq_len(n_comm)) {
  community_layouts[[i]] <- list(id = g, graph = g, layouts = list())
}

# ── 2. Fill layout matrices (hoisted out of the community loop) ───────────────
for (k in seq_along(res_g)) {
  comm_i <- graph_idx_rep[k]
  lt <- as.character(attr(res_g[[k]], "layout_type"))
  coords_mat <- as.matrix(res_g[[k]][, c("x", "y")])
  community_layouts[[comm_i]]$layouts[[lt]] <- coords_mat
}

# ── 3. Flatten to a list of independent render tasks ─────────────────────────
render_tasks <- list()
for (i in seq_len(n_comm)) {
  entry <- community_layouts[[i]]
  subg <- entry$graph
  layouts_list <- entry$layouts
  if (length(layouts_list) == 0L) {
    next
  }

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

  for (k in seq_along(layouts_list)) {
    render_tasks[[length(render_tasks) + 1L]] <- list(
      subg = subg,
      coords = layouts_list[[k]],
      layout_name = names(layouts_list)[k],
      vsize = vsize,
      vcol = vcol
    )
  }
}

# ── 4. Worker: renders and saves one SVG ─────────────────────────────────────
render_one_graph <- function(task) {
  library(igraph)
  subg <- task$subg
  coords <- task$coords
  layout_name <- task$layout_name
  vsize <- task$vsize
  vcol <- task$vcol

  main_title <- paste0(
    "Main Graph ",
    vcount(subg),
    " nodes\n(",
    layout_name,
    ")"
  )
  out_file <- paste0("MainGraph_", layout_name, ".svg")

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
  invisible(out_file)
}

# ── 5. Execute all renders in parallel ───────────────────────────────────────

rxExec(
  render_one_graph,
  task = rxElemArg(render_tasks),
  execObjects = "save_graph_svg",
  packagesToLoad = c("igraph", "viridis")
)


par(old_par)
# Select top N nodes to label (e.g., top 50 by degree)
deg <- degree(g, mode = "all")

top_nodes <- names(sort(deg, decreasing = TRUE))[1:50]
# Use a base-R network plot so the same SVG writer can be reused for
# the network view and any companion panels.
coords_drl <- res_g[[1]]
# Convert layout to numeric matrix
xy <- as.matrix(coords_drl[, c("x", "y")])

save_graph_svg(
  plot_or_expr = function() {
    par(mar = c(1, 1, 2, 1))
    g_top <- induced_subgraph(g, vids = top_nodes)
    xy_top <- xy[match(top_nodes, V(g)$name), ]
    plot.igraph(
      g_top,
      layout = xy_top,
      main = "Speirgorm network (top degree nodes)",
      vertex.size = 4,
      vertex.label.cex = 0.25,
      edge.arrow.size = 0.3,
      vertex.color = adjustcolor("tomato", alpha.f = 0.6)
    )
  },
  filename = "gtn_TopNodes_Speirgorm_Network.svg",
  folder = "docs/images"
)
# rxWriteObject(
#   ds_Graphs,
#   "gtn_Graph - ggraph - layout_with_drl",
#   gtn,
#   overwrite = TRUE
# )

# --- end of one visualisation

# ============================================================================
# SECTION 1.5: Create Subgraphs by Year and Month (Parallel via rxExec)
# ============================================================================
cat(
  "\n=== Step 3q: Creating subgraphs filtered by year and month of edgeStarts ===\n"
)
pal <- viridis::viridis(max(V(g)$kcore, na.rm = TRUE) + 1)
# Assuming edgeStarts is a Date or can be converted to Date
edge_dates <- E(g)$edgeStarts
if (!inherits(edge_dates, "Date")) {
  edge_dates <- as.Date(edge_dates)
}

# Extract year-month as string "YYYY-MM"
year_month <- format(edge_dates, "%Y-%m")
unique_ym <- sort(unique(year_month))

# Worker function: processes one year-month slice
process_ym <- function(
  ym,
  year_month,
  g,
  pal,
  ds_Graphs,
  save_graph_svg,
  save_network_svg,
  plot_word_comparison_date,
  render_word_comparison_date
) {
  library(dplyr)
  library(stringr)
  library(tidytext)
  library(lubridate)
  library(igraph)
  library(wordcloud)
  library(sna)
  library(network)
  library(RColorBrewer)

  # date preparation - build the sub graphs
  eids <- which(year_month == ym)
  if (length(eids) == 0) {
    return(NULL)
  }

  nrc <- get_sentiments("nrc")
  sub_g <- igraph::subgraph.edges(g, eids, delete.vertices = TRUE)
  sub_net <- asNetwork(sub_g)
  bsn_ideg <- sna::degree(sub_net, cmode = "indegree")
  bsn_odeg <- sna::degree(sub_net, cmode = "outdegree")
  # Extract edge-level posts and attach community membership from the graph object
  nodes <- igraph::as_data_frame(sub_g, what = "vertices")
  edges <- igraph::as_data_frame(sub_g, what = "edges")

  posts <- data.frame(
    name = edges$from,
    text = edges$text,
    created_at = lubridate::ymd(edges$created_at),
    document = seq_len(nrow(edges))
  ) |>
    merge(nodes[, c("name", "community")], by = "name", all.x = TRUE)

  # Tokenise, clean, and remove stop words.
  # Returns one row per (document, word) with community membership preserved.
  build_tidy_posts <- function(df) {
    # "ireland" / "irish" are corpus-wide ubiquitous (this is an Irish Bluesky
    # community) and are not discriminative for LDA; treat them like stop words.
    custom_stops <- tibble::tibble(
      word = c(
        "speirgorm",
        "spéirgorm",
        "speirghorm",
        "spéirghorm",
        "ireland",
        "irish"
      )
    )

    df |>
      dplyr::filter(!stringr::str_detect(text, "^RT")) |>
      dplyr::mutate(
        text = stringr::str_to_lower(text),
        text = stringr::str_replace_all(text, "https?://\\S+|www\\.\\S+", " "),
        text = stringr::str_replace_all(text, "[#@]", " "),
        text = stringr::str_replace_all(text, "&amp;|&lt;|&gt;", " ")
      ) |>
      tidytext::unnest_tokens(word, text, token = "words") |>
      dplyr::filter(
        !word %in% stop_words$word,
        !word %in% stringr::str_remove_all(stop_words$word, "'"),
        !word %in% custom_stops$word,
        nchar(word) > 1
      ) |>
      dplyr::mutate(document = as.integer(document))
  }

  tidy_posts <- build_tidy_posts(posts)

  sentiment_analysis <- tidy_posts |>
    inner_join(nrc, by = "word", relationship = "many-to-many")

  sentiment_summary <- sentiment_analysis |>
    count(sentiment, sort = TRUE)

  counts <- sentiment_summary$n
  names(counts) <- sentiment_summary$sentiment
  counts <- counts[order(counts)]
  global_freq <- tidy_posts |>
    count(word, sort = TRUE)

  make_wordcloud_base <- function(freq_df) {
    wordcloud(
      words = freq_df$word,
      freq = freq_df$n,
      max.words = 200,
      random.order = FALSE,
      rot.per = 0.35,
      colors = RColorBrewer::brewer.pal(8, "Dark2")
    )
  }

  # build a grid layout for the plots: 3 rows, 2 columns
  grid_matrix <- matrix(c(1, 2, 1, 3, 1, 4), nrow = 3, byrow = TRUE)

  # plot creation
  cat(
    "Created subgraph for",
    ym,
    "with",
    vcount(sub_g),
    "nodes and",
    ecount(sub_g),
    "edges\n"
  )

  main_title <- paste0("Sub Graph ", vcount(sub_g), " nodes\n(", ym, ")")

  # Precompute vertex aesthetics (bug fix: subg -> sub_g)
  vsize <- if (!is.null(V(sub_g)$pagerank)) {
    V(sub_g)$pagerank + 1
  } else {
    rep(6, vcount(sub_g))
  }
  vcol <- if (!is.null(V(sub_g)$kcore)) {
    pal[as.integer(V(sub_g)$kcore) + 1]
  } else {
    "steelblue"
  }

  out_file <- paste0("images/SubGraph_", ym, ".svg")
  svg(out_file, width = 20, height = 12)
  layout(grid_matrix)

  # Plot the subgraph using igraph
  plot.igraph(
    sub_g,
    layout = layout_nicely(sub_g, dim = 2),
    main = main_title,
    vertex.size = vsize,
    vertex.label.cex = 0.9,
    edge.arrow.size = 0.3,
    vertex.color = vcol
  )

  # Plot 2 the sentiment barplot
  barplot(
    counts,
    horiz = TRUE,
    col = grDevices::rainbow(length(counts), alpha = 0.7),
    border = NA,
    main = "Sentiment Analysis Using NRC Lexicon",
    xlab = "Count",
    las = 1
  )

  # Plot 3 the word cloud
  make_wordcloud_base(global_freq) # row 2 col 2

  # Plot 4 the degree scatter plot
  plot(
    bsn_ideg,
    bsn_odeg,
    type = "n",
    xlab = "Re-posts received",
    ylab = "Re-posts made"
  )
  abline(0, 1, lty = 3)
  text(
    jitter(bsn_ideg, factor = 0.5, amount = 0.5),
    jitter(bsn_odeg, factor = 0.5, amount = 0.5),
    labels = network.vertex.names(sub_net),
    cex = 0.9,
    col = 2
  )

  dev.off()

  # Each worker writes its own keys — no contention
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
  list(ym = ym, sub_g = sub_g, sub_net = sub_net)
}


# Switch to parallel context (adjust numCoresToUse as needed)
rxSetComputeContext(RxLocalParallel())

results <- rxExec(
  FUN = process_ym,
  ym = rxElemArg(unique_ym),
  year_month = year_month,
  g = g,
  pal = pal,
  ds_Graphs = ds_Graphs,
  save_graph_svg = save_graph_svg,
  save_network_svg = save_network_svg,
  plot_word_comparison_date = plot_word_comparison_date,
  render_word_comparison_date = render_word_comparison_date,
  execObjects = c(
    "save_graph_svg",
    "save_network_svg",
    "plot_word_comparison_date",
    "render_word_comparison_date",
    "subgraph_from_edges",
    "ds_Graphs"
  ),

  packagesToLoad = c(
    "dplyr",
    "igraph",
    "ggplot2",
    "ggrepel",
    "intergraph",
    "lubridate",
    "magrittr",
    "network",
    "stringr",
    "tidytext"
  )
)

# Restore original compute context
rxSetComputeContext(origComputeContext)

# Filter out any NULL results and collect into named lists
results <- Filter(Negate(is.null), results)
subgraphs_igraph <- setNames(
  lapply(results, `[[`, "sub_g"),
  sapply(results, `[[`, "ym")
)
subgraphs_statnet <- setNames(
  lapply(results, `[[`, "sub_net"),
  sapply(results, `[[`, "ym")
)

# Save the aggregated lists
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
library(gganimate)
coords_graphopt <- res_g[[which(
  vapply(
    res_g,
    function(x) as.character(attr(x, "layout_type")),
    character(1)
  ) ==
    "nicely"
)]]
if (length(coords_graphopt) != 3) {
  stop("Unable to find a single 'graphopt' layout result in res_g")
}
coords_graphopt <- as.matrix(coords_graphopt[, c("x", "y")])
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
  transition_states(edge_dates) +
  theme_void()

write_graph(
  g,
  "docs/graphs/g bluesky Speirgorm Network RepostsMade vs RepostsReceived.graphml",
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
cat(
  "\n=== Step 3r: Print out the summary metrics ===\n"
)
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

cat(
  "\n=== Metrics computation complete. Results saved to graphs/ ===\n"
)
cat("  Use nodes_with_metrics for per-user analysis\n")
cat("  Use network_metrics for global statistics\n")
cat("  Use community_stats to understand cluster structure\n\n")

tobermvd <- c(
  'bsn_comp',
  'bsn_dyadcensus',
  'bsn_summary_df',
  'bsn_summary_df_chr',
  'community_layouts',
  'coords_drl',
  'coords_mat',
  'dist_matrix',
  'entry',
  'ig_cen',
  'ig_comp',
  'ig_summary_df',
  'ig_summary_df_chr'
)
rm(list = tobermvd)
gc()
