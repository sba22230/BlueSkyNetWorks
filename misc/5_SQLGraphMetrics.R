# ============================================================================
# BlueSkyNetWorks: SQL-based Graph Metrics Integration
# ============================================================================
# This script shows how to compute network metrics using SQL Server graph tables
# and retrieve them in R for visualization and further analysis
# ============================================================================

# source("0_functions.R")

cat("\n=== SQL Server Graph Metrics Integration ===\n")

# ============================================================================
# SETUP: Connect to MSSQL
# ============================================================================

# Create ODBC connection to SQL Server
conn <- DBI::dbConnect(
  odbc::odbc(),
  Driver = "ODBC Driver 17 for SQL Server",
  Server = Sys.getenv("SQL_SERVER", "localhost"),
  Database = "BlueSkyNet",
  Trusted_Connection = "yes"
)

cat("✓ Connected to SQL Server\n")

# ============================================================================
# SECTION 1: EXECUTE SQL METRICS COMPUTATION
# ============================================================================
# Run the stored procedures to populate metrics in Person table

cat("\n[1/3] Computing metrics in SQL Server...\n")

# Execute closeness centrality computation
tryCatch(
  {
    DBI::dbExecute(conn, "EXEC sp_ComputeCloseness")
    cat("  ✓ Closeness centrality computed\n")
  },
  error = function(e) {
    cat("  ⚠ Closeness computation warning:", e$message, "\n")
  }
)

# Execute clustering coefficient computation
tryCatch(
  {
    DBI::dbExecute(conn, "EXEC sp_ComputeClusteringCoefficient")
    cat("  ✓ Clustering coefficient computed\n")
  },
  error = function(e) {
    cat("  ⚠ Clustering computation warning:", e$message, "\n")
  }
)

# Execute influence score computation
tryCatch(
  {
    DBI::dbExecute(conn, "EXEC sp_ComputeInfluenceScore")
    cat("  ✓ Influence score computed\n")
  },
  error = function(e) {
    cat("  ⚠ Influence score computation warning:", e$message, "\n")
  }
)

# ============================================================================
# SECTION 2: RETRIEVE METRICS FROM SQL
# ============================================================================

cat("\n[2/3] Retrieving computed metrics from SQL Server...\n")

# Query all person metrics from SQL
sql_metrics <- DBI::dbGetQuery(
  conn,
  "SELECT 
    handle,
    posts_authored,
    reposts_made,
    reposts_received,
    COALESCE(closeness, 0) AS closeness,
    COALESCE(local_clustering, 0) AS local_clustering,
    COALESCE(influence_score, 0) AS influence_score
  FROM dbo.Person
  WHERE posts_authored > 0 OR reposts_made > 0 OR reposts_received > 0"
)

sql_metrics <- as_tibble(sql_metrics)
cat(sprintf(
  "  ✓ Retrieved %d nodes with computed metrics\n",
  nrow(sql_metrics)
))

# Get network-level statistics
network_stats <- DBI::dbGetQuery(
  conn,
  "EXEC sp_ComputeNetworkMetrics @network_stats = NULL"
)

if (!is.null(network_stats)) {
  cat("  ✓ Network statistics computed\n")
}

# ============================================================================
# SECTION 3: COMPARE SQL METRICS WITH R METRICS
# ============================================================================

cat("\n[3/3] Comparing SQL vs. R metrics...\n")

# Load the previously computed R metrics (from 4_ComputeMetrics.R)
# Assuming nodes_with_metrics is already in memory
# If not, load from parquet:
# nodes_with_metrics <- arrow::read_parquet("Report/_site/graphs/speirgorm_nodes_with_metrics.parquet")

# Merge SQL metrics with R metrics for comparison
comparison <- sql_metrics %>%
  left_join(
    nodes_with_metrics %>%
      select(name, pagerank, betweenness, in_degree, out_degree),
    by = c("handle" = "name")
  ) %>%
  filter(!is.na(pagerank)) %>% # Keep only matched rows
  mutate(
    # Normalize SQL influence score to 0-1 for comparison with pagerank
    influence_normalized = influence_score / 100,
    pagerank_normalized = safe_rescale(pagerank)
  )

cat("\n  Sample comparison (top 10 by SQL influence):\n")
print(
  comparison %>%
    arrange(desc(influence_score)) %>%
    select(
      handle,
      influence_score,
      influence_normalized,
      pagerank_normalized,
      closeness,
      local_clustering
    ) %>%
    head(10)
)

# Correlation between SQL influence and R PageRank
cor_influence_pagerank <- cor(
  comparison$influence_normalized,
  comparison$pagerank_normalized,
  use = "complete.obs"
)
cat(sprintf(
  "\n  Correlation (SQL influence vs. R PageRank): %.4f\n",
  cor_influence_pagerank
))

# ============================================================================
# SECTION 4: USE SQL SUBGRAPH FOR VISUALIZATION
# ============================================================================
# Filter to high-influence users for cleaner network visualization

cat("\n[BONUS] Extracting high-influence subgraph from SQL...\n")

# Define influence threshold (e.g., top 10% of influence)
influence_threshold <- quantile(sql_metrics$influence_score, 0.9, na.rm = TRUE)

cat(sprintf(
  "  Influence threshold (90th percentile): %.2f\n",
  influence_threshold
))

# Query high-influence subgraph from SQL
subgraph_edges <- DBI::dbGetQuery(
  conn,
  sprintf(
    "SELECT * FROM fn_GetInfluenceSubgraph(%d)",
    as.integer(influence_threshold)
  )
)

subgraph_edges <- as_tibble(subgraph_edges)
cat(sprintf(
  "  ✓ Extracted subgraph: %d edges among high-influence nodes\n",
  nrow(subgraph_edges)
))

# Get nodes in subgraph
subgraph_nodes <- sql_metrics %>%
  filter(influence_score >= influence_threshold)

cat(sprintf("  ✓ Subgraph contains %d nodes\n", nrow(subgraph_nodes)))

# ============================================================================
# SECTION 5: VISUALIZE USING SUBGRAPH
# ============================================================================

if (nrow(subgraph_edges) > 0 && nrow(subgraph_nodes) > 0) {
  cat("\n[VISUALIZATION] Creating graph from SQL subgraph...\n")

  # Convert to igraph format
  g_subgraph <- igraph::graph_from_data_frame(
    d = subgraph_edges %>%
      select(from_handle, to_handle) %>%
      rename(from = from_handle, to = to_handle),
    directed = TRUE,
    vertices = subgraph_nodes %>%
      select(handle, influence_score) %>%
      rename(name = handle)
  )

  # Compute layout
  coords <- layout_with_drl(g_subgraph)
  V(g_subgraph)$x <- coords[, 1]
  V(g_subgraph)$y <- coords[, 2]

  # Plot subgraph with influence-based sizing
  p <- ggraph(
    g_subgraph,
    layout = "manual",
    x = V(g_subgraph)$x,
    y = V(g_subgraph)$y
  ) +
    geom_edge_parallel0(
      aes(width = 0.5),
      edge_alpha = 0.3,
      arrow = arrow(
        angle = 20,
        length = unit(0.15, "inches"),
        ends = "last",
        type = "closed"
      )
    ) +
    geom_node_point(
      aes(fill = influence_score, size = influence_score),
      colour = "#000000",
      shape = 21,
      stroke = 0.5
    ) +
    geom_node_text(aes(label = name), repel = TRUE, size = 3) +
    scale_fill_gradient(low = "#87CEFF", high = "#27408B") +
    scale_size(range = c(2, 8)) +
    theme_graph() +
    labs(
      title = sprintf(
        "High-Influence Subgraph (influence >= %.1f)",
        influence_threshold
      )
    )

  print(p)
  cat("  ✓ Subgraph visualization complete\n")
}

# ============================================================================
# SECTION 6: EXPORT METRICS TO PARQUET
# ============================================================================

cat("\n[EXPORT] Saving metrics to disk...\n")

# Save SQL metrics for future use
arrow::write_parquet(
  sql_metrics,
  "Report/_site/graphs/speirgorm_metrics_from_sql.parquet"
)
cat(
  "  ✓ SQL metrics saved to Report/_site/graphs/speirgorm_metrics_from_sql.parquet\n"
)

# Save comparison for analysis
arrow::write_parquet(
  comparison,
  "Report/_site/graphs/speirgorm_metrics_comparison.parquet"
)
cat(
  "  ✓ Comparison saved to Report/_site/graphs/speirgorm_metrics_comparison.parquet\n"
)

# Save subgraph edges and nodes
arrow::write_parquet(
  subgraph_edges,
  "Report/_site/graphs/speirgorm_subgraph_edges.parquet"
)
arrow::write_parquet(
  subgraph_nodes,
  "Report/_site/graphs/speirgorm_subgraph_nodes.parquet"
)
cat("  ✓ Subgraph saved to Report/_site/graphs/speirgorm_subgraph_*.parquet\n")

# ============================================================================
# CLEANUP
# ============================================================================

DBI::dbDisconnect(conn)
cat("\n✓ SQL Server connection closed\n")

cat("\n=== Summary ===\n")
cat(sprintf("Total nodes: %d\n", nrow(sql_metrics)))
cat(sprintf("High-influence nodes: %d\n", nrow(subgraph_nodes)))
cat(sprintf("Subgraph edges: %d\n", nrow(subgraph_edges)))
cat("\nMetrics available in 'sql_metrics' tibble:\n")
print(colnames(sql_metrics))
