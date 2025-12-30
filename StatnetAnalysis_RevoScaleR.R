# BlueSkyNet_RevoScaleR.R
library(DBI)
library(odbc)
library(dplyr)
library(lubridate)
library(igraph)
library(ggraph)
library(RevoScaleR) # RevoScaleR provides RxSqlServerData, rxDataStep, etc.


# ---------------------------
# Step 6: Prepare RevoScaleR compute context (RxInSqlServer) and run statnet analysis remotely
# - The remote function will:
#   * read adjacency from graph (MATCH or join on Reposted)
#   * build a 'network' object (statnet) on the server
#   * compute network statistics (degree, components, centrality, optional ERGM)
#   * persist summary tables back to SQL Server (node_metrics, component_membership, network_summary)
# ---------------------------

# Create compute context (RxInSqlServer). If not available, fallback to RxLocalParallel.
computeCtx <- tryCatch({
  RxInSqlServer(connectionString = connStr, shareDir = shareDir)
}, error = function(e) {
  message("RxInSqlServer not available; falling back to RxLocalParallel.")
  RxLocalParallel()
})
rxSetComputeContext(computeCtx)

# Remote function to run on server via rxExec
statnet_remote <- function(person_table = "Person",
                           reposted_table = "Reposted",
                           node_metrics_table = "node_metrics",
                           component_table = "component_membership",
                           network_summary_table = "network_summary") {
  # Load packages on the remote compute node
  library(RevoScaleR)
  library(network)   # statnet network object
  library(sna)       # network analysis helpers
  # If you need ergm or other statnet packages, load them here (must be installed on server)
  library(ergm)
  
  # Build adjacency by selecting from graph tables using MATCH
  connStrLocal <- Sys.getenv("R_RX_SQLSERVER_CONNECTIONSTRING")
  if (is.null(connStrLocal) || connStrLocal == "") {
    # fallback to connStr from closure if present
    connStrLocal <- connStr
  }
  
  # Use MATCH to get edges with names
  edges_query <- paste0("
    SELECT f.name AS [from], t.name AS [to], r.repost_uri, r.created_at
    FROM ", person_table, " AS f, ", reposted_table, " AS r, ", person_table, " AS t
    WHERE MATCH(f-(r)->t)
  ")
  
  edges_src <- RxSqlServerData(sqlQuery = edges_query, connectionString = connStrLocal, stringsAsFactors = FALSE)
  edges_df <- rxImport(edges_src)
  
  # Import node attributes from Person
  nodes_src <- RxSqlServerData(table = person_table, connectionString = connStrLocal, stringsAsFactors = FALSE)
  nodes_df <- rxImport(nodes_src)
  
  # Build network object (statnet)
  # network::network() expects a data.frame of edges or adjacency matrix
  # We'll create a two-column edgelist and pass vertex.attr
  el <- edges_df[, c("from", "to")]
  # Ensure factors/characters are consistent
  el[] <- lapply(el, as.character)
  
  # Create network with directed = TRUE
  net <- network::network(el, directed = TRUE, vertex.attr = nodes_df, loops = TRUE, multiple = TRUE, ignore.eval = FALSE, names.eval = NULL)
  
  # Compute node-level metrics (server-side)
  deg_in  <- sna::degree(net, gmode = "digraph", cmode = "indegree")
  deg_out <- sna::degree(net, gmode = "digraph", cmode = "outdegree")
  # Component membership (weak components)
  comps <- sna::components.sna(as.sociomatrix.sna(net), connected = "weak")$membership
  
  # Centrality measures (example: betweenness)
  btw <- tryCatch(sna::betweenness(net), error = function(e) rep(NA, network::network.size(net)))
  
  # Build node metrics data.frame
  node_metrics <- data.frame(
    name = network::get.vertex.attribute(net, "name"),
    display_name = network::get.vertex.attribute(net, "display_name"),
    repost_count = as.integer(network::get.vertex.attribute(net, "repost_count")),
    indegree = as.integer(deg_in),
    outdegree = as.integer(deg_out),
    betweenness = as.numeric(btw),
    component = as.integer(comps),
    stringsAsFactors = FALSE
  )
  
  # Persist node_metrics to SQL Server
  node_metrics_out <- RxSqlServerData(table = node_metrics_table, connectionString = connStrLocal)
  rxDataStep(inData = node_metrics, outFile = node_metrics_out, overwrite = TRUE)
  
  # Persist component membership (list nodes per component)
  component_df <- node_metrics[, c("name", "component")]
  component_out <- RxSqlServerData(table = component_table, connectionString = connStrLocal)
  rxDataStep(inData = component_df, outFile = component_out, overwrite = TRUE)
  
  # Compute network-level summary
  net_summary <- data.frame(
    nodes = network::network.size(net),
    edges = network::network.edgecount(net),
    density = network::network.density(net),
    mean_indegree = mean(deg_in, na.rm = TRUE),
    mean_outdegree = mean(deg_out, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  
  net_summary_out <- RxSqlServerData(table = network_summary_table, connectionString = connStrLocal)
  rxDataStep(inData = net_summary, outFile = net_summary_out, overwrite = TRUE)
  
  # Optionally run ERGM (commented out; requires ergm installed and may be heavy)
  # ergm_fit <- ergm::ergm(net ~ edges + gwdegree(0.5, fixed=TRUE))
  # Save ergm summary as text table if desired
  
  # Return a small summary to the client
  list(nodes = nrow(nodes_df), edges = nrow(edges_df), node_metrics_table = node_metrics_table)
}


# Execute the remote function once on the server compute context
# Ensure connStr is available in the remote environment via execObjects
res <- rxExec(statnet_remote,
              person_table = "Person",
              reposted_table = "Reposted",
              node_metrics_table = "node_metrics",
              component_table = "component_membership",
              network_summary_table = "network_summary",
              execObjects = c("connStr"),
              packagesToLoad = c("network", "sna", "RevoScaleR"))

print(res)

# ---------------------------
# Step 6: Pull small result sets back into R for plotting / igraph
#   Only pull what you need (e.g., top nodes and their edges)
# ---------------------------
# Pull top nodes into R
top_nodes_local <- rxImport(inData = top_nodes_table)

# Pull edges that involve top nodes (server-side filter via SQL query)
edges_for_top_query <- "
SELECT e.*
FROM edges e
INNER JOIN top_nodes t
  ON e.[from] = t.name OR e.[to] = t.name
"
edges_for_top_source <- RxSqlServerData(
  sqlQuery = edges_for_top_query,
  connectionString = connStr,
  stringsAsFactors = FALSE
)
edges_for_top_local <- rxImport(inData = edges_for_top_source)

# ---------------------------
# Step 7: Build igraph locally (small subset) and plot
# ---------------------------
g_local <- graph_from_data_frame(
  d = edges_for_top_local,
  vertices = top_nodes_local,
  directed = TRUE
)

# Example layout and plot (you can reuse your ggraph code)
coords <- layout_with_drl(g_local)
V(g_local)$x <- coords[, 1]
V(g_local)$y <- coords[, 2]

library(ggraph)
ggraph(g_local, layout = "manual") +
  geom_edge_link(alpha = 0.3) +
  geom_node_point(aes(size = repost_count, color = repost_count)) +
  geom_node_text(aes(label = display_name), repel = TRUE) +
  scale_size_continuous(range = c(3, 12)) +
  scale_color_gradient(low = "lightblue", high = "red") +
  theme_void()

# ---------------------------
# Step 8: Quick server-side diagnostics (examples)
# ---------------------------
# Count rows in edges (server-side)
edges_count_query <- "SELECT COUNT(*) AS edges_count FROM edges"
edges_count <- rxImport(
  inData = RxSqlServerData(
    sqlQuery = edges_count_query,
    connectionString = connStr
  )
)
print(edges_count)

# Count nodes
nodes_count_query <- "SELECT COUNT(*) AS nodes_count FROM nodes"
nodes_count <- rxImport(
  inData = RxSqlServerData(
    sqlQuery = nodes_count_query,
    connectionString = connStr
  )
)
print(nodes_count)

# BlueSkyNet_rxExec_graph.R
library(RevoScaleR)
library(igraph)   # must be installed on server compute context
# ggraph not needed on server; only compute coords server-side


if (use_trusted_connection) {
  connStr <- paste0("Driver={SQL Server};Server=", sql_server,
                    ";Database=", database, ";Trusted_Connection=Yes;")
} else {
  connStr <- paste0("Driver={SQL Server};Server=", sql_server,
                    ";Database=", database,
                    ";Uid=", sql_user, ";Pwd=", sql_password, ";")
}


# Create the RxInSqlServer compute context
# If RxInSqlServer is not available, replace with RxLocalParallel() as fallback
computeCtx <- tryCatch({
  RxInSqlServer(connectionString = connStr, shareDir = shareDir)
}, error = function(e) {
  message("RxInSqlServer not available; falling back to RxLocalParallel.")
  RxLocalParallel()
})

rxSetComputeContext(computeCtx)

# --- Function to run on the compute node(s) ---
# This function will be serialized and executed remotely by rxExec.
 build_graph_and_save_coords <- function(edges_table_name = "edges",
                                        nodes_table_name = "top_nodes",
                                        coords_table_name = "node_coords") {
  # Load packages inside remote function
  library(RevoScaleR)
  library(igraph)
  
  # Build RxSqlServerData sources to read server-side tables
  connStrLocal <- Sys.getenv("R_RX_SQLSERVER_CONNECTIONSTRING")
  if (is.null(connStrLocal) || connStrLocal == "") {
    # Fallback to using the connection string passed via closure (rxExec will serialize it)
    connStrLocal <- connStr
  }
  
  edges_src <- RxSqlServerData(table = edges_table_name, connectionString = connStrLocal,
                               stringsAsFactors = FALSE)
  nodes_src <- RxSqlServerData(table = nodes_table_name, connectionString = connStrLocal,
                               stringsAsFactors = FALSE)
  
  # Import the (filtered) tables into the compute process memory (should be on server)
  edges_df <- rxImport(edges_src)
  nodes_df <- rxImport(nodes_src)
  
  # Build igraph and compute layout
  g <- graph_from_data_frame(d = edges_df, vertices = nodes_df, directed = TRUE)
  
  # Use DRL layout (heavy). You can choose other layouts if desired.
  coords <- layout_with_drl(g)
  
  # Create a data.frame of coordinates keyed by vertex name
  coords_df <- data.frame(
    name = V(g)$name,
    x = coords[,1],
    y = coords[,2],
    stringsAsFactors = FALSE
  )
  
  # Optionally include node attributes (e.g., repost_count, display_name) if present
  if (!is.null(V(g)$repost_count)) {
    coords_df$repost_count <- as.integer(V(g)$repost_count)
  }
  if (!is.null(V(g)$display_name)) {
    coords_df$display_name <- as.character(V(g)$display_name)
  }
  
  # Persist coords_df back to SQL Server as coords_table_name
  coords_out <- RxSqlServerData(table = coords_table_name, connectionString = connStrLocal)
  rxDataStep(inData = coords_df, outFile = coords_out, overwrite = TRUE)
  
  # Return a small summary (this will be returned to the client)
  list(n_nodes = vcount(g), n_edges = ecount(g), coords_table = coords_table_name)
}

# --- Execute remotely with rxExec ---
# Pass any objects needed by the remote function via execObjects or closures.
# packagesToLoad ensures igraph is loaded on the remote worker.
res <- rxExec(build_graph_and_save_coords,
              edges_table_name = "edges",
              nodes_table_name = "top_nodes",
              coords_table_name = "node_coords",
              execObjects = c("connStr"),   # ensure connStr is available on remote side
              packagesToLoad = c("igraph", "RevoScaleR"))

print(res)

# --- Bring the coordinates back to R for plotting ---
coords_sql <- RxSqlServerData(table = "node_coords", connectionString = connStr)
coords_local <- rxImport(coords_sql)

# Join coords to top_nodes_local if you imported top_nodes earlier
# Example plotting locally using ggraph (lightweight)
library(igraph)
library(ggraph)
library(ggplot2)

# Reconstruct a small igraph locally using edges_for_top_local and top_nodes_local
# (Assumes you have edges_for_top_local and top_nodes_local imported earlier)
g_local <- graph_from_data_frame(d = edges_for_top_local, vertices = top_nodes_local, directed = TRUE)

# Attach server-computed coords to local graph vertices
coords_local <- coords_local[match(V(g_local)$name, coords_local$name), ]
V(g_local)$x <- coords_local$x
V(g_local)$y <- coords_local$y

ggraph(g_local, layout = "manual") +
  geom_edge_link(alpha = 0.3) +
  geom_node_point(aes(size = repost_count, color = repost_count)) +
  geom_node_text(aes(label = display_name), repel = TRUE) +
  scale_size_continuous(range = c(3, 12)) +
  scale_color_gradient(low = "lightblue", high = "red") +
  theme_void()
# ---------------------------
# Notes and tips
# ---------------------------
# - All heavy transforms (SELECT DISTINCT, GROUP BY, JOIN) are executed on SQL Server via RxSqlServerData(sqlQuery=...) and rxDataStep(outFile=RxSqlServerData(...)).
# - If you prefer to use explicit SQL DDL (CREATE TABLE AS SELECT ...), you can run those statements directly in SQL Server (e.g., via DBI/odbc) and then reference the tables with RxSqlServerData.
# - For very large datasets, avoid rxImport() without filters; instead use RxSqlServerData(sqlQuery=...) to read only the rows you need.
# - If you want to compute additional aggregates (e.g., in-degree, out-degree, temporal summaries), express them as SQL GROUP BY queries and persist the results as tables in BlueSkyNet.
# - If you need to run R functions on the server (not just SQL), consider using Microsoft R Server's remote compute contexts (rxSetComputeContext) — that is a more advanced setup and depends on your server configuration.
