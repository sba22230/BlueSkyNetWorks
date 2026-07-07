# 3_build_graph_and_metrics.R
# source("0_functions.R")

# ---------------------------
# Configuration: SQL Server
# ---------------------------
# Edit these values for your environment
sql_server <- "localhost" # e.g., "localhost\\SQLEXPRESS" or
# "sqlserver.domain.com"
database <- "BlueSkyNet"
use_trusted_connection <- TRUE # set FALSE if using SQL auth
# shareDir must be accessible by SQL Server compute context
shareDir <- paste("H:\\AllShare\\", Sys.getenv("USERNAME"), sep = "")
# change to a path accessible by SQL Server machine

origComputeContext <- rxGetComputeContext()
if (use_trusted_connection) {
  odbc_con <- dbConnect(
    odbc::odbc(),
    Driver = "SQL Server",
    Server = sql_server,
    Database = database,
    Trusted_Connection = "Yes"
  )

  connStr <- paste0(
    "Driver={SQL Server};Server=",
    sql_server,
    ";Database=",
    database,
    ";Trusted_Connection=Yes;"
  )
} else {
  odbc_con <- dbConnect(
    odbc::odbc(),
    Driver = "SQL Server",
    Server = sql_server,
    Database = database,
    UID = sql_user,
    PWD = sql_password
  )

  connStr <- paste0(
    # nolint: object_name_linter.
    "Driver={SQL Server};Server=",
    sql_server,
    ";Database=",
    database,
    ";Uid=",
    sql_user,
    ";Pwd=",
    sql_password,
    ";"
  )
}
sql_cc <- RxInSqlServer(
  connectionString = connStr,
  numTasks = wrkrs,
  autoCleanup = TRUE
)

library(odbc)
# Read edges/nodes from SQL via RxSqlServerData
edges_rx <- RxSqlServerData(
  sqlQuery = "
    SELECT f.handle AS [from],
           t.handle AS [to]
    FROM dbo.Reposted AS r
    JOIN dbo.Person AS f ON r.$from_id = f.$node_id
    JOIN dbo.Person AS t ON r.$to_id   = t.$node_id;",
  connectionString = connStr
)

nodes_rx <- RxSqlServerData(
  sqlQuery = "SELECT handle AS name FROM dbo.Person;",
  connectionString = connStr
)
rxSetComputeContext(sql_cc)
# Run igraph/statnet inside SQL compute context
centrality_df <- rxExec(
  function(edges_data, nodes_data) {
    library(igraph)

    # Pull data into memory inside SQL compute context
    edges_df <- rxImport(edges_data)
    nodes_df <- rxImport(nodes_data)

    g <- graph_from_data_frame(
      d = edges_df,
      vertices = nodes_df,
      directed = TRUE
    )

    pr <- page_rank(g, directed = TRUE)$vector
    bet <- betweenness(g, directed = TRUE)
    bet_norm <- betweenness(g, directed = TRUE, normalized = TRUE)
    clo <- closeness(g, mode = "out", normalized = FALSE)
    clo_norm <- closeness(g, mode = "out", normalized = TRUE)
    eig <- eigen_centrality(g, directed = TRUE)$vector
    deg <- degree(g, mode = "total")
    core_vals <- coreness(g)
    hits <- hits_scores(g, scale = TRUE)
    comm <- cluster_walktrap(g)
    local_clustering <- transitivity(
      g,
      type = "local",
      isolates = "zero"
    )

    # Extract components
    nm <- comm$names
    mem <- comm$membership
    mod <- comm$modularity

    # Find the longest vector
    n <- max(length(nm), length(mem), length(mod))

    # Pad shorter vectors with zeros
    pad <- function(x, n) {
      length(x) <- n
      x[is.na(x)] <- 0
      x
    }

    comm_df <- data.frame(
      names = pad(nm, n),
      membership = pad(mem, n),
      modularity = pad(mod, n)
    )

    # safe rescale helper
    safe_rescale <- function(x) {
      if (all(is.na(x))) {
        return(rep(0, length(x)))
      }
      rng <- range(x, na.rm = TRUE)
      if (rng[1] == rng[2]) {
        return(rep(0, length(x)))
      }
      scales::rescale(x, to = c(0, 1), from = rng)
    }
    data.frame(
      name = names(pr),
      pagerank = as.numeric(pr),
      pagerank_norm = safe_rescale(pr),
      betweenness = as.numeric(bet),
      betweenness_norm = as.numeric(bet_norm),
      closeness = as.numeric(clo),
      closeness_norm = as.numeric(clo_norm),
      eigenvector_centrality = as.numeric(eig),
      degree = as.numeric(deg),
      kcore = as.numeric(core_vals),
      hub_score = as.numeric(hits$hub),
      authority_score = as.numeric(hits$authority),
      authority_norm = safe_rescale(hits$authority),
      community = as.numeric(comm_df$membership),
      modularity = as.numeric(comm_df$modularity),
      local_clustering = as.numeric(local_clustering),
      stringsAsFactors = FALSE
    )
  },
  edges_data = edges_rx,
  nodes_data = nodes_rx,
  execObjects = c("connStr")
)[[1]]

# rxExec returns a list
rxSetComputeContext("localpar")
# Write metrics back into Person table
centrality_sql <- RxSqlServerData(
  table = "dbo.centrality_tmp",
  connectionString = connStr
)
rxDataStep(centrality_df, centrality_sql, overwrite = TRUE)

dbExecute(
  odbc_con,
  "
  UPDATE p
  SET p.pagerank   = c.pagerank,
      p.pagerank_norm = c.pagerank_norm,
      p.betweenness = c.betweenness,
      p.betweenness_norm = c.betweenness_norm,
      p.closeness   = c.closeness,
      p.closeness_norm = c.closeness_norm,
      p.eigenvector_centrality = c.eigenvector_centrality,
      p.total_degree = c.degree,

      p.kcore = c.kcore,
      p.hub_score = c.hub_score,
      p.authority_score = c.authority_score,
      p.authority_norm = c.authority_norm,
      p.community = c.community,
      p.modularity = c.modularity,
      p.local_clustering = c.local_clustering
      
  FROM dbo.Person AS p
  JOIN dbo.centrality_tmp AS c ON p.handle = c.name;
"
)

# dbExecute(odbc_con, "DROP TABLE dbo.centrality_tmp;")

dbExecute(
  odbc_con,
  "-- Build in-degree counts
SELECT 
    $to_id AS to_id,
    COUNT(*) AS in_degree
INTO #in_degree_tmp
FROM dbo.Reposted
GROUP BY $to_id;

select * from #in_degree_tmp;
-- Update Person table
UPDATE p
SET p.in_degree = c.in_degree
FROM dbo.Person AS p
JOIN #in_degree_tmp AS c
    ON p.$node_id = c.to_id;

-- Clean up
DROP TABLE #in_degree_tmp;"
)

dbExecute(
  odbc_con,
  "-- Build out-degree counts
SELECT 
$from_id AS from_id, 
COUNT(*) AS out_degree
INTO #out_degree_tmp
FROM dbo.Reposted
 GROUP BY $from_id;

-- select * from #out_degree_tmp;
-- Update Person table
UPDATE p
SET p.out_degree = c.out_degree
FROM dbo.Person AS p
JOIN #out_degree_tmp AS c
    ON p.$node_id = c.from_id;

-- Clean up
DROP TABLE #out_degree_tmp;"
)
