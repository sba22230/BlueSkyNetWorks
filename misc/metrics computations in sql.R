# 3_build_graph_and_metrics.R
source("0_functions.R")

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
    
    pr  <- page_rank(g, directed = TRUE)$vector
    bet <- betweenness(g, directed = TRUE)
    clo <- closeness(g, mode = "out", normalized = FALSE)
    eig <- eigen_centrality(g, directed = TRUE)$vector
    deg <- degree(g, mode = "total" )
    core_vals <- coreness(g)
    hits <- hits_scores(g, scale = TRUE)
    comm <- cluster_walktrap(g)
    
    data.frame(
      name       = names(pr),
      pagerank   = as.numeric(pr),
      betweenness = as.numeric(bet),
      closeness   = as.numeric(clo),
      eigenvector_centrality = as.numeric(eig),
      degree = as.numeric(deg),
      kcore = as.numeric(core_vals),
      hub_score =  as.numeric(hits$hub),
      authority_score = as.numeric(hits$authority),
      community = as.numeric(comm_aligned), 
      
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
  table = "centrality_tmp",
  connectionString = connStr
)
rxDataStep(centrality_df, centrality_sql, overwrite = TRUE)

dbExecute(odbc_con, "
  UPDATE p
  SET p.pagerank   = c.pagerank,
      p.betweenness = c.betweenness,
      p.closeness   = c.closeness,
      p.eigenvector_centrality = c.eigenvector_centrality,
      p.total_degree = c.degree,
      p.kcore = c.kcore,
      p.hub_score = c.hub_score,
      p.authority_score = c.authority_score,
      p.community = c.community
      
  FROM dbo.Person AS p
  JOIN dbo.centrality_tmp AS c ON p.handle = c.name;
")

dbExecute(odbc_con, "DROP TABLE dbo.centrality_tmp;")

dbExecute(odbc_con, "-- Build in-degree counts
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
DROP TABLE #in_degree_tmp;")

dbExecute(odbc_con, "-- Build out-degree counts
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
DROP TABLE #out_degree_tmp;")

