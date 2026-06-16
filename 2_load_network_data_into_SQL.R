# Rewritten to load network data into SQL Server (BlueSkyNet) and run calculations on the server
# Requires: RevoScaleR, DBI/odbc (optional for direct checks), igraph/ggraph for local plotting

 source("0_functions.R")

# ---------------------------
# Step 0: Read raw data Parquet files locally
# ---------------------------
posts_local <- read_parquet(
  "./data/speirgorm_posts.parquet",
  stringsAsFactors = FALSE
)
reposts_local <- arrow::read_parquet(
  "./data/speirgorm_reposts.parquet",

  stringsAsFactors = FALSE
)
cat("\n=== Step 2a: Read Parquet files locally ===\n")
# ---------------------------
# Step 1: Write raw data to SQL Server
# ---------------------------
# Write posts -> table: posts_raw
posts_sql <- rx_sql_table("posts_raw")
rxDataStep(inData = posts_local, outFile = posts_sql, overwrite = TRUE)

# Write reposts -> table: reposts_raw
reposts_sql <- rx_sql_table("reposts_raw")
rxDataStep(inData = reposts_local, outFile = reposts_sql, overwrite = TRUE)

network_query <- "SELECT P.uri AS Post, P.author_handle AS PostedBy, 
CAST(P.indexedAt AS date) AS PostedOn, P.text, P.bookmark_count, P.like_count, 
P.reply_count, P.repost_count,RP.handle AS RepostedBy FROM posts_raw AS P 
INNER JOIN reposts_raw as RP ON P.uri = RP.uri ORDER BY P.uri
"
post_src <- RxSqlServerData(
  sqlQuery = network_query,
  connectionString = connStr,
  stringsAsFactors = FALSE
)
posts_df <- rxImport(post_src)

# save the network data frame for possible later use
write_parquet(posts_df, "data/speirgorm_network.parquet")
cat("\n=== Step 2b: Write raw CSVs to SQL Server ===\n")
# ---------------------------
# Step 2: Create deduplicated edges table on server
#   edges columns: from, to, repost_uri, created_at
#   - from = handle (who reposted)
#   - to   = author_handle of the original post (we will join posts_
# raw to get author_handle)
# ---------------------------
create_edges_sql <- "--Drop existing edge table if it exists
IF OBJECT_ID('dbo.Reposted', 'U') IS NULL
BEGIN
     --DROP TABLE dbo.Reposted;
-- END;

-- 3. Create edge table for repost relationships
CREATE TABLE dbo.Reposted (
    post_uri NVARCHAR(512) NOT NULL,
    posted_on DATE NULL,
    repost_event_at DATETIME2 NULL,
    like_count INT NULL,
    reply_count INT NULL,
    bookmark_count INT NULL,
    repost_count INT NULL,
    text NVARCHAR(MAX) NULL,
    weight FLOAT NULL,
    betweenness FLOAT NULL,
    edgeStarts DATETIME2 NULL,
    edgeEnds DATETIME2 NULL
) AS EDGE;
END;
"
dbExecute(odbc_con, create_edges_sql)

create_nodes_sql <- "-- 1. Drop existing graph node table if it exists
IF OBJECT_ID('dbo.Person', 'U') IS NULL
BEGIN
    --DROP TABLE dbo.Person;
--END;
-- 2. Create graph node table for persons
CREATE TABLE Person (
    handle NVARCHAR(255) NOT NULL,        -- unique user handle
    
    first_seen DATE NULL,                 -- earliest appearance in dataset
    last_seen DATE NULL,                  -- latest appearance

    posts_authored INT DEFAULT 0,         -- count of posts they created
    reposts_made INT DEFAULT 0,           -- count of reposts they performed
    reposts_received INT DEFAULT 0,     -- count of reposts their posts received

    total_likes_on_posts INT DEFAULT 0,   -- optional aggregate
    total_replies_on_posts INT DEFAULT 0, -- optional aggregate
    total_bookmarks_on_posts INT DEFAULT 0,
    -- Core centrality measures
    betweenness FLOAT NULL,
    betweenness_norm FLOAT NULL,
    closeness FLOAT NULL,
    closeness_norm FLOAT NULL,
    eigenvector_centrality FLOAT NULL,
    pagerank FLOAT NULL,
    pagerank_norm FLOAT NULL,
    
    -- Authority and hub scores
    hub_score FLOAT NULL,
    authority_score FLOAT NULL,
    authority_norm FLOAT NULL,
    
    -- Clustering and cohesion
    local_clustering FLOAT NULL,
    kcore INT NULL,
    
    -- Degree variants (already may exist, but explicit here)
    in_degree INT NULL,
    out_degree INT NULL,
    total_degree INT NULL,
    
    -- Community assignment
    community INT NULL,
    modularity FLOAT NULL,
    
    -- Engagement influence score (composite)
    influence_score FLOAT NULL,  -- can be computed as weighted combo of metrics

    PRIMARY KEY (handle)
) AS NODE;
END;"
dbExecute(odbc_con, create_nodes_sql)

cat("\n=== Step 2c: populate the Nodes and Edges tables ===\n")

populate_person_sql <- "TRUNCATE TABLE dbo.Person;
INSERT INTO dbo.Person (
    handle,
    first_seen,
    last_seen,
    posts_authored,
    reposts_made,
    reposts_received,
    total_likes_on_posts,
    total_replies_on_posts,
    total_bookmarks_on_posts
)
SELECT
    handle,

    MIN(PostedOn) AS first_seen,
    MAX(PostedOn) AS last_seen,

    SUM(CASE WHEN role = 'author' THEN 1 ELSE 0 END) AS posts_authored,
    SUM(CASE WHEN role = 'reposter' THEN 1 ELSE 0 END) AS reposts_made,
    SUM(CASE WHEN role = 'author' THEN reposts_received ELSE 0 END) AS reposts_received, 
    MAX(like_count) AS total_likes_on_posts,
    MAX(reply_count) AS total_replies_on_posts,
    MAX(bookmark_count) AS total_bookmarks_on_posts
FROM (
    --------------------------------------------------------------------
    -- Canonical network data frame
    --------------------------------------------------------------------
    SELECT  P.author_handle AS handle, CAST(P.indexedAt AS date) AS PostedOn, 'author' AS role, 1 AS reposts_received, P.like_count, P.reply_count, P.bookmark_count
FROM      posts_raw AS P INNER JOIN
                 reposts_raw AS RP ON P.uri = RP.uri

    UNION ALL

    SELECT
        RP.handle AS handle,
        CAST(P.indexedAt AS date) AS PostedOn,
        'reposter' AS role,
        0 AS reposts_received, P.like_count, P.reply_count, P.bookmark_count
    FROM posts_raw AS P
    INNER JOIN reposts_raw AS RP ON P.uri = RP.uri
) AS X
WHERE handle IS NOT NULL
GROUP BY handle;"
dbExecute(odbc_con, populate_person_sql)

# ---------------------------
# Step 3: Populate Reposted edge table
# - Insert edges by resolving $node_id for from/to using Person.name
# ---------------------------

cat("\n=== Step 2d: Populate Reposted edge table ===\n")

populate_reposted_sql <- "TRUNCATE TABLE dbo.Reposted;

WITH NetDF AS (
    SELECT  
        P.uri AS Post,
        P.author_handle AS PostedBy,
        CAST(P.indexedAt AS date) AS PostedOn,
        P.text,
        P.bookmark_count,
        P.like_count,
        P.reply_count,
        P.repost_count,
        RP.handle AS RepostedBy,
        CAST(P.indexedAt AS date) AS edgeStarts,
        CASE 
           WHEN TRY_CAST(RP.indexed_at AS date) IS NULL THEN CAST(P.indexedAt AS date)
           WHEN TRY_CAST(RP.indexed_at AS date) < CAST(P.indexedAt AS date) THEN CAST(P.indexedAt AS date)
           ELSE TRY_CAST(RP.indexed_at AS date)
        END AS edgeEnds
    FROM posts_raw AS P
    INNER JOIN reposts_raw AS RP ON P.uri = RP.uri
)

INSERT INTO dbo.Reposted (
    $from_id,
    $to_id,
    post_uri,
    posted_on,
    like_count,
    reply_count,
    bookmark_count,
    repost_count,
    text,
    edgeStarts,
    edgeEnds
)
SELECT
    f.$node_id,        -- RepostedBy (source)
    t.$node_id,          -- PostedBy (target)
    r.Post,
    r.PostedOn,
    r.like_count,
    r.reply_count,
    r.bookmark_count,
    r.repost_count,
    r.text,
    r.edgeStarts,
    r.edgeEnds
FROM NetDF AS r
JOIN Person AS f ON f.handle = r.RepostedBy   -- reposter = FROM
JOIN Person AS t ON t.handle = r.PostedBy     -- author = TO
WHERE r.RepostedBy IS NOT NULL
  AND r.PostedBy IS NOT NULL;
"
dbExecute(odbc_con, populate_reposted_sql)

# # ---------------------------
# # Step 4: Create network metrics in SQL Server using igraph in R (via RevoScaleR)
# # ---------------------------

cat(
  "\n=== Step 2e: Create network metrics in SQL Server using SQL ===\n"
)

# moved this as some of the igraph alogrithms use the edge weight

dbExecute(
  odbc_con,
  "-- Build in-degree counts
SELECT 
    $to_id AS to_id,
    COUNT(*) AS in_degree
INTO #in_degree_tmp
FROM dbo.Reposted
GROUP BY $to_id;

-- select * from #in_degree_tmp;
-- Update Person table
UPDATE p
SET p.in_degree = c.in_degree
FROM dbo.Person AS p
JOIN #in_degree_tmp AS c
    ON p.$node_id = c.to_id;

-- Clean up
DROP TABLE #in_degree_tmp;"
)
cat("\n=== Step 2f: Created in-degree metrics in SQL Server using SQL ===\n")

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
cat("\n=== Step 2g: Created out-degree metrics in SQL Server using SQL ===\n")

# Execute the proc sp_ComputeInfluenceScore
tryCatch(
  {
    dbExecute(odbc_con, "EXEC sp_ComputeInfluenceScore;")
  },
  error = function(e) {
    cat("  ⚠ Influence score computation warning:", e$message, "\n")
  }
)
cat("\n=== Step 4c: Ran Influence score computation SQL Server using SQL ===\n")
# Execute the proc sp_ComputeEdgeweight
tryCatch(
  {
    dbExecute(odbc_con, "EXEC sp_ComputeEdgeweight;")
  },
  error = function(e) {
    cat("  ⚠ Edgeweight computation warning:", e$message, "\n")
  }
)
cat("\n=== Step 2h: Ran Edgeweight computation SQL Server using SQL ===\n")
# Execute the proc sp_ComputeEdgeBetweenness
tryCatch(
  {
    dbExecute(odbc_con, "EXEC sp_ComputeEdgeBetweenness;")
  },
  error = function(e) {
    cat("  ⚠ Edge betweenness computation warning:", e$message, "\n")
  }
)
cat(
  "\n=== Step 2i: Ran Edge betweenness computation SQL Server using SQL ===\n"
)

# Read edges/nodes from SQL via RxSqlServerData
edges_rx <- RxSqlServerData(
  sqlQuery = "
    SELECT 
    f.handle AS [from],
    t.handle AS [to],
    r.post_uri AS repost_uri,
    r.posted_on AS created_at,
    r.like_count,
    r.reply_count,
    r.bookmark_count,
    r.repost_count,
    COALESCE(r.betweenness, 0) AS betweenness,
    r.weight
    ,CAST(r.edgeStarts AS date) AS edgeStarts
    ,CAST(r.edgeEnds AS date) AS edgeEnds
    ,COALESCE(r.text, 'NO TEXT HERE') AS text
FROM dbo.Reposted AS r
JOIN dbo.Person AS f
    ON r.$from_id = f.$node_id      -- reposter
JOIN dbo.Person AS t
    ON r.$to_id = t.$node_id        -- original author
ORDER BY r.posted_on DESC",
  connectionString = connStr
)

nodes_rx <- RxSqlServerData(
  sqlQuery = "SELECT [handle] AS name
      ,[first_seen] AS earliestPost
      ,[last_seen] AS latestPost
      ,[posts_authored]
      ,[reposts_made]
      ,[reposts_received]
      ,[total_likes_on_posts]
      ,[total_replies_on_posts]
      ,[total_bookmarks_on_posts]
      ,[betweenness]
      ,[betweenness_norm]
      ,COALESCE([closeness], 0) AS closeness
      ,COALESCE([closeness_norm], 0) AS closeness_norm
      ,[eigenvector_centrality]
      ,[pagerank]
      ,[pagerank_norm]
      ,[hub_score]
      ,[authority_score]
      ,[authority_norm]
      ,[local_clustering]
      ,[kcore]
      ,COALESCE([in_degree], 0) AS in_degree
      ,COALESCE([out_degree], 0) AS out_degree
      ,[total_degree]
      ,[community]
      ,[modularity]
      ,[influence_score]
  FROM [BlueSkyNet].[dbo].[Person];",
  connectionString = connStr
)
cat(
  "\n=== Step 2j: Run igraph inside SQL compute context ===\n"
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
    hits <- hits_scores(g, scale = FALSE)
    hits_norm <- hits_scores(g, scale = TRUE)
    comm <- cluster_louvain(
      as_undirected(g, mode = 'collapse'),
      E(as_undirected(g, mode = 'collapse'))$weight,
      resolution = 1
    )
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
      authority_norm = as.numeric(hits_norm$authority),
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
cat(
  "\n=== Step 2k: Created network metrics in SQL Server using igraph in R (via RevoScaleR) ===\n"
)
# dbExecute(odbc_con, "DROP TABLE dbo.centrality_tmp;")

# # ---------------------------
# # Step 5: Create edges and nodes csv files for later
# #   Create table: repost_counts (to, repost_count)
# # ---------------------------
nodes <- dbGetQuery(
  odbc_con,
  "SELECT [handle] AS name
      ,[first_seen] AS earliestPost
      ,[last_seen] AS latestPost
      ,[posts_authored]
      ,[reposts_made]
      ,[reposts_received]
      ,[total_likes_on_posts]
      ,[total_replies_on_posts]
      ,[total_bookmarks_on_posts]
      ,[betweenness]
      ,[betweenness_norm]
      ,COALESCE([closeness], 0) AS closeness
      ,COALESCE([closeness_norm], 0) AS closeness_norm
      ,[eigenvector_centrality]
      ,[pagerank]
      ,[pagerank_norm]
      ,[hub_score]
      ,[authority_score]
      ,[authority_norm]
      ,[local_clustering]
      ,[kcore]
      ,COALESCE([in_degree], 0) AS in_degree
      ,COALESCE([out_degree], 0) AS out_degree
      ,[total_degree]
      ,[community]
      ,[modularity]
      ,[influence_score]
  FROM [BlueSkyNet].[dbo].[Person];"
)

nodes <- nodes %>%
  mutate(across(
    where(is.character),
    ~ gsub("[[:cntrl:]]", "", .)
  ))

write_parquet(nodes, "graphs/speirgorm_nodes.parquet")

edges <- dbGetQuery(
  odbc_con,
  "SELECT 
    f.handle AS [from],
    t.handle AS [to],
    r.post_uri AS repost_uri,
    r.posted_on AS created_at,
    r.like_count,
    r.reply_count,
    r.bookmark_count,
    r.repost_count,
    COALESCE(r.betweenness, 0) AS betweenness,
    r.weight
    ,CAST(r.edgeStarts AS date) AS edgeStarts
    ,CAST(r.edgeEnds AS date) AS edgeEnds
    ,COALESCE(r.text, 'NO TEXT HERE') AS text
FROM dbo.Reposted AS r
JOIN dbo.Person AS f
    ON r.$from_id = f.$node_id      -- reposter
JOIN dbo.Person AS t
    ON r.$to_id = t.$node_id        -- original author
ORDER BY r.posted_on DESC;
"
)

edges <- edges %>%
  mutate(from = normalize_handle(from), to = normalize_handle(to)) %>%
  mutate(across(
    where(is.character),
    ~ gsub("[[:cntrl:]]", "", .)
  ))
names(edges)

write_parquet(edges, "graphs/speirgorm_edges.parquet")
cat(
  "\n=== Step 2l: Created edges and nodes csv files for later ===\n"
)

tobermvd <- c(
  'posts_local',
  'reposts_local',
  'posts_df',
  'centrality_df'
)
rm(list = tobermvd)
gc()
