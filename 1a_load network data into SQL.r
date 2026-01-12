# Rewritten to load network data into SQL Server (BlueSkyNet) and run calculations on the server
# Requires: RevoScaleR, DBI/odbc (optional for direct checks), igraph/ggraph for local plotting

source("0_functions.R")

# ---------------------------
# Configuration: SQL Server
# ---------------------------
# Edit these values for your environment
sql_server <- "localhost" # e.g., "localhost\\SQLEXPRESS" or
# "sqlserver.domain.com"
database <- "BlueSkyNet"
use_trusted_connection <- TRUE # set FALSE if using SQL auth
#sql_user <- "your_sql_user"
# only used if not using trusted connection
#sql_password <- "your_password"
# only used if not using trusted connection
orgicc <- rxGetComputeContext()
rxSetComputeContext("localpar")

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

# Helper to create RxSqlServerData objects
rx_sql_table <- function(table_name, connectionString = connStr) {
  RxSqlServerData(
    table = table_name,
    connectionString = connectionString,
    stringsAsFactors = FALSE
  )
}

# shareDir must be accessible by SQL Server compute context
shareDir <- paste("H:\\AllShare\\", Sys.getenv("USERNAME"), sep = "")
# change to a path accessible by SQL Server machine

# ---------------------------
# Step 0: Read CSVs locally
# ---------------------------
posts_local <- read_parquet(
  "/../../Source/Repos/BlueSkyNetWorks/data/speirgorm_posts.parquet",
  stringsAsFactors = FALSE
)
reposts_local <- arrow::read_parquet(
  "/../../Source/Repos/BlueSkyNetWorks/data/speirgorm_reposts.parquet",

  stringsAsFactors = FALSE
)

# ---------------------------
# Step 1: Write raw CSVs to SQL Server
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

# ---------------------------
# Step 2: Create deduplicated edges table on server
#   edges columns: from, to, repost_uri, created_at
#   - from = handle (who reposted)
#   - to   = author_handle of the original post (we will join posts_
# raw to get author_handle)
# ---------------------------
create_edges_sql <- "--Drop existing edge table if it exists
IF OBJECT_ID('dbo.Reposted', 'U') IS NOT NULL
BEGIN
    DROP TABLE dbo.Reposted;
END;

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
    edgeStarts DATETIME2 NULL,
    edgeEnds DATETIME2 NULL
) AS EDGE;
"
dbExecute(odbc_con, create_edges_sql)

create_nodes_sql <- "-- 1. Drop existing graph node table if it exists
IF OBJECT_ID('dbo.Person', 'U') IS NOT NULL
BEGIN
    DROP TABLE dbo.Person;
END;
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

    PRIMARY KEY (handle)
) AS NODE;"
dbExecute(odbc_con, create_nodes_sql)

populate_person_sql <- "TRUNCATE TABLE dbo.Person;
INSERT INTO dbo.Person (
    handle,
    first_seen,
    last_seen,
    posts_authored,
    reposts_made,
    reposts_received
)
SELECT
    handle,

    MIN(PostedOn) AS first_seen,
    MAX(PostedOn) AS last_seen,

    SUM(CASE WHEN role = 'author' THEN 1 ELSE 0 END) AS posts_authored,
    SUM(CASE WHEN role = 'reposter' THEN 1 ELSE 0 END) AS reposts_made,
    SUM(CASE WHEN role = 'author' THEN reposts_received ELSE 0 END) AS reposts_received 
FROM (
    --------------------------------------------------------------------
    -- Canonical network data frame
    --------------------------------------------------------------------
    SELECT
        P.author_handle AS handle,
        CAST(P.indexedAt AS date) AS PostedOn,
        'author' AS role,
        1 AS reposts_received
    FROM posts_raw AS P
    INNER JOIN reposts_raw AS RP ON P.uri = RP.uri

    UNION ALL

    SELECT
        RP.handle AS handle,
        CAST(P.indexedAt AS date) AS PostedOn,
        'reposter' AS role,
        0 AS reposts_received
    FROM posts_raw AS P
    INNER JOIN reposts_raw AS RP ON P.uri = RP.uri
) AS X
WHERE handle IS NOT NULL
GROUP BY handle;"
dbExecute(odbc_con, populate_person_sql)

# ---------------------------
# Step 4: Populate Reposted edge table
# - Insert edges by resolving $node_id for from/to using Person.name
# ---------------------------
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
        CAST(P.indexedAt AS date) AS edgeEnds
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
  FROM [BlueSkyNet].[dbo].[Person];"
)

nodes <- nodes |>
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
    r.text ,
    CAST(r.edgeStarts AS date) AS edgeStarts,
    CAST(r.edgeEnds AS date) AS edgeEnds
FROM dbo.Reposted AS r
JOIN dbo.Person AS f
    ON r.$from_id = f.$node_id      -- reposter
JOIN dbo.Person AS t
    ON r.$to_id = t.$node_id        -- original author
ORDER BY r.posted_on DESC;
"
)

edges <- edges |>
  mutate(from = normalize_handle(from), to = normalize_handle(to)) |>
  mutate(across(
    where(is.character),
    ~ gsub("[[:cntrl:]]", "", .)
  ))

write_parquet(edges, "graphs/speirgorm_edges.parquet")
