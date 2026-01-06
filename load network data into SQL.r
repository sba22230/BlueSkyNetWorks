# Rewritten to load network data into SQL Server (BlueSkyNet) and run calculations on the server
# Requires: RevoScaleR, DBI/odbc (optional for direct checks), igraph/ggraph for local plotting

library(DBI)
library(odbc)
library(dplyr)
library(lubridate)
library(igraph)
library(ggraph)
library(RevoScaleR) # RevoScaleR provides RxSqlServerData, rxDataStep, etc.

# ---------------------------
# Configuration: SQL Server
# ---------------------------
# Edit these values for your environment
sql_server <- "localhost" # e.g., "localhost\\SQLEXPRESS" or "sqlserver.domain.com"
database <- "BlueSkyNet"
use_trusted_connection <- TRUE # set FALSE if using SQL auth
sql_user <- "your_sql_user" # only used if not using trusted connection
sql_password <- "your_password" # only used if not using trusted connection
orgicc <- rxGetComputeContext()
rxSetComputeContext("localpar")

if (use_trusted_connection) {
  odbc_con <- dbConnect(odbc::odbc(),
                        Driver   = "SQL Server",
                        Server   = sql_server,
                        Database = database,
                        Trusted_Connection = "Yes")
  
  connStr <- paste0(
    "Driver={SQL Server};Server=",
    sql_server,
    ";Database=",
    database,
    ";Trusted_Connection=Yes;"
  )
} else {
  odbc_con <- dbConnect(odbc::odbc(),
                        Driver   = "SQL Server",
                        Server   = sql_server,
                        Database = database,
                        UID = sql_user,
                        PWD = sql_password)
  
  connStr <- paste0(
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
shareDir <- paste("H:\\AllShare\\",Sys.getenv("USERNAME"),sep="")  # change to a path accessible by SQL Server machine

# ---------------------------
# Step 0: Read CSVs locally
# ---------------------------
posts_local <- read.csv("/../../Source/Repos/BlueSkyNetWorks/data/speirgorm_posts.csv", stringsAsFactors = FALSE)
reposts_local <- read.csv(
  "/../../Source/Repos/BlueSkyNetWorks/data/speirgorm_reposts.csv",
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

# ---------------------------
# Step 2: Create deduplicated edges table on server
#   edges columns: from, to, repost_uri, created_at
#   - from = handle (who reposted)
#   - to   = author_handle of the original post (we will join posts_raw to get author_handle)
# ---------------------------
populate_person_sql <- "
-- Insert distinct names from reposts_raw and posts_raw into Person if not already present
INSERT INTO Person (name, display_name, avatar, did)
SELECT DISTINCT r.handle, r.display_name, r.avatar, r.did
FROM reposts_raw r
WHERE r.handle IS NOT NULL;

INSERT INTO Person (name)
SELECT DISTINCT p.author_handle
FROM posts_raw p
WHERE p.author_handle IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM Person q WHERE q.name = p.author_handle);;
"
dbExecute(odbc_con, populate_person_sql)

# ---------------------------
# Step 4: Populate Reposted edge table
# - Insert edges by resolving $node_id for from/to using Person.name
# ---------------------------
populate_reposted_sql <- "
INSERT INTO Reposted ($from_id, $to_id, repost_uri, created_at)
SELECT f.$node_id, t.$node_id, r.uri, TRY_CAST(r.created_at AS DATETIME2)
FROM reposts_raw r
JOIN posts_raw p ON r.original_uri = p.uri
JOIN Person f ON f.name = r.handle
JOIN Person t ON t.name = p.author_handle
WHERE r.handle IS NOT NULL AND p.author_handle IS NOT NULL;
"
dbExecute(odbc_con, populate_reposted_sql)

# # ---------------------------
# # Step 5: Compute repost counts per 'to' (server-side)
# #   Create table: repost_counts (to, repost_count)
# # ---------------------------
compute_counts_sql <- "
WITH counts AS (
  SELECT t.$node_id AS node_id, COUNT(*) AS repost_count
  FROM Reposted r
  JOIN Person t ON t.$node_id = r.$to_id
  GROUP BY t.$node_id
)
UPDATE p
SET repost_count = ISNULL(c.repost_count, 0)
FROM Person p
LEFT JOIN counts c ON p.$node_id = c.node_id;
"
dbExecute(odbc_con, compute_counts_sql)
