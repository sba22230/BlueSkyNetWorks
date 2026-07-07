library(RevoScaleR)
library(lattice)
library(dplyr)
library(lubridate)
library(igraph)
library(stringr)
library(stringi)
library(tidyr)
library(purrr)
library(tibble)
library(ggraph)
library(visNetwork)
library(retry)
library(rgexf)

origComputeContext <- rxGetComputeContext()
# connect to SQL
connStr <- "Driver=SQL Server;Server=.;Database=BlueSkyNet;Trusted_Connection=True;"
sqlShareDir <- paste("H:\\AllShare\\", Sys.getenv("USERNAME"), sep = "")
sqlWait <- TRUE
sqlConsoleOutput <- FALSE
sqlcc <- RxInSqlServer(
  connectionString = connStr,
  shareDir = sqlShareDir,
  wait = sqlWait,
  consoleOutput = sqlConsoleOutput
)
rxSetComputeContext(sqlcc)

# load data from SQL table into a data frame
postsQuery <- "SELECT * FROM dbo.posts_raw"
repostsQuery <- "SELECT * FROM dbo.reposts_raw"
posts_df <- RxSqlServerData(
  sqlQuery = postsQuery,
  connectionString = connStr,
  colClasses = c(
    like_count = "numeric",
    bookmark_count = "numeric",
    reply_count = "numeric",
    repost_count = "numeric"
  ),
  rowsPerRead = 50000
)
reposts_df <- RxSqlServerData(
  sqlQuery = repostsQuery,
  connectionString = connStr,
  colClasses = c(
    viewer_muted = "logical",
    viewer_blocked_by = "logical",
    labels_neg = "logical",
    verification_verifications_is_valid = "logical",
    labels_ver = "numeric",
    labels_ver_..13 = "numeric",
    labels_ver_..19 = "numeric",
    labels_ver_..22 = "numeric",
    labels_ver_..28 = "numeric",
    labels_ver_..24 = "numeric",
    labels_ver_..30 = "numeric",
    status_record_duration_minutes = "numeric",
    status_is_active = "logical"
  ),
  rowsPerRead = 50000
)

cat("Posts data variable info:\n")
rxGetVarInfo(data = posts_df)

start.time <- proc.time()
rxSummary(~ like_count:F(repost_count, 1, 20), data = posts_df)
used.time <- proc.time() - start.time
print(paste(
  "It takes CPU Time=",
  round(used.time[1] + used.time[2], 2),
  " seconds,
  Elapsed Time=",
  round(used.time[3], 2),
  " seconds to summarize the Posts Data.",
  sep = ""
))

cat("\nReposts data variable info:\n")
rxGetVarInfo(data = reposts_df)
start.time <- proc.time()
rxSummary(
  ~ status_record_duration_minutes:F(labels_ver, 1, 10),
  data = reposts_df
)
used.time <- proc.time() - start.time
print(paste(
  "It takes CPU Time=",
  round(used.time[1] + used.time[2], 2),
  " seconds,
  Elapsed Time=",
  round(used.time[3], 2),
  " seconds to summarize the Reposts Data.",
  sep = ""
))

### Plots and Graphs

# Plot repost amount on SQL Server and return the plot
start.time <- proc.time()
rxHistogram(~repost_count, data = posts_df, title = "Repost Count Histogram")
used.time <- proc.time() - start.time
print(paste(
  "It takes CPU Time=",
  round(used.time[1] + used.time[2], 2),
  " seconds, Elapsed Time=",
  round(used.time[3], 2),
  " seconds to generate plot.",
  sep = ""
))

# Plot repost amount on SQL Server and return the plot
start.time <- proc.time()
rxHistogram(
  ~labels_ver,
  data = reposts_df,
  title = "Lables-ver Count Histogram"
)
used.time <- proc.time() - start.time
print(paste(
  "It takes CPU Time=",
  round(used.time[1] + used.time[2], 2),
  " seconds, Elapsed Time=",
  round(used.time[3], 2),
  " seconds to generate plot.",
  sep = ""
))
# Debug: check reposts_df structure
print(head(reposts_df))

### Networking stuff
nodes_query <- "SELECT [handle] AS name
      ,[first_seen] AS earliestPost
      ,[last_seen] AS latestPost
      ,[posts_authored]
      ,[reposts_made]
      ,[reposts_received]
      ,[total_likes_on_posts]
      ,[total_replies_on_posts]
      ,[total_bookmarks_on_posts]
  FROM [BlueSkyNet].[dbo].[Person];"
# Step 7: Build edge list (who reposted whom)
nodes <- RxSqlServerData(
  sqlQuery = nodes_query,
  connectionString = connStr,
  rowsPerRead = 500000
)

cat("Nodes count:", rxSummary(~., nodes)$nobs.valid, "\n")

# Debug: inspect edges
print(head(nodes))

# Edges = Reposted graph edge table projected to from/to names
edges_query <- "
  SELECT 
    f.handle AS [from],
    t.handle AS [to],
    r.post_uri AS repost_uri,
    r.posted_on AS created_at,
    
    r.like_count,
    r.reply_count,
    r.bookmark_count,
    r.repost_count
    ,CAST(r.edgeStarts AS date) AS edgeStarts
    ,CAST(r.edgeEnds AS date) AS edgeEnds
    ,r.text 
FROM dbo.Reposted AS r
JOIN dbo.Person AS f
    ON r.$from_id = f.$node_id      -- reposter
JOIN dbo.Person AS t
    ON r.$to_id = t.$node_id        -- original author
ORDER BY r.posted_on DESC;
"

edges <- edges %>%
  mutate(from = normalize_handle(from), to = normalize_handle(to)) %>%
  mutate(across(
    where(is.character),
    ~ gsub("[[:cntrl:]]", "", .)
  ))

names(edges)

edges <- RxSqlServerData(
  sqlQuery = edges_query,
  connectionString = connStr,
  rowsPerRead = 5000000
)

cat("Nodes count:", rxSummary(~., edges)$nobs.valid, "\n")

# Debug: inspect nodes
print(head(edges))

# Step 9: Build igraph object and plot basic network
g1 <- graph_from_data_frame(
  d = rxImport(edges),
  vertices = rxImport(nodes),
  directed = TRUE
)
cat("Graph summary:\n")
print(summary(g1))

