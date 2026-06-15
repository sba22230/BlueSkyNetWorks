# BlueSkyNetWorks AI Agent Instructions

## Project Overview
BlueSkyNetWorks is an R-based network analysis project that collects Bluesky social media data and analyzes it using graph theory and network statistics. The project extracts posts, reposts, and thread relationships to build directed networks for analysis via statnet.

## Core Architecture

### Data Pipeline (Sequential Processing)
1. **Search Phase** (`deep_search_posts`): Query Bluesky API for posts matching a search term (e.g., "Speirgorm")
   - Uses `bskyr` package for API access
   - Implements pagination with cursor-based pagination and time-window anchoring
   - Checkpoints every ~1000 rows to resume on failure
   - Outputs: `speirgorm_posts.parquet`, `speirgorm_posts.csv`

2. **Hydration Phase** (`hydrate_in_batches`): Expand post metadata by fetching reposts and thread replies
   - Parallel batch processing using `furrr` (multicore)
   - Two hydration tracks: reposts → `get_reposts_df()` and threads → `get_thread_df()`
   - Batches size: 500 URIs per batch with 3-6s inter-batch sleep
   - Outputs: batched CSV files (`reposts_batch_*.csv`, `threads_batch_*.csv`) + checkpoints

3. **SQL Ingestion**: Load hydrated data into MSSQL (SQL Server graph tables)
   - Schema: `Person` (nodes), `Reposted` (edges)
   - Uses T-SQL graph syntax for network queries

4. **Network Analysis** (`3_StatnetAnalysis.R`): Build network objects and compute metrics
   - Converts edges/nodes to `statnet` network objects
   - Computes: degree, betweenness, clustering, community detection
   - Visualizes: `ggraph` (static), `visNetwork` (interactive)

### Key Libraries & Roles
- **bskyr**: Bluesky API client (authentication, search, post/repost/thread fetching)
- **arrow/parquet**: Fast data serialization for checkpointing
- **dplyr/tidyr/purrr**: Data manipulation and functional programming
- **statnet/igraph/intergraph**: Network objects and analysis
- **RevoScaleR/ODBC**: SQL Server connectivity
- **furrr**: Parallel map-reduce operations
- **ggraph/visNetwork**: Network visualization

## Critical Developer Patterns

### API Error Handling
All BlueSky API calls wrap in `retry()` with exponential backoff and error classification:
```r
retry(bs_get_reposts(uri, auth=bs_auth), when="error", max_tries=4, interval=runif(1,0.8,1.8))
```
Distinguish: 400 (bad URI → skip), 502 (temp gateway → queue), other → log and continue.

### Safe Data Extraction
Use `safe_chr()` / `safe_int()` to extract nested API fields that may be NULL/missing:
```r
safe_chr(.x, "author", "handle")  # returns NA_character_ if not found, never fails
```
Never assume nested list structure; API responses vary (see `get_posts_from_resp()` for shape detection).

### Checkpointing & Resume
- Write parquet checkpoints at strategic milestones (every ~1000 rows in search, per batch in hydration)
- Resume logic checks for existing checkpoints before reprocessing
- Example: `deep_search_posts()` resumes from `since:{latest_timestamp}` filter
- This allows long-running jobs to survive transient API failures

### Parallel Processing Strategy
Use `furrr::future_map_dfr()` with explicit seed for reproducibility:
```r
future_map_dfr(uri_vector, get_reposts_df, .progress=TRUE, .options=furrr_options(seed=22230))
```
Worker count: `wrkrs = max(1, floor(availableCores() * 0.3))` (conservative: 30% of cores).

## Data Formats & Conventions

### Directory Structure
- `data/`: Raw CSV/parquet data and hydration checkpoints
  - `speirgorm_posts.parquet` → canonical post dataset
  - `speirgorm_hydrated_*.parquet` → checkpoint for reposts/threads
  - `reposts_batch_*.csv`, `threads_batch_*.csv` → per-batch hydration outputs
- `graphs/`: Network objects and edge/node tables (parquet)
- `.Rproj`: RStudio project config; ensures `UseNativePipeOperator: Yes` (use `|>` not `%>%`)

### Column Naming
- Database/network: `PostedBy`, `RepostedBy`, `PostedOn` (PascalCase for nodes/temporal)
- Bluesky API fields: `uri`, `indexedAt`, `author` (camelCase, preserve API structure)
- Metrics: `posts_authored`, `reposts_made`, `reposts_received` (snake_case)

### Authentication
Credentials stored via `bskyr::bs_get_user()` / `bs_get_pass()` (saved auth), then `bs_auth()`. Never hardcode credentials.

## Workflow Commands

### Running Analysis Scripts
1. **Source 0_functions.R first** — establishes libraries, authentication, worker pool
2. **Run scripts in order**: `1_BlueSky_Robust.r` (search+hydrate), then `3_StatnetAnalysis.R` (analyze)
3. **Interactive**: Use RStudio terminal or R Interactive console; scripts source dependencies
4. **SQL**: Execute `.sql` files in SSMS or via ODBC from R (see `DBI::dbSendStatement()`)

### Debugging Hydration Hangs
- Check `wrkrs` value and `Sys.sleep()` logic (should prevent API rate limits)
- Inspect `speirgorm_hydrated_*.parquet` for progress
- Review `reposts_batch_*.csv` file count and size (should increase monotonically)
- If stalled, manually resume: delete checkpoint, increase `batch_size`, or reduce worker count

## Common Pitfalls to Avoid

1. **Don't re-authenticate mid-script**: `bs_auth` is set once; reusing it in loops may cause token expiry
2. **Don't ignore "until" anchor logic**: In `deep_search_posts()`, pushing the `until` parameter backward prevents duplicate fetches
3. **Don't assume all API responses have the same shape**: Use `get_posts_from_resp()` to detect whether posts are in `$posts` or wrapped
4. **Don't parallel-map over huge vectors without chunking**: `furrr` keeps all in-flight futures in memory; use `chunk_vec()` to batch
5. **Don't skip `distinct()` after binding rows**: Row deduplication by URI is essential after batching

## Extending the Project

- **Add new Bluesky queries**: Wrap new `bs_*()` calls with `safe_*()` extractors and retry logic, then integrate into `deep_search_posts()`
- **Add SQL analysis**: Use graph table syntax in `CreateEdgesNodestables.sql`; test queries in SSMS first
- **Add new metrics**: Compute in `statnet` then merge into `nodes` tibble before SQL insert
- **Generate reports**: Use `BluseSkyNetworking.Rmd` as template; knit to HTML for interactive exploration
