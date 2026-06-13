# BlueSkyNetWorks

An R-based social network analysis platform for collecting, processing, and analysing interaction data from the [Bluesky](https://bsky.app) social media platform. The project tracks discussions around the **#Speirgorm** hashtag, constructing directed networks of reposts and replies for graph-theoretic analysis, community detection, and NLP-based post analysis.

---

## Overview

The pipeline:

1. Queries the Bluesky API for posts matching `#Speirgorm`
2. Hydrates each post with repost and reply thread data
3. Ingests raw data into a SQL Server graph database
4. Builds directed network graphs and computes centrality metrics
5. Exports interactive and static visualisations
6. Runs temporal and sentiment-based NLP analysis

---

## Project Structure

```
BlueSkyNetWorks/
├── master_pipeline.R               # Orchestrates all pipeline stages
├── 0_functions.R                   # Libraries, authentication, utility functions
├── 1_BlueSky_Robust.r              # API data collection and hydration
├── 2_load network data into SQL.r  # SQL Server ingestion and graph table setup
├── 3_build_graph_and_metrics.R     # igraph/statnet construction and metrics
├── 3a_NetworkVisualisation.r       # Layout computation and community colouring
├── 3b_TSNA_Analysis.r              # Temporal network analysis
├── 4_visualisation.R               # visNetwork, ggraph, GEXF export
├── 5_PostNLP_analysis.R            # TidyText NLP and post frequency analysis
├── 6_TextAnalysis.R                # Sentiment lexicon analysis (Bing, NRC, AFINN)
├── Model_functions.R               # Sentiment scoring (logistic regression, Naive Bayes)
├── BluseSkyNetworking.Rmd          # Interactive R Notebook for exploration
├── SQL/                            # T-SQL schema, stored procedures, views
├── data/                           # Parquet and CSV data artifacts
└── Report/_site/graphs/                         # Exported graph files (Parquet, GEXF, GraphML, SVG, HTML)
```

---

## Pipeline Stages

### Stage 1 — Data Collection (`1_BlueSky_Robust.r`)

- Searches Bluesky API for `#Speirgorm` posts using cursor-based pagination
- Checkpoints every ~1,000 rows to `data/speirgorm_posts.parquet`
- Hydrates posts in parallel batches to retrieve repost and thread data
- Outputs: `speirgorm_posts.parquet`, `speirgorm_reposts.parquet`, `speirgorm_threads.parquet`

### Stage 2 — SQL Ingestion (`2_load network data into SQL.r`)

- Loads parquet data into a SQL Server database (`BlueSkyNet`)
- Creates SQL Server graph tables: `Person` (nodes) and `Reposted` (edges)
- Uses RevoScaleR for parallel remote compute context

### Stage 3 — Graph Analysis (`3_build_graph_and_metrics.R`)

- Constructs directed graphs using **igraph** and **statnet**
- Computes node-level metrics: in/out-degree, betweenness, closeness, eigenvector centrality, PageRank
- Computes network-level metrics: density, diameter, average path length, clustering coefficient, triad census
- Detects communities via the Walktrap algorithm
- Outputs: `speirgorm_edges.parquet`, `speirgorm_nodes.parquet`, `network_metrics.parquet`

### Stage 4 — Visualisation (`4_visualisation.R`, `3a_NetworkVisualisation.r`)

- Generates interactive network visualisations with **visNetwork**
- Generates static plots with **ggraph** (Fruchterman-Reingold, Kamada-Kawai layouts)
- Exports to GEXF (Gephi), GraphML, SVG, and HTML formats

### Stage 5 — NLP Analysis (`5_PostNLP_analysis.R`, `6_TextAnalysis.R`)

- Tokenises post text and computes word frequencies per community
- Runs sentiment analysis using Bing, NRC, and AFINN lexicons
- Analyses posting frequency and temporal dynamics

---

## Getting Started

### Prerequisites

- **R** ≥ 4.1
- **SQL Server** (local instance; database `BlueSkyNet`)
- A **Bluesky** account for API authentication

### R Package Dependencies

```r
install.packages(c(
  "bskyr",       # Bluesky API client
  "arrow",       # Parquet I/O
  "igraph",      # Network analysis
  "statnet",     # Network analysis (statnet suite)
  "sna",
  "intergraph",  # igraph <-> statnet conversion
  "dplyr", "tidyr", "purrr", "furrr",
  "ggraph", "visNetwork", "ggplot2",
  "tidytext", "textdata", "tm",
  "DBI", "odbc",
  "RevoScaleR"   # For SQL Server parallel compute (Microsoft R)
))
```

> **Note:** `RevoScaleR` is part of Microsoft R Open / SQL Server Machine Learning Services and is not available on CRAN.

### Authentication

Bluesky credentials are managed interactively via `bskyr`. On first run, `0_functions.R` will prompt for your handle and app password. Credentials are stored securely and reused throughout the session — do not hardcode them.

SQL Server uses Windows Authentication by default. Adjust the connection string in `0_functions.R` if SQL authentication is required.

### Running the Pipeline

```r
# Run all stages sequentially
source("master_pipeline.R")
```

Or run individual stages:

```r
source("0_functions.R")                      # Always source first
source("1_BlueSky_Robust.r")                 # Collect data
source("2_load network data into SQL.r")     # Ingest to SQL
source("3_build_graph_and_metrics.R")        # Build graph & metrics
source("4_visualisation.R")                  # Export visualisations
source("5_PostNLP_analysis.R")               # NLP analysis
```

For interactive exploration, open and knit `BluseSkyNetworking.Rmd`.

---

## Data Artifacts

| File | Size | Description |
|------|------|-------------|
| `data/speirgorm_posts.parquet` | 5.6 MB | Raw posts matching `#Speirgorm` |
| `data/speirgorm_reposts.parquet` | 13.3 MB | Repost relationships |
| `data/speirgorm_threads.parquet` | 1.6 MB | Reply thread data |
| `Report/_site/graphs/speirgorm_edges.parquet` | — | Network edges with metadata |
| `Report/_site/graphs/speirgorm_nodes.parquet` | — | Network nodes with computed metrics |
| `Report/_site/graphs/visnetwork_export.gexf` | — | Gephi-compatible graph export |
| `Report/_site/graphs/visnetwork_tmp.html` | — | Interactive network (browser) |

---

## SQL Schema

The `SQL/` directory contains T-SQL scripts for the `BlueSkyNet` database:

| File | Purpose |
|------|---------|
| `Tables.sql` | Creates `posts_raw`, `reposts_raw`, `Person`, `Reposted` graph tables |
| `StoredProcedures.sql` | Procedures for graph metric computation |
| `GraphMetrics.sql` | SQL-level centrality calculations |
| `NetworkLevelMetrics.sql` | Network-wide aggregations |
| `View.sql` | Summary views for posts and users |
| `Build Network Table.sql` | Graph projection queries |

---

## Design Patterns

**Resilient API collection** — `retry()` with exponential backoff and jitter protects against rate limits. Bad URIs (HTTP 400) are skipped; transient errors (HTTP 502) are queued for retry.

**Checkpointing** — Progress is saved at regular intervals so long-running jobs can resume from the last checkpoint without reprocessing.

**Parallel hydration** — `furrr::future_map_dfr()` with up to 70% of available cores batches repost and thread fetching efficiently.

**Safe extraction** — `safe_chr()`, `safe_int()`, and similar helpers handle missing or inconsistently shaped API responses without crashing.

---

## Outputs

- **Interactive HTML** network visualisation (visNetwork)
- **Static SVG** plots of full network and per-community subgraphs
- **GEXF / GraphML** exports for Gephi or Cytoscape
- **Animated GIF / MP4** of temporal network evolution
- **Sentiment and frequency plots** per community

---

## Naming Conventions

| Context | Convention | Example |
|---------|-----------|---------|
| Database / graph fields | PascalCase | `PostedBy`, `RepostedOn` |
| Bluesky API fields | camelCase | `uri`, `indexedAt` |
| Computed metrics | snake_case | `reposts_received`, `betweenness_centrality` |
