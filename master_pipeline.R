# master_pipeline.R
source("0_functions.R")

cat("\n=== STEP 1: Ingest from Bluesky API ===\n")
source("1_BlueSky_Robust.R")

cat("\n=== STEP 2: Load into SQL & build Person/Reposted ===\n")
source("2_load network data into SQL.r")

cat("\n=== STEP 3: Build graph + compute metrics (RevoScaleR) ===\n")
source("3_build_graph_and_metrics.R")

cat("\n=== STEP 4: Visualisation exports ===\n")
source("4_visualisation.R")

cat("\n=== STEP 5: Temporal / TSNA analysis ===\n")
source("5_tsna_dynamic.R")

cat("\nPipeline complete.\n")
