# master_pipeline.R
source("0_functions.R")

cat("\n=== STEP 1: Ingest from Bluesky API ===\n")
source("1_BlueSky_Robust.R")

cat("\n=== STEP 2: Load into SQL & build Person/Reposted ===\n")
source("2_load_network_data_into_SQL.R")

cat("\n=== STEP 3: Build graph + compute metrics (RevoScaleR) ===\n")
source("3_build_graph_and_metrics.R")

cat("\n=== STEP 4: Visualisation exports ===\n")
source("4_visualisation.R")

cat("\n=== STEP 5: NLP analysis ===\n")
source("Model_functions.R")
source("5_NLP_analysis.R")

cat("\n=== STEP 6: Text analysis ===\n")

source("6_TextAnalysis.R")


cat("\nPipeline complete.\n")
