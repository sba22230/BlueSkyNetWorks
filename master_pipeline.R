# master_pipeline.R
source(here::here("0_functions.R"))

preflight_pipeline(
  required_files = c(
    project_path("0_functions.R"),
    project_path("1_BlueSky_Robust.r"),
    project_path("2_load_network_data_into_SQL.R")
  ),
  required_packages = c("arrow", "bskyr", "dplyr", "purrr", "readr")
)

cat("\n=== STEP 1: Ingest from Bluesky API ===\n")
source(project_path("1_BlueSky_Robust.r"))

cat("\n=== STEP 2: Load into SQL & build Person/Reposted ===\n")
source(project_path("2_load_network_data_into_SQL.R"))

cat("\n=== STEP 3: Build graph + compute metrics (RevoScaleR) ===\n")
source(project_path("3_build_graph_and_metrics.R"))

cat("\n=== STEP 4: Visualisation exports ===\n")
source(project_path("4_visualisation.R"))

cat("\n=== STEP 5: Community Analysis ===\n")
source(project_path("5_Communities.R"))

cat("\n=== STEP 6: NLP analysis ===\n")
source(project_path("Model_functions.R"))
source(project_path("5 NLP Analysis Functions.r"))
source(project_path("6_NLP_analysis.R"))

cat("\n=== STEP 7: Text Analysis ===\n")
source(project_path("7_TextAnalysis.R"))

cat("\nPipeline complete.\n")
# --- Example usage (adjust community IDs and paths to your data) ---
# freq_plot      <- plot_author_word_freq(g)
# comm_data      <- get_community_posts(g)
# ViewPostsByDate(comm_data$posts, comm_1 = 6, comm_2 = 9)
# ViewCommunityContrastedByWords(comm_data$totals, comm_data$counts,
#                                comm_data$tidy_posts, comm_1 = 6, comm_2 = 9)
#
# community_graphs <- rxReadObject(ds_Graphs, "Community Graphs")
# timelines        <- plot_community_timelines(community_graphs)
# show_plots(timelines$word_plots, page = 1)
#
# classifier <- build_community_classifier(g, target_community = "1")
#
# sentiment_models <- build_sentiment_models("data/sentimentdataset.csv")
# sentiment_output <- plot_sentiment_distributions(g, sentiment_models)
