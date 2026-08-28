# =============================================================================
# 6_TextAnalysis.R — Text, sentiment, and topic modelling for BlueSkyNetWorks
# This script prepares post text, applies sentiment lexicons, and fits topic
# models using the network's graph and community assignments.
# =============================================================================
# Load shared packages, helper functions, graph-loading logic, and connection
# settings used throughout the analysis.
source(here::here("0_functions.R"))
# =============================================================================
# 1. DATA PREPARATION
# Convert graph data into a text-analysis corpus while preserving the community
# associated with the author of each post.
# =============================================================================

# Set a reproducible seed for any sampling performed while loading the graph.
set.seed(22230)
# Use 1,000 posts by default, while allowing a previously defined `num_posts`
# value to control the analysis when the script is run interactively.
if (!exists("num_posts") || is.null(num_posts)) {
  num_posts <- 15000
}
# Load the graph and separate its vertex and edge attributes into data frames.
g <- get_graph_data(num_posts)
nodes <- igraph::as_data_frame(g, what = "vertices")
edges <- igraph::as_data_frame(g, what = "edges")

cat(
  "\n=== Step 7a: Getting posts data frame ready for text analysis ===\n"
)
# Build one row per edge-level post. The edge's `from` value identifies the
# author, while `document` gives each post a stable ID for later tokenisation.
posts <- data.frame(
  name = edges$from,
  text = edges$text,
  created_at = lubridate::ymd(edges$created_at),
  document = seq_len(nrow(edges))
) |>
  # Attach the author's Louvain community from the node table. Keep all posts
  # even if a matching community is unavailable (`all.x = TRUE`).
  merge(nodes[, c("name", "community")], by = "name", all.x = TRUE)

# Tokenise and clean post text. The returned table has one row per
# document-word pair and retains the document's community membership.
build_tidy_posts <- function(df) {
  # "ireland" / "irish" are corpus-wide ubiquitous (this is an Irish Bluesky
  # community) and are not discriminative for LDA; treat them like stop words.
  custom_stops <- tibble::tibble(
    word = c(
      "speirgorm",
      "spéirgorm",
      "speirghorm",
      "spéirghorm",
      "ireland",
      "irish"
    )
  )

  df |>
    # Exclude reposts so the same text is not counted as an independent authored
    # document in the language analyses.
    dplyr::filter(!stringr::str_detect(text, "^RT")) |>
    dplyr::mutate(
      # Normalize case and replace URLs, mention/hashtag markers, and escaped
      # HTML entities with spaces before tokenisation.
      text = stringr::str_to_lower(text),
      text = stringr::str_replace_all(text, "https?://\\S+|www\\.\\S+", " "),
      text = stringr::str_replace_all(text, "[#@]", " "),
      text = stringr::str_replace_all(text, "&amp;|&lt;|&gt;", " ")
    ) |>
    # Split each post into individual word tokens.
    tidytext::unnest_tokens(word, text, token = "words") |>
    dplyr::filter(
      # Remove standard stop words, apostrophe variants, project-specific
      # corpus words, and one-character tokens.
      !word %in% stop_words$word,
      !word %in% stringr::str_remove_all(stop_words$word, "'"),
      !word %in% custom_stops$word,
      nchar(word) > 1
    ) |>
    # Ensure document IDs use a consistent integer type for joins and matrices.
    dplyr::mutate(document = as.integer(document))
}

# Create the cleaned token corpus used by sentiment analysis and topic models.
tidy_posts <- build_tidy_posts(posts)

# =============================================================================
# 2. CORPUS-LEVEL SENTIMENT ANALYSIS
# Compare the cleaned corpus with several complementary sentiment lexicons.
# =============================================================================
cat(
  "\n=== Step 7b: Text analysis using sentiment lexicons ===\n"
)
# Load each sentiment lexicon once so the same definitions can be reused in
# corpus-level and community-level analyses later in the script.
bing <- get_sentiments("bing")
nrc <- get_sentiments("nrc")
afinn <- get_sentiments("afinn")

# --- Bing: binary positive / negative ----------------------------------------

# Match tokens to Bing's positive/negative labels. The many-to-many relationship
# is explicit because lexicon tables can contain multiple rows per word.
sentiment_analysis_bing <- tidy_posts |>
  inner_join(bing, by = "word", relationship = "many-to-many")

sentiment_summary_bing <- sentiment_analysis_bing |>
  # Count the matched positive and negative words across the complete corpus.
  count(sentiment, sort = TRUE)

# Save a bar chart showing the corpus-wide Bing sentiment counts.
save_graph_svg(
  plot_or_expr = function() {
    counts <- sentiment_summary_bing$n
    names(counts) <- sentiment_summary_bing$sentiment
    barplot(
      counts,
      col = c("#2ca02c", "#d62728"),
      border = NA,
      main = "Sentiment Analysis Using Bing Lexicon",
      xlab = "Sentiment",
      ylab = "Count",
      las = 1
    )
  },
  filename = "bingsentiment.svg",
  folder = "images"
)

# --- NRC: 8 emotions + aggregate positive / negative -------------------------

# NRC supplies eight individual emotions plus aggregate positive and negative
# categories, allowing a broader emotional profile than Bing.
sentiment_analysis <- tidy_posts |>
  inner_join(nrc, by = "word", relationship = "many-to-many")

sentiment_summary <- sentiment_analysis |>
  # Count every matched NRC category for the corpus-level summary.
  count(sentiment, sort = TRUE)

# Save a horizontal bar chart because the NRC category labels are numerous.
save_graph_svg(
  plot_or_expr = function() {
    counts <- sentiment_summary$n
    names(counts) <- sentiment_summary$sentiment
    counts <- counts[order(counts)]
    barplot(
      counts,
      horiz = TRUE,
      col = grDevices::rainbow(length(counts), alpha = 0.7),
      border = NA,
      main = "Sentiment Analysis Using NRC Lexicon",
      xlab = "Count",
      las = 1
    )
  },
  filename = "nrcsentiment.svg",
  folder = project_path("docs", "images")
)

# --- AFINN: numeric sentiment score ------------------------------------------

# AFINN assigns numeric sentiment values to words rather than discrete labels.
sentiment_analysis_afinn <- tidy_posts |>
  inner_join(afinn, by = "word", relationship = "many-to-many")

sentiment_summary_afinn <- sentiment_analysis_afinn |>
  # Count how often each numeric score occurs and sort scores from high to low.
  count(value, name = "count") |>
  arrange(desc(value))

save_graph_svg(
  plot_or_expr = function() {
    counts <- sentiment_summary_afinn$count
    labels <- sentiment_summary_afinn$value
    order_idx <- order(labels)
    counts <- counts[order_idx]
    labels <- labels[order_idx]
    barplot(
      counts,
      names.arg = labels,
      horiz = TRUE,
      col = ifelse(labels >= 0, "#2ca02c", "#d62728"),
      border = NA,
      main = "Sentiment Analysis Using AFINN Lexicon",
      xlab = "Count",
      las = 1
    )
  },
  filename = "afinnsentiment.svg",
  folder = "images"
)

# =============================================================================
# 3. WORD CLOUD (full corpus)
# Visualise the most frequent cleaned tokens in the complete corpus.
# =============================================================================
cat(
  "\n=== Step 7c: Build Word Cloud ===\n"
)
# Count token frequency. The resulting `word` and `n` columns are accepted
# directly by `wordcloud2`.
global_freq <- tidy_posts |>
  count(word, sort = TRUE)
# Configure the word cloud's size, rotation, spacing, and randomized placement.
speirgormWordcloud <- wordcloud2(
  global_freq,
  size = 2,
  minRotation = -pi / 6,
  maxRotation = pi / 6,
  rotateRatio = 0.9,
  gridSize = 3,
  shuffle = TRUE
)
# Export the interactive widget and its supporting files for browser viewing.
saveWidget(
  speirgormWordcloud,
  project_path("docs", "SpeirGormWordcloud.html"),
  selfcontained = FALSE
)
# =============================================================================
# 4. PER-COMMUNITY LDA TOPIC MODELLING
# Fit a separate topic model for each sufficiently large community. This makes
# it possible to inspect the topics that characterize individual communities,
# although topic numbers are not directly comparable across separate models.
# =============================================================================
cat(
  "\n=== Step 7d: Start of Topic Modelling ===\n"
)
# Rebuild the token-level corpus so this topic-modelling section can be run
# independently of the earlier sentiment-analysis section.
tidy_posts <- build_tidy_posts(posts)

# Count distinct documents per community and keep communities with at least ten
# documents. Very small communities do not provide enough text for a stable LDA
# fit.
valid_communities <- tidy_posts |>
  distinct(community, document) |>
  count(community) |>
  filter(n >= 10) |>
  pull(community)

# Convert document-word counts into a document-term matrix (DTM), while
# removing terms that occur in more than `max_df` proportion of documents.
# Common terms such as "ireland" and "irish" can otherwise dominate every
# topic without helping to distinguish topics from one another.
build_dtm <- function(word_counts, max_df = 0.3) {
  # The input has one row per document-word pair, so the number of rows for a
  # word is the number of documents in which that word occurs.
  n_docs <- dplyr::n_distinct(word_counts$document)
  word_counts |>
    # Calculate each term's document frequency as a proportion of the corpus.
    dplyr::group_by(word) |>
    dplyr::filter(dplyr::n() / n_docs <= max_df) |>
    dplyr::ungroup() |>
    # Cast the retained counts into the sparse DTM expected by topicmodels::LDA.
    tidytext::cast_dtm(document, word, n)
}

# Fit one five-topic model per valid community. Each model receives only that
# community's documents, so its topics describe within-community vocabulary.
lda_models <- valid_communities |>
  # Preserve community IDs as names on the resulting list of models.
  set_names() |>
  map(\(comm) {
    # Count words within documents belonging to the current community, then
    # apply the same document-frequency filtering used by every model.
    dtm <- tidy_posts |>
      filter(community == comm) |>
      count(document, word) |>
      build_dtm()
    # Fit five topics and use a fixed seed for reproducible model initialization.
    LDA(dtm, k = 5, control = list(seed = 7843))
  })

# Extract beta values from each model. Beta is the estimated probability of a
# word given a topic; larger beta values identify words most strongly associated
# with that topic.
community_topics <- lda_models |>
  # Iterate over both each model and its community ID so the source community
  # remains attached after all model outputs are combined.
  imap(\(model, comm) {
    tidy(model, matrix = "beta") |>
      mutate(community = as.integer(comm))
  }) |>
  # Combine the per-community beta tables into one long-format table and retain
  # the ten highest-beta terms for each community-topic combination.
  list_rbind() |>
  group_by(community, topic) |>
  slice_max(beta, n = 10) |>
  ungroup()

# Convenience alias: first-community model kept for backward compatibility
lda_model <- lda_models[[1]]

# --- Heatmap: dominant term per topic for the top 6 communities --------------
# Identify the six valid communities with the most documents. These communities
# are used only for the visual summary below; the full topic table is retained
# for the database export later in the section.
cat(
  "\n=== Step 7e: Top 6 Topics per Community ===\n"
)
top6_comms <- tidy_posts |>
  # Count documents rather than tokens so long posts do not receive extra
  # weight when communities are ranked.
  distinct(community, document) |>
  count(community, sort = TRUE) |>
  filter(community %in% valid_communities) |>
  slice_head(n = 6) |>
  pull(community)

# For each community, choose the highest-beta term for each topic, while
# preventing a term from being reused by another topic in that community. This
# avoids a single common word winning every topic label in the heatmap.
pick_unique_terms <- function(df) {
  # Process topics in numeric order and track terms already assigned.
  topics <- sort(unique(df$topic))
  used <- character(0)

  purrr::map(topics, \(t) {
    # Select the strongest unused term for the current topic.
    best <- dplyr::filter(df, topic == t, !term %in% used) |>
      dplyr::slice_max(beta, n = 1)
    # Add the selected term to the exclusion list for later topics.
    used <<- c(used, best$term)
    best
  }) |>
    purrr::list_rbind()
}

### TODO: fix the code below to show only 1 topic per square
# Keep the selected communities, apply the unique-term helper within each
# community, and reshape labels for plotting. Each tile represents one topic
# within one community; the text shows that topic's selected representative
# term and the fill shows its beta value.
community_topics |>
  filter(community %in% top6_comms) |>
  group_by(community) |>
  # `group_modify()` calls the helper independently for each community.
  group_modify(~ pick_unique_terms(.x)) |>
  ungroup() |>
  mutate(
    community = factor(paste("Comm", community)),
    topic = paste("Topic", topic)
  ) |>
  # Draw one tile per community-topic combination and print the representative
  # term on top of the tile.
  ggplot(aes(x = topic, y = community, fill = beta)) +
  geom_tile(color = "white") +
  geom_text(aes(label = term), size = 3) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "Dominant term per LDA topic by community",
    subtitle = "Top 6 communities by document count — one unique term per topic",
    x = NULL,
    y = NULL,
    fill = "Beta"
  ) +
  theme_minimal()

community_sql <- RxSqlServerData(
  table = "communityTopics",
  connectionString = connStr
)
# Export the complete top-term table, including communities and terms not shown
# in the six-community heatmap, to the SQL Server table.
rxDataStep(community_topics, community_sql, overwrite = TRUE)

# --- Gamma: mean document-topic proportion per community ---------------------
# Gamma is the proportion of each document assigned to each topic. This section
# averages those document-level proportions to create a topic profile per
# community.
cat(
  "\n=== Step 7f: Gamma: mean document-topic proportion per community ===\n"
)
community_gamma <- lda_models |>
  # Each community has its own LDA model, so calculate its gamma summary before
  # combining the results into one table.
  imap(\(model, comm) {
    tidy(model, matrix = "gamma") |>
      # Average document-topic proportions within the current community.
      summarise(mean_gamma = mean(gamma), .by = topic) |>
      mutate(community = as.integer(comm))
  }) |>
  list_rbind()

# Identify each community's strongest topic and use it to create a grouped,
# readable y-axis order for the heatmap.
comm_order <- community_gamma |>
  group_by(community) |>
  slice_max(mean_gamma, n = 1) |>
  ungroup() |>
  arrange(topic, desc(mean_gamma)) |>
  pull(community)

# Build human-readable labels from the strongest beta terms. Because the topics
# were fitted independently for each community, these labels are approximate
# and are mainly intended to make the heatmap easier to read.
make_topic_labels <- function(beta_df, n_terms = 1) {
  # Sum beta across communities so each topic's label is based on corpus-wide
  # term importance rather than on an arbitrary single community.
  agg <- beta_df |>
    dplyr::group_by(topic, term) |>
    dplyr::summarise(total_beta = sum(beta), .groups = "drop")

  # Track terms already used by an earlier topic label.
  used <- character(0)

  # Let the clearest topic claim its strongest term first, reducing duplicate
  # labels when several topics share common vocabulary.
  topic_peak <- agg |>
    dplyr::group_by(topic) |>
    dplyr::slice_max(total_beta, n = 1) |>
    dplyr::arrange(dplyr::desc(total_beta)) |>
    dplyr::pull(topic)

  purrr::map(topic_peak, \(t) {
    # Exclude terms already claimed by a higher-priority topic.
    best <- dplyr::filter(agg, topic == t, !term %in% used) |>
      dplyr::slice_max(total_beta, n = n_terms)

    # Update the enclosing `used` vector so the next iteration sees this term.
    used <<- c(used, best$term)

    tibble::tibble(
      topic = t,
      # Include the topic number and wrap long labels for the x-axis.
      label = stringr::str_wrap(
        paste0("T", t, ": ", paste(unique(best$term), collapse = " / ")),
        width = 18
      )
    )
  }) |>
    purrr::list_rbind() |>
    # Restore numeric topic order, then create a named vector for `recode()`.
    dplyr::arrange(topic) |>
    (\(d) setNames(d$label, as.character(d$topic)))()
}

topic_labels <- make_topic_labels(community_topics)

# Apply the community and topic orders, replace numeric topic IDs with readable
# labels, and draw a heatmap of mean topic concentration.
community_gamma |>
  mutate(
    community = factor(community, levels = comm_order),
    topic_name = recode(as.character(topic), !!!topic_labels),
    topic_name = factor(topic_name, levels = topic_labels)
  ) |>
  # Darker tiles represent a larger mean document-topic proportion.
  ggplot(aes(x = topic_name, y = community, fill = mean_gamma)) +
  geom_tile(color = "white", linewidth = 0.4) +
  scale_fill_gradient(
    low = "white",
    high = "#2c6fad",
    # Format gamma values as percentages because they range from 0 to 1.
    labels = scales::percent_format()
  ) +
  labs(
    title = "Community-level topic concentration (gamma)",
    subtitle = "Mean document-topic proportion — topics labelled by dominant terms\n(Note: topics fitted independently per community; labels are approximate)",
    x = NULL,
    y = "Community",
    fill = "Mean γ"
  ) +
  theme_minimal() +
  theme(
    # Keep labels readable while removing visual clutter from the heatmap.
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 8, angle = 20, hjust = 1),
    panel.grid = element_blank()
  )

# =============================================================================
# 5. GLOBAL LDA — single corpus-wide model for comparable community profiles
# Fit one LDA model to the complete corpus rather than fitting a separate model
# for each community. Because every community is projected onto the same five
# topics, their topic profiles can be compared directly.
# =============================================================================
cat(
  "\n=== Step 7g: Gamma: mean document-topic proportion for global LDA ===\n"
)
# Count each word once per document and convert the result to a document-term
# matrix. `build_dtm()` also removes words occurring in too many documents.
global_dtm <- tidy_posts |>
  count(document, word) |>
  build_dtm()

# Fit a reproducible five-topic global LDA model. The seed ensures that repeated
# runs use the same random starting state when the input data are unchanged.
global_lda <- LDA(global_dtm, k = 5, control = list(seed = 7843))

# Create a lookup from each document ID to its community. The LDA gamma output
# is keyed by document, so this lookup is needed to aggregate topic proportions
# back to communities.
doc_community <- tidy_posts |>
  distinct(document, community)

# Extract gamma, the document-topic proportion estimated by LDA, and attach the
# community for each document. Keep only valid communities, which were defined
# earlier as having at least 10 documents.
global_gamma <- tidy(global_lda, matrix = "gamma") |>
  mutate(document = as.integer(document)) |>
  left_join(doc_community, by = "document") |>
  filter(!is.na(community), community %in% valid_communities) |>
  # Average the document-level topic proportions within each community. A high
  # mean gamma means that topic accounts for a larger share of that community's
  # documents on average.
  group_by(community, topic) |>
  summarise(mean_gamma = mean(gamma), .groups = "drop")

# Find each community's strongest topic and order communities by that topic and
# its strength. This groups communities with similar dominant topics together
# on the heatmap's y-axis.
comm_order_global <- global_gamma |>
  group_by(community) |>
  slice_max(mean_gamma, n = 1) |>
  ungroup() |>
  arrange(topic, desc(mean_gamma)) |>
  pull(community)

# Generate readable labels for the five topics from their highest-beta terms.
# Beta describes how strongly each word is associated with a topic.
global_topic_labels <- tidy(global_lda, matrix = "beta") |>
  make_topic_labels(n_terms = 1)

# Prepare the heatmap data without changing the underlying numeric values.
# Factors preserve the intended community order, while topic names replace
# opaque topic numbers with labels based on representative words.
global_gamma |>
  mutate(
    community = factor(community, levels = comm_order_global),
    topic_name = factor(
      recode(as.character(topic), !!!global_topic_labels),
      levels = global_topic_labels
    )
  ) |>
  # Each tile represents one community-topic combination; darker blue indicates
  # a larger average document-topic proportion (mean gamma).
  ggplot(aes(x = topic_name, y = community, fill = mean_gamma)) +
  geom_tile(color = "white", linewidth = 0.4) +
  scale_fill_gradient(
    low = "white",
    high = "#2c6fad",
    # Display gamma values as percentages because each document's topic
    # proportions are on a 0-to-1 scale.
    labels = scales::percent_format()
  ) +
  labs(
    title = "Community-level topic concentration — global LDA",
    subtitle = "Mean document-topic proportion (gamma) from a single corpus-wide model",
    x = NULL,
    y = "Community",
    fill = "Mean γ"
  ) +
  theme_minimal() +
  theme(
    # Reduce label size and remove grid lines so the many tiles remain legible.
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 8, angle = 20, hjust = 1),
    panel.grid = element_blank()
  )

# =============================================================================
# 6. COMMUNITY-LEVEL SENTIMENT
# Summarise sentiment words by Louvain community so sentiment can be compared
# across groups in the network rather than only across the full corpus.
# =============================================================================
cat(
  "\n=== Step 7h: Community-level sentiment analysis ===\n"
)
# --- Bing: net positive / negative per community -----------------------------
# Match each token in the tidy corpus to the Bing lexicon. The many-to-many
# relationship is intentional because a token can have multiple lexicon rows.
community_sentiment <- tidy_posts |>
  inner_join(bing, by = "word", relationship = "many-to-many") |>
  # Count positive and negative lexicon matches within each community.
  count(community, sentiment) |>
  # Make one column for each sentiment category, filling absent categories
  # with zero so subtraction is defined for every community.
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |>
  # Positive values indicate a net positive balance; negative values indicate
  # a net negative balance.
  mutate(net_sentiment = positive - negative)

# --- NRC wide: all emotions + net sentiment + dominant emotion per community -
# Repeat the process with NRC, retaining all emotion counts in a wide table for
# downstream tabular analysis.
community_sentiment_nrc_wide <- tidy_posts |>
  inner_join(nrc, by = "word", relationship = "many-to-many") |>
  count(community, sentiment) |>
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |>
  mutate(
    # NRC includes aggregate positive and negative categories, allowing the
    # same net sentiment calculation as the Bing summary.
    net_sentiment = positive - negative,
    # For each community, inspect only the eight individual emotions. Exclude
    # NRC's aggregate positive and negative columns when finding the maximum.
    dominant_emotion = pmap_chr(
      pick(anger, anticipation, disgust, fear, joy, sadness, surprise, trust),
      # Return the name of the emotion with the highest matched-word count.
      \(...) names(which.max(c(...)))
    )
  )

# --- NRC long: emotion profile plot (8 pure emotions, proportional) ----------

# Define the individual NRC emotions used in the profile plot. The aggregate
# positive and negative categories are deliberately left out.
emotions <- c(
  "anger",
  "anticipation",
  "disgust",
  "fear",
  "joy",
  "sadness",
  "surprise",
  "trust"
)

# Use a stable, named color for each emotion so the meaning is consistent across
# the plot and any later visualisations that reuse this palette.
emotion_palette <- c(
  anger = "#d62728",
  anticipation = "#ff7f0e",
  disgust = "#8c564b",
  fear = "#9467bd",
  joy = "#2ca02c",
  sadness = "#1f77b4",
  surprise = "#17becf",
  trust = "#bcbd22"
)

# Create a long-form table containing one row per community-emotion pair. Only
# the eight pure emotions are retained; aggregate positive/negative rows are
# excluded from this profile.
community_sentiment_nrc <- tidy_posts |>
  inner_join(nrc, by = "word", relationship = "many-to-many") |>
  filter(sentiment %in% emotions) |>
  count(community, sentiment)

# Select the 15 communities with the most NRC-matched emotion words. This keeps
# the chart focused on communities with enough matched text to compare.
top_communities <- community_sentiment_nrc |>
  count(community, wt = n, sort = TRUE) |>
  slice_head(n = 15) |>
  pull(community)

# Order the selected communities by total matched-word count. With ggplot2's
# factor ordering, the largest community appears at the bottom of the chart.
community_order <- community_sentiment_nrc |>
  filter(community %in% top_communities) |>
  count(community, wt = n) |>
  arrange(n) |>
  pull(community)

community_sentiment_nrc |>
  filter(community %in% top_communities) |>
  mutate(
    # Apply the chosen display order and a fixed left-to-right emotion order.
    community = factor(community, levels = community_order),
    sentiment = factor(sentiment, levels = emotions)
  ) |>
  # A proportional stacked bar shows each community's emotional composition,
  # making shares comparable even when total word counts differ.
  ggplot(aes(x = n, y = community, fill = sentiment)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = emotion_palette) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(
    title = "Emotion profile by community (NRC lexicon)",
    subtitle = "Top 15 communities by matched word count — proportional share",
    x = "Share of emotion words",
    y = "Community",
    fill = "Emotion"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# =============================================================================
# 7. TOPIC SIMILARITY VS. INTER-COMMUNITY NETWORK DENSITY
# Do topically similar communities also share more structural ties?
# Requires: comm_louvain (produced in the network analysis script)
#
# This section compares two properties for every pair of communities:
#   1. How similar their global-LDA topic profiles are.
#   2. How densely they are connected by network edges.
# The final scatterplot and correlations test whether these properties tend
# to increase together.
# =============================================================================
cat(
  "\n=== Step 7i: Topical Similarity vs. Inter-Community Network Density ===\n"
)
# Build a lookup table containing every node and its Louvain community. The
# community counts are needed later to calculate the number of possible ties
# between each pair of communities.
mem_tbl <- tibble::tibble(
  node = igraph::V(g)$name,
  community = igraph::V(g)$community
)
comm_sizes_tbl <- mem_tbl |> count(community, name = "size")

# Keep only communities that are present in both data sources: the graph's
# Louvain membership data and the global-LDA gamma results.
shared_comms <- intersect(
  unique(global_gamma$community),
  comm_sizes_tbl$community
)
top_comms_focal <- comm_sizes_tbl |>
  filter(community %in% shared_comms) |>
  # Focus the comparison on the 30 largest eligible communities so the plot
  # remains interpretable and is less dominated by very small groups.
  slice_max(size, n = 30) |>
  pull(community)

# --- Cosine similarity of global gamma profiles ------------------------------
# Each community is represented by a vector of mean topic proportions from
# global_gamma. Cosine similarity compares the direction of two vectors, so a
# high value means their relative topic profiles are similar.

cosine_sim <- function(mat) {
  # Calculate the Euclidean length of each community's topic vector.
  norms <- sqrt(rowSums(mat^2))
  # Normalize each row to unit length, then multiply by its transpose. The
  # resulting matrix contains one cosine-similarity value for every pair.
  mat_n <- mat / norms
  mat_n %*% t(mat_n)
}

# Convert the long gamma table into a matrix with one row per community and
# one column per topic. Missing topic values are treated as zero.
gamma_mat <- global_gamma |>
  filter(community %in% top_comms_focal) |>
  pivot_wider(names_from = topic, values_from = mean_gamma, values_fill = 0) |>
  tibble::column_to_rownames("community") |>
  as.matrix()

cos_mat <- cosine_sim(gamma_mat)

# --- Cross-community edge density for each community pair --------------------
# Extract graph edges as pairs of vertex indices and use the vertex attributes
# to translate those indices into community IDs.

el <- igraph::as_edgelist(g, names = FALSE)
node_comm <- igraph::V(g)$community

edge_comm_pairs <- tibble::tibble(
  c1 = node_comm[el[, 1]],
  c2 = node_comm[el[, 2]]
) |>
  # Remove within-community edges and edges involving communities outside the
  # selected focal set.
  filter(c1 != c2, c1 %in% top_comms_focal, c2 %in% top_comms_focal) |>
  # Store each unordered pair in a consistent order, so A-B and B-A are
  # counted together rather than treated as separate community pairs.
  mutate(pair_a = pmin(c1, c2), pair_b = pmax(c1, c2)) |>
  # Count observed cross-community edges for each pair.
  count(pair_a, pair_b, name = "cross_edges") |>
  mutate(
    # Look up the number of nodes in each community.
    size_a = comm_sizes_tbl$size[match(pair_a, comm_sizes_tbl$community)],
    size_b = comm_sizes_tbl$size[match(pair_b, comm_sizes_tbl$community)],
    # For a pair of community sizes A and B, there are A * B possible
    # cross-community node pairs. Density is observed ties divided by that
    # maximum possible count.
    possible = size_a * size_b,
    inter_density = cross_edges / possible
  )

# --- Join cosine similarity to inter-community density -----------------------

# Convert the cosine-similarity matrix back to a long table. Keep only one
# copy of each unordered pair by retaining rows where community A < community B.
cos_long <- as.data.frame(as.table(cos_mat)) |>
  rename(comm_a = Var1, comm_b = Var2, cosine = Freq) |>
  mutate(comm_a = as.integer(comm_a), comm_b = as.integer(comm_b)) |>
  filter(comm_a < comm_b)

comparison <- cos_long |>
  # Attach the observed cross-edge count and density to each community pair.
  # A left join preserves pairs that have no cross-community edges.
  left_join(
    rename(edge_comm_pairs, comm_a = pair_a, comm_b = pair_b),
    by = c("comm_a", "comm_b")
  ) |>
  mutate(
    # Missing densities represent pairs with no observed cross-edges, so treat
    # them as zero and retain a flag for the plot and the second correlation.
    inter_density = tidyr::replace_na(inter_density, 0),
    has_edge = inter_density > 0
  )

# Calculate rank-based correlations because edge densities are typically
# strongly skewed. `cor_all` includes every pair, including zero-edge pairs;
# `cor_edge` is restricted to pairs with at least one observed cross-edge.
cor_all <- cor(comparison$cosine, comparison$inter_density, method = "spearman")
cor_edge <- cor(
  comparison$cosine[comparison$has_edge],
  comparison$inter_density[comparison$has_edge],
  method = "spearman"
)

# --- Scatter: cosine similarity vs. inter-community edge density -------------
# Plot topic similarity against edge density. Points are colored to distinguish
# pairs with no observed cross-edges from pairs that do share network ties.
ggplot(comparison, aes(x = cosine, y = inter_density)) +
  geom_point(aes(color = has_edge), alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8, color = "firebrick") +
  scale_color_manual(
    values = c("FALSE" = "grey70", "TRUE" = "#2c6fad"),
    labels = c("No cross-edges", "Has cross-edges")
  ) +
  scale_y_continuous(labels = scales::label_scientific()) +
  # Add the sample definition and both correlation results to the subtitle.
  labs(
    title = "Topic similarity vs. inter-community edge density",
    subtitle = sprintf(
      "Top 30 communities by size · Spearman ρ = %.3f (all pairs), %.3f (edges only)",
      cor_all,
      cor_edge
    ),
    x = "Cosine similarity of global γ profiles",
    y = "Inter-community edge density",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

# --- Paired heatmaps: topic similarity vs. edge density ----------------------
cat(
  "\n=== Step 7j: Paired heatmaps: topic similarity vs. edge density ===\n"
)
# Find each community's dominant global-LDA topic, then order communities by
# that topic and its strength so communities with similar topics appear near
# one another in both heatmaps.
dom_topic_order <- global_gamma |>
  filter(community %in% top_comms_focal) |>
  group_by(community) |>
  slice_max(mean_gamma, n = 1) |>
  ungroup() |>
  arrange(topic, desc(mean_gamma)) |>
  pull(community)

# Convert the numeric community IDs to character labels for factor levels.
# The factor levels preserve the ordering calculated above in ggplot2.
fac_levels <- as.character(dom_topic_order)

# The pairwise tables contain only one row for each unordered pair (for example,
# A-B, but not B-A). Add a reversed copy so every cell in the heatmap has data.
make_symmetric <- function(df, col_a, col_b) {
  bind_rows(df, rename(df, !!col_a := !!col_b, !!col_b := !!col_a))
}

# Apply the shared community order and mirror the cosine-similarity pairs.
cos_sym <- cos_long |>
  mutate(
    comm_a = factor(comm_a, levels = fac_levels),
    comm_b = factor(comm_b, levels = fac_levels)
  ) |>
  make_symmetric("comm_a", "comm_b")

# Apply the same order and mirror the edge-density pairs. Missing pairs were
# already assigned zero density in `comparison` above.
edge_sym <- comparison |>
  mutate(
    comm_a = factor(comm_a, levels = fac_levels),
    comm_b = factor(comm_b, levels = fac_levels)
  ) |>
  make_symmetric("comm_a", "comm_b")

# Heatmap 1: darker blue tiles mean communities have more similar global-LDA
# topic profiles, based on cosine similarity of their gamma vectors.
p_cos <- ggplot(cos_sym, aes(x = comm_b, y = comm_a, fill = cosine)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "#2c6fad") +
  labs(
    title = "Topic cosine similarity (global γ)",
    x = NULL,
    y = "Community",
    fill = "Cosine"
  ) +
  theme_minimal(base_size = 9) +
  theme(axis.text = element_text(size = 6), panel.grid = element_blank())

# Heatmap 2: darker red tiles mean a higher density of network edges between
# the two communities. log1p() compresses the highly skewed density values,
# while multiplying by 1e5 makes small densities easier to distinguish.
p_edge <- ggplot(
  edge_sym,
  aes(x = comm_b, y = comm_a, fill = log1p(inter_density * 1e5))
) +
  geom_tile() +
  scale_fill_gradient(
    low = "white",
    high = "#b5201a",
    name = "log(density+1)"
  ) +
  labs(
    title = "Inter-community edge density",
    x = "Community",
    y = NULL
  ) +
  theme_minimal(base_size = 9) +
  theme(axis.text = element_text(size = 6), panel.grid = element_blank())

# Stack the two heatmaps vertically for visual comparison using patchwork.
p_cos / p_edge
