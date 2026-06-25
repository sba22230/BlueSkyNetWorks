# =============================================================================
# 6_TextAnalysis.R — Text, sentiment, and topic modelling for BlueSkyNetWorks
# =============================================================================


# =============================================================================
# 1. DATA PREPARATION
# =============================================================================

# Extract edge-level posts and attach community membership from the graph object
nodes <- igraph::as_data_frame(g, what = "vertices")
edges <- igraph::as_data_frame(g, what = "edges")

posts <- data.frame(
  name       = edges$from,
  text       = edges$text,
  created_at = lubridate::ymd(edges$created_at),
  document   = seq_len(nrow(edges))
) |>
  merge(nodes[, c("name", "community")], by = "name", all.x = TRUE)

# Tokenise, clean, and remove stop words.
# Returns one row per (document, word) with community membership preserved.
build_tidy_posts <- function(df) {
  custom_stops <- tibble::tibble(
    word = c("speirgorm", "spéirgorm", "speirghorm", "spéirghorm")
  )

  df |>
    dplyr::filter(!stringr::str_detect(text, "^RT")) |>
    dplyr::mutate(
      text = stringr::str_to_lower(text),
      text = stringr::str_replace_all(text, "https?://\\S+|www\\.\\S+", " "),
      text = stringr::str_replace_all(text, "[#@]", " "),
      text = stringr::str_replace_all(text, "&amp;|&lt;|&gt;", " ")
    ) |>
    tidytext::unnest_tokens(word, text, token = "words") |>
    dplyr::filter(
      !word %in% stop_words$word,
      !word %in% stringr::str_remove_all(stop_words$word, "'"),
      !word %in% custom_stops$word,
      nchar(word) > 1
    ) |>
    dplyr::mutate(document = as.integer(document))
}

tidy_posts <- build_tidy_posts(posts)

# =============================================================================
# 2. CORPUS-LEVEL SENTIMENT ANALYSIS
# =============================================================================

# Load sentiment lexicons once; reused across sections
bing  <- get_sentiments("bing")
nrc   <- get_sentiments("nrc")
afinn <- get_sentiments("afinn")

# --- Bing: binary positive / negative ----------------------------------------

sentiment_analysis_bing <- tidy_posts |>
  inner_join(bing, by = "word")

sentiment_summary_bing <- sentiment_analysis_bing |>
  count(sentiment, sort = TRUE)

bingsentiment <- ggplot(sentiment_summary_bing, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Sentiment Analysis Using Bing Lexicon",
    x = "Sentiment",
    y = "Count"
  ) +
  theme_minimal()

save_graph_svg(bingsentiment, "bingsentiment.svg", folder = "docs/images")

# --- NRC: 8 emotions + aggregate positive / negative -------------------------

sentiment_analysis <- tidy_posts |>
  inner_join(nrc, by = "word", relationship = "many-to-many")

sentiment_summary <- sentiment_analysis |>
  count(sentiment, sort = TRUE)

nrcsentiment <- ggplot(
  sentiment_summary,
  aes(x = n, y = reorder(sentiment, n), fill = sentiment)
) +
  geom_bar(stat = "identity") +
  labs(
    title = "Sentiment Analysis Using NRC Lexicon",
    x = "Count",
    y = "Sentiment"
  ) +
  theme_minimal()

save_graph_svg(nrcsentiment, filename = "nrcsentiment.svg", folder = "docs/images")

# --- AFINN: numeric sentiment score ------------------------------------------

sentiment_analysis_afinn <- tidy_posts |>
  inner_join(afinn, by = "word")

sentiment_summary_afinn <- sentiment_analysis_afinn |>
  count(value, name = "count") |>
  arrange(desc(value))

afinnsentiment <- ggplot(
  sentiment_summary_afinn,
  aes(x = count, y = factor(value), fill = factor(value))
) +
  geom_bar(stat = "identity") +
  labs(
    title = "Sentiment Analysis Using AFINN Lexicon",
    x = "Count",
    y = "Sentiment Score"
  ) +
  theme_minimal()

save_graph_svg(afinnsentiment, filename = "afinnsentiment.svg", folder = "docs/images")

# =============================================================================
# 3. WORD CLOUD (full corpus)
# =============================================================================

# word / n format is accepted directly by wordcloud2
global_freq <- tidy_posts |>
  count(word, sort = TRUE)
#speirgormWordcloud <- 
  wordcloud2(global_freq, size = 2, minRotation = -pi/6, maxRotation = pi/6, rotateRatio = 0.9, gridSize = 3, shuffle = TRUE)
# save_graph_svg(speirgormWordcloud, filename = "Speirgorm_wordcloud.svg", folder = "docs/images")
# =============================================================================
# 4. PER-COMMUNITY LDA TOPIC MODELLING
# =============================================================================

# Rebuild tidy_posts so this section can be run independently of section 2
tidy_posts <- build_tidy_posts(posts)

valid_communities <- tidy_posts |>
  distinct(community, document) |>
  count(community) |>
  filter(n >= 10) |>
  pull(community)

# Build a DTM from a word-count data frame, removing terms that appear in more
# than `max_df` proportion of documents. High-document-frequency words (e.g.
# "ireland", "irish") dominate every topic and make LDA results uninformative.
build_dtm <- function(word_counts, max_df = 0.3) {
  n_docs <- dplyr::n_distinct(word_counts$document)
  word_counts |>
    dplyr::group_by(word) |>
    dplyr::filter(dplyr::n() / n_docs <= max_df) |>
    dplyr::ungroup() |>
    tidytext::cast_dtm(document, word, n)
}

# Build a document-term matrix and fit a 5-topic LDA model for each community
lda_models <- valid_communities |>
  set_names() |>
  map(\(comm) {
    dtm <- tidy_posts |>
      filter(community == comm) |>
      count(document, word) |>
      build_dtm()
    LDA(dtm, k = 5, control = list(seed = 7843))
  })

# Top-10 beta (word-topic probability) terms per topic for every community
community_topics <- lda_models |>
  imap(\(model, comm) {
    tidy(model, matrix = "beta") |>
      mutate(community = as.integer(comm))
  }) |>
  list_rbind() |>
  group_by(community, topic) |>
  slice_max(beta, n = 10) |>
  ungroup()

# Convenience alias: first-community model kept for backward compatibility
lda_model <- lda_models[[1]]

# --- Heatmap: dominant term per topic for the top 6 communities --------------

top6_comms <- tidy_posts |>
  distinct(community, document) |>
  count(community, sort = TRUE) |>
  filter(community %in% valid_communities) |>
  slice_head(n = 6) |>
  pull(community)

# For each community, assign each topic its highest-beta term not already taken
# by another topic. Without this, one dominant word (e.g. "ireland") can win
# every topic, making the heatmap uninformative.
pick_unique_terms <- function(df) {
  topics <- sort(unique(df$topic))
  used   <- character(0)

  purrr::map(topics, \(t) {
    best <- dplyr::filter(df, topic == t, !term %in% used) |>
      dplyr::slice_max(beta, n = 1)
    used <<- c(used, best$term)
    best
  }) |>
    purrr::list_rbind()
}

community_topics |>
  filter(community %in% top6_comms) |>
  group_by(community) |>
  group_modify(~ pick_unique_terms(.x)) |>
  ungroup() |>
  mutate(
    community = factor(paste("Comm", community)),
    topic     = paste("Topic", topic)
  ) |>
  ggplot(aes(x = topic, y = community, fill = beta)) +
  geom_tile(color = "white") +
  geom_text(aes(label = term), size = 3) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "Dominant term per LDA topic by community",
    subtitle = "Top 6 communities by document count — one unique term per topic",
    x = NULL, y = NULL, fill = "Beta"
  ) +
  theme_minimal()

rxDataStep(community_topics, community_sql, overwrite = TRUE)
community_sql <- RxSqlServerData(table = "communityTopics", connectionString = connStr )
# --- Gamma: mean document-topic proportion per community ---------------------

community_gamma <- lda_models |>
  imap(\(model, comm) {
    tidy(model, matrix = "gamma") |>
      summarise(mean_gamma = mean(gamma), .by = topic) |>
      mutate(community = as.integer(comm))
  }) |>
  list_rbind()

# Order communities by their dominant topic for a cleaner y-axis
comm_order <- community_gamma |>
  group_by(community) |>
  slice_max(mean_gamma, n = 1) |>
  ungroup() |>
  arrange(topic, desc(mean_gamma)) |>
  pull(community)

# Build human-readable topic labels from the top-beta term(s) per topic.
# Topics are fitted independently per community so cross-community
# comparisons of these labels are approximate.
make_topic_labels <- function(beta_df, n_terms = 1) {
  beta_df |>
    group_by(topic, term) |>
    summarise(total_beta = sum(beta), .groups = "drop") |>
    slice_max(total_beta, n = n_terms, by = topic) |>
    arrange(topic, desc(total_beta)) |>
    summarise(
      label = paste0("T", first(topic), ": ", paste(unique(term), collapse = " / ")),
      .by = topic
    ) |>
    mutate(label = stringr::str_wrap(label, width = 18)) |>
    arrange(topic) |>
    (\(d) setNames(d$label, as.character(d$topic)))()
}

topic_labels <- make_topic_labels(community_topics)

community_gamma |>
  mutate(
    community  = factor(community, levels = comm_order),
    topic_name = recode(as.character(topic), !!!topic_labels),
    topic_name = factor(topic_name, levels = topic_labels)
  ) |>
  ggplot(aes(x = topic_name, y = community, fill = mean_gamma)) +
  geom_tile(color = "white", linewidth = 0.4) +
  scale_fill_gradient(
    low = "white", high = "#2c6fad",
    labels = scales::percent_format()
  ) +
  labs(
    title    = "Community-level topic concentration (gamma)",
    subtitle = "Mean document-topic proportion — topics labelled by dominant terms\n(Note: topics fitted independently per community; labels are approximate)",
    x = NULL, y = "Community", fill = "Mean γ"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 8, angle = 20, hjust = 1),
    panel.grid  = element_blank()
  )

# =============================================================================
# 5. GLOBAL LDA — single corpus-wide model for comparable community profiles
# =============================================================================

global_dtm <- tidy_posts |>
  count(document, word) |>
  build_dtm()

global_lda <- LDA(global_dtm, k = 5, control = list(seed = 7843))

# Document → community lookup needed to roll gamma up to community level
doc_community <- tidy_posts |>
  distinct(document, community)

# Average gamma per community (restricted to communities with >= 10 documents)
global_gamma <- tidy(global_lda, matrix = "gamma") |>
  mutate(document = as.integer(document)) |>
  left_join(doc_community, by = "document") |>
  filter(!is.na(community), community %in% valid_communities) |>
  group_by(community, topic) |>
  summarise(mean_gamma = mean(gamma), .groups = "drop")

# Order communities by their dominant topic for the y-axis
comm_order_global <- global_gamma |>
  group_by(community) |>
  slice_max(mean_gamma, n = 1) |>
  ungroup() |>
  arrange(topic, desc(mean_gamma)) |>
  pull(community)

global_topic_labels <- tidy(global_lda, matrix = "beta") |>
  make_topic_labels(n_terms = 1)

global_gamma |>
  mutate(
    community  = factor(community, levels = comm_order_global),
    topic_name = factor(
      recode(as.character(topic), !!!global_topic_labels),
      levels = global_topic_labels
    )
  ) |>
  ggplot(aes(x = topic_name, y = community, fill = mean_gamma)) +
  geom_tile(color = "white", linewidth = 0.4) +
  scale_fill_gradient(
    low = "white", high = "#2c6fad",
    labels = scales::percent_format()
  ) +
  labs(
    title    = "Community-level topic concentration — global LDA",
    subtitle = "Mean document-topic proportion (gamma) from a single corpus-wide model",
    x = NULL, y = "Community", fill = "Mean γ"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 8, angle = 20, hjust = 1),
    panel.grid  = element_blank()
  )

# =============================================================================
# 6. COMMUNITY-LEVEL SENTIMENT
# =============================================================================

# --- Bing: net positive / negative per community -----------------------------
community_sentiment <- tidy_posts |>
  inner_join(bing, by = "word") |>
  count(community, sentiment) |>
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |>
  mutate(net_sentiment = positive - negative)

# --- NRC wide: all emotions + net sentiment + dominant emotion per community -
# Kept as a separate wide-format table for downstream tabular analysis
community_sentiment_nrc_wide <- tidy_posts |>
  inner_join(nrc, by = "word") |>
  count(community, sentiment) |>
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |>
  mutate(
    net_sentiment = positive - negative,
    # Dominant among the 8 pure emotions only (excludes aggregate pos/neg columns)
    dominant_emotion = pmap_chr(
      pick(anger, anticipation, disgust, fear, joy, sadness, surprise, trust),
      \(...) names(which.max(c(...)))
    )
  )

# --- NRC long: emotion profile plot (8 pure emotions, proportional) ----------

emotions <- c("anger", "anticipation", "disgust", "fear",
              "joy", "sadness", "surprise", "trust")

emotion_palette <- c(
  anger        = "#d62728",
  anticipation = "#ff7f0e",
  disgust      = "#8c564b",
  fear         = "#9467bd",
  joy          = "#2ca02c",
  sadness      = "#1f77b4",
  surprise     = "#17becf",
  trust        = "#bcbd22"
)

# Long-form table restricted to the 8 pure emotions (exclude aggregate pos/neg)
community_sentiment_nrc <- tidy_posts |>
  inner_join(nrc, by = "word") |>
  filter(sentiment %in% emotions) |>
  count(community, sentiment)

# Top 15 communities by total matched word count
top_communities <- community_sentiment_nrc |>
  count(community, wt = n, sort = TRUE) |>
  slice_head(n = 15) |>
  pull(community)

# Order by total size so larger communities sit at the bottom of the y-axis
community_order <- community_sentiment_nrc |>
  filter(community %in% top_communities) |>
  count(community, wt = n) |>
  arrange(n) |>
  pull(community)

community_sentiment_nrc |>
  filter(community %in% top_communities) |>
  mutate(
    community = factor(community, levels = community_order),
    sentiment = factor(sentiment, levels = emotions)
  ) |>
  ggplot(aes(x = n, y = community, fill = sentiment)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = emotion_palette) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(
    title    = "Emotion profile by community (NRC lexicon)",
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
# =============================================================================

# Community-size lookup derived from Louvain membership vector
mem_tbl <- tibble::tibble(
  node      = igraph::V(g)$name,
  community = igraph::membership(comm_louvain)
)
comm_sizes_tbl <- mem_tbl |> count(community, name = "size")

# Restrict to top 30 communities (by node count) that also appear in global_gamma
shared_comms   <- intersect(unique(global_gamma$community), comm_sizes_tbl$community)
top_comms_focal <- comm_sizes_tbl |>
  filter(community %in% shared_comms) |>
  slice_max(size, n = 30) |>
  pull(community)

# --- Cosine similarity of global gamma profiles ------------------------------

cosine_sim <- function(mat) {
  norms <- sqrt(rowSums(mat^2))
  mat_n <- mat / norms
  mat_n %*% t(mat_n)
}

gamma_mat <- global_gamma |>
  filter(community %in% top_comms_focal) |>
  pivot_wider(names_from = topic, values_from = mean_gamma, values_fill = 0) |>
  tibble::column_to_rownames("community") |>
  as.matrix()

cos_mat <- cosine_sim(gamma_mat)

# --- Cross-community edge density for each community pair --------------------

el        <- igraph::as_edgelist(g, names = FALSE)
node_comm <- igraph::membership(comm_louvain)

edge_comm_pairs <- tibble::tibble(
  c1 = node_comm[el[, 1]],
  c2 = node_comm[el[, 2]]
) |>
  filter(c1 != c2, c1 %in% top_comms_focal, c2 %in% top_comms_focal) |>
  mutate(pair_a = pmin(c1, c2), pair_b = pmax(c1, c2)) |>
  count(pair_a, pair_b, name = "cross_edges") |>
  mutate(
    size_a        = comm_sizes_tbl$size[match(pair_a, comm_sizes_tbl$community)],
    size_b        = comm_sizes_tbl$size[match(pair_b, comm_sizes_tbl$community)],
    possible      = size_a * size_b,
    inter_density = cross_edges / possible
  )

# --- Join cosine similarity to inter-community density -----------------------

cos_long <- as.data.frame(as.table(cos_mat)) |>
  rename(comm_a = Var1, comm_b = Var2, cosine = Freq) |>
  mutate(comm_a = as.integer(comm_a), comm_b = as.integer(comm_b)) |>
  filter(comm_a < comm_b)

comparison <- cos_long |>
  left_join(
    rename(edge_comm_pairs, comm_a = pair_a, comm_b = pair_b),
    by = c("comm_a", "comm_b")
  ) |>
  mutate(
    inter_density = tidyr::replace_na(inter_density, 0),
    has_edge      = inter_density > 0
  )

# Spearman correlation: all pairs, and pairs that share at least one edge
cor_all  <- cor(comparison$cosine, comparison$inter_density, method = "spearman")
cor_edge <- cor(
  comparison$cosine[comparison$has_edge],
  comparison$inter_density[comparison$has_edge],
  method = "spearman"
)

# --- Scatter: cosine similarity vs. inter-community edge density -------------
ggplot(comparison, aes(x = cosine, y = inter_density)) +
  geom_point(aes(color = has_edge), alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8, color = "firebrick") +
  scale_color_manual(
    values = c("FALSE" = "grey70", "TRUE" = "#2c6fad"),
    labels = c("No cross-edges", "Has cross-edges")
  ) +
  scale_y_continuous(labels = scales::label_scientific()) +
  labs(
    title    = "Topic similarity vs. inter-community edge density",
    subtitle = sprintf(
      "Top 30 communities by size · Spearman ρ = %.3f (all pairs), %.3f (edges only)",
      cor_all, cor_edge
    ),
    x     = "Cosine similarity of global γ profiles",
    y     = "Inter-community edge density",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

# --- Paired heatmaps: topic similarity vs. edge density ----------------------
# Order communities by dominant topic so like-topics cluster visually
dom_topic_order <- global_gamma |>
  filter(community %in% top_comms_focal) |>
  group_by(community) |>
  slice_max(mean_gamma, n = 1) |>
  ungroup() |>
  arrange(topic, desc(mean_gamma)) |>
  pull(community)

fac_levels <- as.character(dom_topic_order)

# Helper: mirror upper-triangle data to produce a full symmetric matrix for tiles
make_symmetric <- function(df, col_a, col_b) {
  bind_rows(df, rename(df, !!col_a := !!col_b, !!col_b := !!col_a))
}

cos_sym <- cos_long |>
  mutate(
    comm_a = factor(comm_a, levels = fac_levels),
    comm_b = factor(comm_b, levels = fac_levels)
  ) |>
  make_symmetric("comm_a", "comm_b")

edge_sym <- comparison |>
  mutate(
    comm_a = factor(comm_a, levels = fac_levels),
    comm_b = factor(comm_b, levels = fac_levels)
  ) |>
  make_symmetric("comm_a", "comm_b")

p_cos <- ggplot(cos_sym, aes(x = comm_b, y = comm_a, fill = cosine)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "#2c6fad") +
  labs(
    title = "Topic cosine similarity (global γ)",
    x = NULL, y = "Community", fill = "Cosine"
  ) +
  theme_minimal(base_size = 9) +
  theme(axis.text = element_text(size = 6), panel.grid = element_blank())

p_edge <- ggplot(
  edge_sym,
  aes(x = comm_b, y = comm_a, fill = log1p(inter_density * 1e5))
) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "#b5201a", name = "log(density+1)") +
  labs(
    title = "Inter-community edge density",
    x = "Community", y = NULL
  ) +
  theme_minimal(base_size = 9) +
  theme(axis.text = element_text(size = 6), panel.grid = element_blank())

p_cos / p_edge
