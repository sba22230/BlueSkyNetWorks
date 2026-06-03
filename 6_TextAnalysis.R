# Text analysis file

library(tm)
library(ggplot2)
library(dplyr)
library(tidytext)
library(textdata)

# Adding Community to the posts data frame
nodes <- igraph::as_data_frame(g, what = "vertices")
edges <- igraph::as_data_frame(g, what = "edges")
posts <- cbind(edges$from, edges$text, edges$created_at)
posts <- as.data.frame(posts)
posts <- posts |> mutate(V3 = lubridate::ymd(V3))

colnames <- c("name", "text", "created_at")
colnames(posts) <- colnames

posts <- merge(
  posts,
  nodes[, c("name", "community")],
  by = "name",
  all.x = TRUE
)

text_column <- posts$text
# Convert to lowercase
text_column <- tolower(text_column)

# Remove punctuation
text_column <- gsub("[[:punct:]]", "", text_column)

# Print the first few rows of the preprocessed text data
head(text_column)
# Convert to data frame
text_df <- data.frame(text = text_column, stringsAsFactors = FALSE)

# Unnest the text into words
text_words <- text_df %>%
  unnest_tokens(word, text)

# Load the Bing lexicon
bing <- get_sentiments("bing")

# Join the text words with the Bing lexicon
sentiment_analysis_bing <- text_words %>%
  inner_join(bing, by = "word")

# Summarize sentiment counts
sentiment_summary_bing <- sentiment_analysis_bing %>%
  count(sentiment, sort = TRUE)

# Print the sentiment summary for Bing
print(sentiment_summary_bing)

# Create a bar chart of sentiment counts for Bing
ggplot(sentiment_summary_bing, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Sentiment Analysis Using Bing Lexicon",
    x = "Sentiment",
    y = "Count"
  ) +
  theme_minimal()

# Load the NRC lexicon
nrc <- get_sentiments("nrc")

# Join the text words with the NRC lexicon
sentiment_analysis <- text_words %>%
  inner_join(nrc, by = "word", relationship = "many-to-many")

# Summarize sentiment counts
sentiment_summary <- sentiment_analysis %>%
  count(sentiment, sort = TRUE)

# Print the sentiment summary
print(sentiment_summary)

# Create a bar chart of sentiment counts
ggplot(
  sentiment_summary,
  aes(x = reorder(sentiment, n), y = n, fill = sentiment)
) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Sentiment Analysis Using NRC Lexicon",
    x = "Sentiment",
    y = "Count"
  ) +
  theme_minimal()

# Load the AFINN lexicon
afinn <- get_sentiments("afinn")

# Join the text words with the AFINN lexicon
sentiment_analysis_afinn <- text_words %>%
  inner_join(afinn, by = "word")

# Summarize sentiment scores
sentiment_summary_afinn <- sentiment_analysis_afinn %>%
  group_by(value) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(desc(value))

# Print the sentiment summary for AFINN
print(sentiment_summary_afinn)

# Create a bar chart of sentiment scores for AFINN
ggplot(
  sentiment_summary_afinn,
  aes(
    x = factor(value, levels = sort(unique(value))),
    y = count,
    fill = factor(value)
  )
) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Sentiment Analysis Using AFINN Lexicon",
    x = "Sentiment Score",
    y = "Count"
  ) +
  theme_minimal()

library(topicmodels)
library(purrr)

# Custom stop words: speirgorm variants, URLs, domains, and malformed tokens
custom_stops <- tibble::tibble(
  word = c(
    "speirgorm",
    "spéirgorm",
    "speirghorm",
    "spéirghorm"
  )
)

# Helper to drop URL-like tokens (domains, www., .ie/.com/.org etc.)
is_url_token <- function(x) {
  stringr::str_detect(
    x,
    "www\\.|^http|\\.com|\\.ie|\\.net|\\.org|\\.eu|bsky\\.social"
  )
}

tidy_posts <- posts |>
  filter(!str_detect(text, "^RT")) |>
  mutate(text = str_replace_all(text, replace_reg, "")) |>
  unnest_tokens(word, text, token = "regex", pattern = unnest_reg) |>
  filter(
    !word %in% stop_words$word,
    !word %in% str_remove_all(stop_words$word, "'"),
    str_detect(word, "[a-z]")
  )

# Identify communities large enough to support LDA (at least 10 documents)
valid_communities <- tidy_posts |>
  distinct(community, document) |>
  count(community) |>
  filter(n >= 10) |>
  pull(community)

# Build a DTM and fit a 5-topic LDA model for each qualifying community
lda_models <- valid_communities |>
  set_names() |>
  map(\(comm) {
    dtm <- tidy_posts |>
      filter(community == comm) |>
      anti_join(stop_words, by = "word") |>
      anti_join(custom_stops, by = "word") |>
      dplyr::filter(!is_url_token(word)) |>
      count(document, word) |>
      cast_dtm(document, word, n)
    LDA(dtm, k = 5, control = list(seed = 7843))
  })

# Extract the top 10 terms per topic for every community
community_topics <- lda_models |>
  imap(\(model, comm) {
    tidy(model, matrix = "beta") |>
      mutate(community = as.integer(comm))
  }) |>
  list_rbind() |>
  group_by(community, topic) |>
  slice_max(beta, n = 10) |>
  ungroup()

# Keep the single-community model for backward compatibility
lda_model <- lda_models[[1]]

# Compare topic distributions: dominant term per topic across top 6 communities
top6_comms <- tidy_posts |>
  dplyr::distinct(community, document) |>
  dplyr::count(community, sort = TRUE) |>
  dplyr::filter(community %in% valid_communities) |>
  dplyr::slice_head(n = 6) |>
  dplyr::pull(community)

community_topics |>
  dplyr::filter(community %in% top6_comms) |>
  dplyr::group_by(community, topic) |>
  dplyr::slice_max(beta, n = 1) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    community = factor(paste("Comm", community)),
    topic = paste("Topic", topic)
  ) |>
  ggplot(aes(x = topic, y = community, fill = beta)) +
  geom_tile(color = "white") +
  geom_text(aes(label = term), size = 3) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "Dominant term per LDA topic by community",
    subtitle = "Top 6 communities by document count",
    x = NULL,
    y = NULL,
    fill = "Beta"
  ) +
  theme_minimal()

# Gamma matrix: community-level topic concentration
community_gamma <- lda_models |>
  purrr::imap(\(model, comm) {
    tidytext::tidy(model, matrix = "gamma") |>
      dplyr::summarise(mean_gamma = mean(gamma), .by = topic) |>
      dplyr::mutate(community = as.integer(comm))
  }) |>
  purrr::list_rbind()

comm_order <- community_gamma |>
  dplyr::group_by(community) |>
  dplyr::slice_max(mean_gamma, n = 1) |>
  dplyr::ungroup() |>
  dplyr::arrange(topic, dplyr::desc(mean_gamma)) |>
  dplyr::pull(community)

# Derive topic labels from top-beta terms — aggregated across all community models.
# Note: topics are fitted independently per community, so labels are approximate.
make_topic_labels <- function(beta_df, n_terms = 2) {
  beta_df |>
    dplyr::group_by(topic, term) |>
    dplyr::summarise(total_beta = sum(beta), .groups = "drop") |>
    dplyr::slice_max(total_beta, n = n_terms, by = topic) |>
    dplyr::arrange(topic, dplyr::desc(total_beta)) |>
    dplyr::summarise(
      label = paste0(
        "T",
        dplyr::first(topic),
        ": ",
        paste(term, collapse = " & ")
      ),
      .by = topic
    ) |>
    dplyr::arrange(topic) |>
    (\(d) setNames(d$label, as.character(d$topic)))()
}

topic_labels <- make_topic_labels(community_topics)

community_gamma |>
  dplyr::mutate(
    community = factor(community, levels = comm_order),
    topic_name = dplyr::recode(as.character(topic), !!!topic_labels),
    topic_name = factor(topic_name, levels = topic_labels)
  ) |>
  ggplot(aes(x = topic_name, y = community, fill = mean_gamma)) +
  geom_tile(color = "white", linewidth = 0.4) +
  scale_fill_gradient(
    low = "white",
    high = "#2c6fad",
    labels = scales::percent_format()
  ) +
  labs(
    title = "Community-level topic concentration (gamma)",
    subtitle = "Mean document-topic proportion — topics labelled by dominant terms\n(Note: topics are fitted independently per community; labels are approximate)",
    x = NULL,
    y = "Community",
    fill = "Mean γ"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 8),
    panel.grid = element_blank()
  )

# --- Global LDA: single corpus-wide model for comparable community profiles ---

global_dtm <- tidy_posts |>
  dplyr::anti_join(stop_words, by = "word") |>
  dplyr::anti_join(custom_stops, by = "word") |>
  dplyr::filter(!is_url_token(word)) |>
  dplyr::count(document, word) |>
  cast_dtm(document, word, n)

global_lda <- LDA(global_dtm, k = 5, control = list(seed = 7843))

# Document-community lookup
doc_community <- tidy_posts |>
  dplyr::distinct(document, community)

# Average gamma per community (restricted to communities with >= 10 documents)
global_gamma <- tidytext::tidy(global_lda, matrix = "gamma") |>
  dplyr::mutate(document = as.integer(document)) |>
  dplyr::left_join(doc_community, by = "document") |>
  dplyr::filter(!is.na(community), community %in% valid_communities) |>
  dplyr::group_by(community, topic) |>
  dplyr::summarise(mean_gamma = mean(gamma), .groups = "drop")

comm_order_global <- global_gamma |>
  dplyr::group_by(community) |>
  dplyr::slice_max(mean_gamma, n = 1) |>
  dplyr::ungroup() |>
  dplyr::arrange(topic, dplyr::desc(mean_gamma)) |>
  dplyr::pull(community)

global_topic_labels <- tidytext::tidy(global_lda, matrix = "beta") |>
  make_topic_labels(n_terms = 2)

global_gamma |>
  dplyr::mutate(
    community = factor(community, levels = comm_order_global),
    topic_name = factor(
      dplyr::recode(as.character(topic), !!!global_topic_labels),
      levels = global_topic_labels
    )
  ) |>
  ggplot(aes(x = topic_name, y = community, fill = mean_gamma)) +
  geom_tile(color = "white", linewidth = 0.4) +
  scale_fill_gradient(
    low = "white",
    high = "#2c6fad",
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
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 8, angle = 20, hjust = 1),
    panel.grid = element_blank()
  )

community_sentiment <- tidy_posts |>
  inner_join(get_sentiments("bing"), by = "word") |>
  count(community, sentiment) |>
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |>
  mutate(net_sentiment = positive - negative)

community_sentiment_nrc <- tidy_posts |>
  inner_join(nrc, by = "word") |>
  count(community, sentiment) |>
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |>
  mutate(
    net_sentiment = positive - negative,
    # Dominant emotion: highest count among the 8 pure emotions (exclude pos/neg)
    dominant_emotion = pmap_chr(
      pick(anger, anticipation, disgust, fear, joy, sadness, surprise, trust),
      \(...) names(which.max(c(...)))
    )
  )
library(dplyr)
library(tidyr)
library(ggplot2)

# 8 pure emotions only — exclude aggregate positive/negative
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

# Build NRC community emotion profile
community_sentiment_nrc <- tidy_posts |>
  inner_join(nrc, by = "word") |>
  filter(sentiment %in% emotions) |>
  count(community, sentiment)

# Top 15 communities by total matched word count
top_communities <- community_sentiment_nrc |>
  count(community, wt = n, sort = TRUE) |>
  slice_head(n = 15) |>
  pull(community)

# Order communities by total size for the y-axis
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
    title = "Emotion profile by community (NRC lexicon)",
    subtitle = "Top 15 communities by matched word count — proportional share",
    x = "Share of emotion words",
    y = "Community",
    fill = "Emotion"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

library(wordcloud2)
wordcloud2(global_freq)

# =============================================================================
# Topic similarity vs. inter-community network density
# Do topically similar communities also share more structural ties?
# =============================================================================

# Build community-size lookup from Louvain membership
mem_tbl <- tibble(
  node = V(g)$name,
  community = membership(comm_louvain)
)
comm_sizes_tbl <- mem_tbl |> dplyr::count(community, name = "size")

# Restrict to top 30 communities (by node count) that also appear in global_gamma
shared_comms <- intersect(
  unique(global_gamma$community),
  comm_sizes_tbl$community
)
top_comms_focal <- comm_sizes_tbl |>
  dplyr::filter(community %in% shared_comms) |>
  dplyr::slice_max(size, n = 30) |>
  dplyr::pull(community)

# --- Topic cosine-similarity matrix ---
gamma_mat <- global_gamma |>
  dplyr::filter(community %in% top_comms_focal) |>
  tidyr::pivot_wider(
    names_from = topic,
    values_from = mean_gamma,
    values_fill = 0
  ) |>
  tibble::column_to_rownames("community") |>
  as.matrix()

cosine_sim <- function(mat) {
  norms <- sqrt(rowSums(mat^2))
  mat_n <- mat / norms
  mat_n %*% t(mat_n)
}
cos_mat <- cosine_sim(gamma_mat)

# --- Cross-community edge density for each community pair ---
el <- igraph::as_edgelist(g, names = FALSE)
node_comm <- igraph::membership(comm_louvain)

edge_comm_pairs <- tibble::tibble(
  c1 = node_comm[el[, 1]],
  c2 = node_comm[el[, 2]]
) |>
  dplyr::filter(
    c1 != c2,
    c1 %in% top_comms_focal,
    c2 %in% top_comms_focal
  ) |>
  dplyr::mutate(pair_a = pmin(c1, c2), pair_b = pmax(c1, c2)) |>
  dplyr::count(pair_a, pair_b, name = "cross_edges") |>
  dplyr::mutate(
    size_a = comm_sizes_tbl$size[match(pair_a, comm_sizes_tbl$community)],
    size_b = comm_sizes_tbl$size[match(pair_b, comm_sizes_tbl$community)],
    possible = size_a * size_b,
    inter_density = cross_edges / possible
  )

# --- Join cosine similarity to inter-community density ---
cos_long <- as.data.frame(as.table(cos_mat)) |>
  dplyr::rename(comm_a = Var1, comm_b = Var2, cosine = Freq) |>
  dplyr::mutate(comm_a = as.integer(comm_a), comm_b = as.integer(comm_b)) |>
  dplyr::filter(comm_a < comm_b)

comparison <- cos_long |>
  dplyr::left_join(
    edge_comm_pairs |> dplyr::rename(comm_a = pair_a, comm_b = pair_b),
    by = c("comm_a", "comm_b")
  ) |>
  dplyr::mutate(
    inter_density = tidyr::replace_na(inter_density, 0),
    has_edge = inter_density > 0
  )

cor_all <- cor(comparison$cosine, comparison$inter_density, method = "spearman")
cor_edge <- cor(
  comparison$cosine[comparison$has_edge],
  comparison$inter_density[comparison$has_edge],
  method = "spearman"
)

# --- Scatter: cosine similarity vs. inter-community edge density ---
ggplot(comparison, aes(x = cosine, y = inter_density)) +
  geom_point(aes(color = has_edge), alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8, color = "firebrick") +
  scale_color_manual(
    values = c("FALSE" = "grey70", "TRUE" = "#2c6fad"),
    labels = c("No cross-edges", "Has cross-edges")
  ) +
  scale_y_continuous(labels = scales::label_scientific()) +
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

# --- Paired heatmaps: topic similarity vs. edge density ---
# Order communities by their dominant topic so like-topics cluster together
dom_topic_order <- global_gamma |>
  dplyr::filter(community %in% top_comms_focal) |>
  dplyr::group_by(community) |>
  dplyr::slice_max(mean_gamma, n = 1) |>
  dplyr::ungroup() |>
  dplyr::arrange(topic, dplyr::desc(mean_gamma)) |>
  dplyr::pull(community)

fac_levels <- as.character(dom_topic_order)

# Symmetric long-form for heatmap tiles
cos_sym <- cos_long |>
  dplyr::mutate(
    comm_a = factor(comm_a, levels = fac_levels),
    comm_b = factor(comm_b, levels = fac_levels)
  ) |>
  (\(d) {
    dplyr::bind_rows(d, dplyr::rename(d, comm_a = comm_b, comm_b = comm_a))
  })()

edge_sym <- comparison |>
  dplyr::mutate(
    comm_a = factor(comm_a, levels = fac_levels),
    comm_b = factor(comm_b, levels = fac_levels)
  ) |>
  (\(d) {
    dplyr::bind_rows(d, dplyr::rename(d, comm_a = comm_b, comm_b = comm_a))
  })()

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

p_cos / p_edge
