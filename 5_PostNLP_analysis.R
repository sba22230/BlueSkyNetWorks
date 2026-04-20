# NLP of posts using TidyText
edges <- read_parquet("graphs/speirgorm_edges.parquet")
posts <- cbind(edges$from, edges$text, edges$created_at)
posts <- as.data.frame(posts)
posts <- posts |> mutate(V3 = lubridate::ymd(V3))

replace_reg <- "https?://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
unnest_reg <- "([^\\p{L}\\d#@']|'(?![\\p{L}\\d#@]))" # updated for all charahcters

tidy_posts <- posts |>
  filter(!str_detect(V2, "^RT")) |>
  mutate(V2 = str_replace_all(V2, replace_reg, "")) |>
  unnest_tokens(word, V2, token = "regex", pattern = unnest_reg) |>
  filter(
    !word %in% stop_words$word,
    !word %in% str_remove_all(stop_words$word, "'"),
    str_detect(word, "[a-z]")
  )

global_freq <- tidy_posts |>
  count(word, name = "global_n") |>
  mutate(global_freq = global_n / sum(global_n))

frequency <- tidy_posts |>
  count(V1, word, sort = TRUE) |>
  left_join(
    tidy_posts |>
      count(V1, name = "total")
  ) |>
  mutate(freq = n / total)

frequency <- frequency |>
  select(V1, word, freq) |>
  filter(V1 %in% c("fionadoris.bsky.social", "stiofanoaodh.bsky.social")) |>
  pivot_wider(names_from = V1, values_from = freq) |>
  arrange(fionadoris.bsky.social, stiofanoaodh.bsky.social)

library(scales)
ggplot(
  frequency,
  aes(x = fionadoris.bsky.social, y = stiofanoaodh.bsky.social)
) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = ) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red", linetype = "dashed") +
  labs(
    x = "Frequency in fionadoris.bsky.social",
    y = "Frequency in stiofanoaodh.bsky.social"
  ) +
  theme_minimal()

nodes <- read_parquet("graphs/speirgorm_nodes.parquet")
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

posts <- posts |>
  filter(community == "55" | community == "66")

ggplot(posts, aes(x = created_at, fill = community)) +
  geom_bar(position = "identity", show.legend = FALSE) +
  facet_wrap(~community, ncol = 1)

tidy_posts <- posts |>
  filter(!str_detect(text, "^RT")) |>
  mutate(text = str_replace_all(text, replace_reg, "")) |>
  unnest_tokens(word, text, token = "regex", pattern = unnest_reg) |>
  filter(
    !word %in% stop_words$word,
    !word %in% str_remove_all(stop_words$word, "'"),
    str_detect(word, "[a-z]")
  )

# 1. Counts per community/word
counts_long <- tidy_posts |>
  count(community, word, name = "n")

# 2. Totals per community
totals <- counts_long |>
  group_by(community) |>
  summarise(N = sum(n), .groups = "drop")

N55 <- totals$N[totals$community == "55"]
N66 <- totals$N[totals$community == "66"]

# 3. Wide counts for log-odds
counts <- counts_long |>
  pivot_wider(names_from = community, values_from = n, values_fill = 0)

alpha <- 0.01

counts <- counts |>
  mutate(
    log_odds_55 = log((`55` + alpha) / (N55 - `55` + alpha)),
    log_odds_66 = log((`66` + alpha) / (N66 - `66` + alpha)),
    log_odds = log_odds_55 - log_odds_66
  )

# 4. Frequencies (as you had)
frequency <- tidy_posts |>
  count(community, word, sort = TRUE) |>
  left_join(
    tidy_posts |>
      count(community, name = "total"),
    by = "community"
  ) |>
  mutate(freq = n / total)

frequency_filtered <- frequency |>
  group_by(community) |>
  arrange(desc(freq)) |>
  slice_head(n = 50) |>
  ungroup()

frequency <- frequency_filtered |>
  select(community, word, freq) |>
  pivot_wider(names_from = community, values_from = freq, values_fill = 0) |>
  mutate(
    `55` = ifelse(`55` == 0, 1e-6, `55`),
    `66` = ifelse(`66` == 0, 1e-6, `66`)
  )

# 5. Join log-odds onto frequency so we have both freq + distinctiveness
frequency <- frequency |>
  left_join(
    counts |>
      select(word, log_odds),
    by = "word"
  ) |>
  mutate(
    dominant = case_when(
      `55` > `66` ~ "55",
      `66` > `55` ~ "66",
      TRUE ~ "equal"
    )
  )

# 6. Top distinctive words (balanced)
top55 <- frequency |>
  slice_max(log_odds, n = 10)

top66 <- frequency |>
  slice_min(log_odds, n = 10)

top20 <- bind_rows(top55, top66)

# 7. Common words near the diagonal
common_words <- frequency |>
  mutate(
    log_ratio = abs(log10(`55`) - log10(`66`))
  ) |>
  #filter(log_ratio < 0.4) |>
  slice_max(`55` + `66`, n = 10)

# 8. Plot
ggplot(
  frequency,
  aes(x = `55`, y = `66`)
) +
  geom_point(
    aes(color = dominant),
    alpha = 0.4,
    size = 2
  ) +
  geom_density_2d(color = "grey70", alpha = 0.3) +
  geom_text_repel(
    data = top20,
    aes(label = word),
    size = 4,
    fontface = "bold",
    color = "black",
    max.overlaps = Inf
  ) +
  geom_text_repel(
    data = common_words,
    aes(label = word),
    size = 3,
    color = "grey20",
    fontface = "italic",
    max.overlaps = Inf
  ) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_manual(
    values = c(
      "55" = "#1f77b4",
      "66" = "#d62728",
      "equal" = "grey40"
    ),
    name = "More frequent in"
  ) +
  geom_abline(color = "red", linetype = "dashed") +
  labs(
    x = "Frequency in 55",
    y = "Frequency in 66",
    title = "Word Frequency Comparison Between Communities 55 and 66",
    subtitle = "Log-odds distinctive words (bold) and common words (italic)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

community_graphs <- rxReadObject(ds_Graphs, "Community Graphs")
n_comm <- length(community_graphs)
# for each sub graph plot the post timeline
all_posts <- lapply(seq_len(n_comm), function(i) {
  graph <- community_graphs[[i]]
  posts <- cbind(E(graph)$text, E(graph)$created_at)
  posts <- as.data.frame(posts)
  posts <- posts |> mutate(V2 = lubridate::ymd(V2))
  colnames(posts) <- c("text", "created_at")
  posts$community_id <- i
  posts
}) |>
  dplyr::bind_rows()

ggplot(all_posts, aes(x = created_at)) +
  geom_bar(show.legend = FALSE) +
  facet_wrap(~community_id, ncol = 3, scales = "free_y") + # <-- key fix
  labs(
    x = "Date of posts",
    y = "Count of posts",
    title = "Posting timelines for all communities"
  ) +
  theme_minimal(base_size = 12)

plots <- lapply(seq_len(n_comm), function(i) {
  plot_word_comparison_date(community_graphs[[i]], i)
})

show_plots <- function(plots, page = 1, per_page = 4) {
  start <- (page - 1) * per_page + 1
  end <- min(page * per_page, length(plots))
  wrap_plots(plots[start:end], ncol = 2) &
    theme(plot.margin = margin(10, 10, 10, 10))
}

show_plots(plots, page = 1, per_page = 4)
show_plots(plots, page = 2, per_page = 4)
show_plots(plots, page = 3, per_page = 4)
