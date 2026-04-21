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
    x = "Frequency in Community fionadoris.bsky.social",
    y = "Frequency in Community stiofanoaodh.bsky.social"
  ) +
  theme_minimal()

# Adding Community to the posts data frame
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

posts_dt <- posts |>
  count(community, name = "num_posts") |>
  arrange(desc(num_posts))

datatable(posts_dt)

ViewPostsByDate(posts, 2, 22)

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

# 3. Wide counts for log-odds
counts <- counts_long |>
  pivot_wider(names_from = community, values_from = n, values_fill = 0)

ViewCommunityContrastedByWords(totals, counts, tidy_posts, 2, 22)

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
