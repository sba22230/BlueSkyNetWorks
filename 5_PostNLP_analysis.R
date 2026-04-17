# NLP of posts using TidyText
edges_df <- read_parquet("graphs/speirgorm_edges.parquet")
posts <- cbind(edges_df$from, edges_df$text, edges_df$created_at)
posts <- as.data.frame(posts)
posts <- posts |> mutate(V3 = lubridate::ymd(V3))
posts <- posts |>
  filter(V1 == "fionadoris.bsky.social" | V1 == "sharrow.eurosky.social")
ggplot(posts, aes(x = V3, fill = V1)) +
  geom_bar(position = "identity", show.legend = FALSE) +
  facet_wrap(~V1, ncol = 1)

library(tidytext)
library(stringr)

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

frequency <- tidy_posts |>
  count(V1, word, sort = TRUE) |>
  left_join(
    tidy_posts |>
      count(V1, name = "total")
  ) |>
  mutate(freq = n / total)

frequency <- frequency |>
  select(V1, word, freq) |>
  pivot_wider(names_from = V1, values_from = freq) |>
  arrange(fionadoris.bsky.social, sharrow.eurosky.social)

library(scales)
ggplot(
  frequency,
  aes(x = fionadoris.bsky.social , y = sharrow.eurosky.social)
) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = ) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red", linetype = "dashed") +
  labs(
    x = "Frequency in fionadoris.bsky.social",
    y = "Frequency in sharrow.eurosky.social"
  ) +
  theme_minimal()
