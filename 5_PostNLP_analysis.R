# NLP of posts using TidyText
edges_df <- read_parquet("graphs/speirgorm_edges.parquet")
posts <- cbind( edges_df$from, edges_df$text, edges_df$created_at)
posts <- as.data.frame(posts)
posts <- posts |> mutate(V3 = lubridate::ymd(V3))
ggplot(posts, aes(x = V3, fill = V1)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  facet_wrap(~V1, ncol = 1)
