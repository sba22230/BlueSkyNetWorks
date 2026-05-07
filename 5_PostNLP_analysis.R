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

ViewPostsByDate(posts, 2, 6)

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

ViewCommunityContrastedByWords(totals, counts, tidy_posts, 2, 6)

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

posts <- cbind(edges$from, edges$text, edges$created_at)
posts <- as.data.frame(posts)
posts <- posts |>
  mutate(V3 = lubridate::ymd(V3)) %>%
  mutate(document = row_number())

colnames <- c("name", "text", "created_at", "document")
colnames(posts) <- colnames

posts <- merge(
  posts,
  nodes[, c("name", "community")],
  by = "name",
  all.x = TRUE
)

tidy_posts <- posts %>%
  unnest_tokens(word, text) %>%
  group_by(word) %>%
  filter(n() > 10) %>%
  ungroup()

head(tidy_posts)
#install.packages('rsample')
library(rsample)
posts_split <- posts %>%
  select(document) %>%
  initial_split()

train_data <- training(posts_split)
test_data <- testing(posts_split)

sparse_words <- tidy_posts %>%
  count(document, word) %>%
  inner_join(train_data) %>%
  cast_sparse(document, word, n)

class(sparse_words)
dim(sparse_words)

word_rownames <- as.integer(rownames(sparse_words))

posts_joined <- data_frame(document = word_rownames) %>%
  left_join(
    posts %>%
      select(document, community)
  )

library(glmnet)
library(doParallel)

registerDoParallel(cores = 16)

is_comm <- posts_joined$community == "1"
model <- cv.glmnet(
  sparse_words,
  is_comm,
  family = "binomial",
  parallel = TRUE,
  keep = TRUE
)
plot(model)
plot(model$glmnet.fit)

library(broom)

coefs <- model$glmnet.fit %>%
  tidy() %>%
  filter(lambda == model$lambda.1se)

library(forcats)
coefs %>%
  group_by(estimate > 0) %>%
  top_n(10, abs(estimate)) %>%
  ungroup() %>%
  ggplot(aes(fct_reorder(term, estimate), estimate, fill = estimate > 0)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  coord_flip() +
  labs(
    x = NULL,
    title = "Coefficients that increase/decrease probability the most"
  )

# GLM Model building

library(text2vec)
library(glmnet)
library(caret)
library(dplyr)
library(readr)


sentimentdataset <- read_csv(
  "data/sentimentdataset.csv",
  col_types = cols(
    `Unnamed: 0` = col_skip(),
    User = col_skip(),
    Country = col_skip(),
    Year = col_skip(),
    Month = col_skip(),
    Day = col_skip(),
    Hour = col_skip()
  )
) |>
  rename(doc_id = 1, text = 2)

sentimentdataset$text <- iconv(sentimentdataset$text, from = "", to = "UTF-8", sub = "byte")


# Fix class labels for glmnet
sentimentdataset$Sentiment <- factor(make.names(sentimentdataset$Sentiment))


tokens <- sentimentdataset$text |>
  tolower() |>
  word_tokenizer()

it <- itoken(tokens, ids = sentimentdataset$doc_id, progressbar = FALSE)

vocab <- create_vocabulary(it) |>
  prune_vocabulary(term_count_min = 5)

vocab <- create_vocabulary(it, ngram = c(1L, 4L)) |>
  prune_vocabulary(term_count_min = 5)

vectorizer <- vocab_vectorizer(vocab)
vectorizer_nb <- vocab_vectorizer(vocab)

dtm <- create_dtm(it, vectorizer)
dtm_nb <- create_dtm(it, vectorizer_nb)  # term counts

tfidf <- TfIdf$new()
dtm_tfidf <- tfidf$fit_transform(dtm)

y <- sentimentdataset$Sentiment

# convert to dense/data.frame for e1071
x_nb <- as.matrix(dtm_nb)
train_df <- as.data.frame(x_nb)
train_df$Sentiment <- y

cvfit <- cv.glmnet(
  x = dtm_tfidf,
  y = y,
  family = "multinomial",
  type.measure = "class",
  nfolds = 5,
  parallel = TRUE
)

pred <- predict(cvfit, dtm_tfidf, s = "lambda.min", type = "class")
y <- droplevels(y)
 pred <- factor(pred, levels = levels(y))
confusionMatrix(pred, y)

# Bayesian Sentiment Analysis 
# train Naive Bayes
library(e1071)
nb_model <- naiveBayes(Sentiment ~ ., data = train_df)
stopImplicitCluster()
scored_posts <- score_sentiment(posts, cvfit, vectorizer, tfidf)

sentiment_props <- scored_posts |>
  count(sentiment) |>
  mutate(prop = n / sum(n))

library(lattice)

sentiment_props <- sentiment_props |>
  arrange(desc(prop))

library(latticeExtra)

barchart(
  prop ~ sentiment,
  data = sentiment_props,
  xlab = "Sentiment",
  ylab = "Proportion of Posts",
  main = "Sentiment Distribution in Posts",
  scales = list(
    y = list(
      at = seq(0, 1, by = 0.1),
      labels = paste0(seq(0, 100, by = 10), "%")
    )
  )
)

posts_nb <- score_sentiment_nb(posts, nb_model, vectorizer_nb)

sentiment_nb_props <- posts_nb |>
  count(sentiment_nb) |>
  mutate(prop = n / sum(n))

library(lattice)

sentiment_props <- sentiment_nb_props |>
  arrange(desc(prop))

library(latticeExtra)

barchart(
  prop ~ sentiment_nb,
  data = sentiment_nb_props,
  xlab = "Sentiment",
  ylab = "Proportion of Posts",
  main = "Sentiment Distribution in Posts",
  scales = list(
    y = list(
      at = seq(0, 1, by = 0.1),
      labels = paste0(seq(0, 100, by = 10), "%")
    )
  )
)


