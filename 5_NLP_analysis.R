# NLP of posts using TidyText

edges <- igraph::as_data_frame(g, what = "edges")
nodes <- igraph::as_data_frame(g, what = "vertices")

posts <- cbind(edges$to, edges$text, edges$created_at)
posts <- as.data.frame(posts)
posts <- posts |> mutate(V3 = lubridate::ymd(V3))

#top_reposter <- nodes |> group_by(name) |> summarise(total_reposted = sum(reposts_made, na.rm = TRUE)) |> arrange(desc(total_reposted)) |> slice(1:1)
top_author <- nodes |>
  group_by(name) |>
  summarise(total_authored = sum(posts_authored, na.rm = TRUE)) |>
  arrange(desc(total_authored)) |>
  slice(1:2)
name1 <- top_author$name[1]
name2 <- top_author$name[2]
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
  filter(V1 %in% c(name1, name2)) |>
  pivot_wider(names_from = V1, values_from = freq) |>
  arrange(name1, name2)

cols <- setdiff(names(frequency), "word")

library(scales)
ggplot(
  frequency,
  aes(x = .data[[cols[1]]], y = .data[[cols[2]]])
) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red", linetype = "dashed") +
  labs(
    x = paste0("Frequency by ", name1),
    y = paste0("Frequency by ", name2)
  ) +
  theme_minimal()

# Adding Community to the posts data frame
nodes <- igraph::as_data_frame(g, what = "vertices")
edges <- igraph::as_data_frame(g, what = "edges")
posts <- cbind(edges$from, edges$text, edges$created_at)
posts <- as.data.frame(posts)
posts <- posts |> mutate(V3 = lubridate::ymd(V3))

post_col_names <- c("name", "text", "created_at")
colnames(posts) <- post_col_names

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

comm_1 <- posts_dt$community[1]
comm_2 <- posts_dt$community[2]

ViewPostsByDate(posts, comm_1, comm_2)

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

ViewCommunityContrastedByWords(totals, counts, tidy_posts, comm_1, comm_2)

community_graphs <- rxReadObject(ds_Graphs, "Community Graphs")
#community_graphs <- comm
n_comm <- length(community_graphs)
# for each sub graph plot the post timeline
all_posts <- lapply(seq_len(n_comm), function(i) {
  graph <- community_graphs[[i]]
  posts <- cbind(E(graph)$text, E(graph)$created_at)
  posts <- as.data.frame(posts)
  posts <- posts |> mutate(V2 = lubridate::ymd(V2))
  colnames(posts) <- c("text", "created_at")
  posts$community_id <- unique(V(graph)$community)
  posts
}) |>
  dplyr::bind_rows()

plot_community_timelines <- function(posts_df) {
  comms <- unique(posts_df$community_id)
  n_plots <- length(comms)
  ncol <- 4
  nrow <- ceiling(n_plots / ncol)
  old_par <- par(
    mfrow = c(nrow, ncol),
    mar = c(3, 3, 2, 1),
    oma = c(2, 2, 3, 1)
  )
  on.exit(par(old_par), add = TRUE)

  for (comm in comms) {
    comm_posts <- posts_df[posts_df$community_id == comm, , drop = FALSE]
    if (nrow(comm_posts) == 0) {
      plot.new()
      title(main = paste("Community", comm))
      next
    }

    counts <- table(comm_posts$created_at)
    dates <- as.Date(names(counts))
    values <- as.integer(counts)

    plot(
      dates,
      values,
      type = "h",
      lwd = 10,
      lend = "butt",
      col = "steelblue",
      xlab = "",
      ylab = "Count",
      main = paste("Community", comm),
      xaxt = "n"
    )
    axis.Date(
      1,
      at = pretty(dates),
      format = "%Y-%m-%d",
      las = 2,
      cex.axis = 0.7
    )
  }
  title("Post Timelines for Communities", outer = TRUE, cex.main = 1.2)
}

out_file <- "Post Timelines for Communities.svg"
save_graph_svg(
  plot_or_expr = function() {
    plot_community_timelines(all_posts)
  },
  filename = out_file,
  folder = "docs/images",
  width = 16,
  height = 12
)

plot_word_comparison_date_base <- function(plot_data) {
  plot(
    plot_data$frequency$median_time,
    plot_data$frequency$log_odds,
    pch = 16,
    col = rgb(0.12, 0.47, 0.71, 0.4),
    xlab = "Median posting time",
    ylab = "Distinctiveness (Dirichlet log-odds)",
    main = plot_data$title_text
  )

  if (nrow(plot_data$top_words) > 0) {
    with(
      plot_data$top_words,
      text(
        median_time,
        log_odds,
        labels = word,
        cex = 0.7,
        col = "royalblue",
        pos = 3
      )
    )
  }
  if (nrow(plot_data$common_words) > 0) {
    with(
      plot_data$common_words,
      text(
        median_time,
        log_odds,
        labels = word,
        cex = 0.6,
        col = "forestgreen",
        pos = 1
      )
    )
  }
}

plots <- lapply(seq_len(n_comm), function(i) {
  plot_word_comparison_date(
    community_graphs[[i]],
    unique(V(community_graphs[[i]])$community)
  )
})

show_plots <- function(plots, page = 1, per_page = 4) {
  start <- (page - 1) * per_page + 1
  end <- min(page * per_page, length(plots))
  svg_filename <- paste0("WordComparison_Page_", page, ".svg")

  save_graph_svg(
    plot_or_expr = function() {
      old_par <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), oma = c(2, 2, 3, 1))
      on.exit(par(old_par), add = TRUE)

      for (i in seq(start, end)) {
        plot_word_comparison_date_base(plots[[i]])
      }
      title("Word Comparison by Community", outer = TRUE, cex.main = 1.2)
    },
    filename = svg_filename,
    folder = "docs/images",
    width = 16,
    height = 12
  )
}

show_plots(plots, page = 1, per_page = 4)
show_plots(plots, page = 2, per_page = 4)
show_plots(plots, page = 3, per_page = 4)

### Model Building ###

posts <- cbind(edges$from, edges$text, edges$created_at)
posts <- as.data.frame(posts)
posts <- posts |>
  mutate(V3 = lubridate::ymd(V3)) %>%
  mutate(document = row_number())

post_col_names <- c("name", "text", "created_at", "document")
colnames(posts) <- post_col_names

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

# cast_sparse() does not guarantee sorted row indices within each column,
# which dgCMatrix requires. A double-transpose forces the Matrix package to
# rebuild the compressed column structure in canonical order, preventing the
# "i slot is not increasing" error that glmnet raises during validation.
sparse_words <- Matrix::t(Matrix::t(sparse_words))

word_rownames <- as.integer(rownames(sparse_words))

posts_joined <- tibble(document = word_rownames) %>%
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

sentimentdataset$text <- iconv(
  sentimentdataset$text,
  from = "",
  to = "UTF-8",
  sub = "byte"
)


# Fix class labels for glmnet
sentimentdataset$Sentiment <- factor(make.names(sentimentdataset$Sentiment))


tokens <- sentimentdataset$text |>
  tolower() |>
  word_tokenizer()

it <- itoken(tokens, ids = sentimentdataset$doc_id, progressbar = FALSE)

# Build vocabulary with 1–4 ngrams; prune rare terms to keep the DTM tractable
vocab <- create_vocabulary(it, ngram = c(1L, 4L)) |>
  prune_vocabulary(term_count_min = 5)

vectorizer <- vocab_vectorizer(vocab)
# NB uses raw term counts; glmnet uses TF-IDF — both need the same vocabulary
# so they share one vectorizer, keeping the feature spaces identical.
vectorizer_nb <- vectorizer

dtm <- create_dtm(it, vectorizer)
# NB and glmnet start from the same raw count DTM; TF-IDF is applied later
# only for glmnet, so we alias rather than rebuilding from scratch.
dtm_nb <- dtm

tfidf <- TfIdf$new()
dtm_tfidf <- tfidf$fit_transform(dtm)

y <- sentimentdataset$Sentiment

# dtm_nb is already a sparse matrix; keep it sparse for fastNaiveBayes
x_nb <- dtm_nb

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

# Bayesian Sentiment Analysis — train on the sparse DTM directly.
# fastNaiveBayes accepts dgCMatrix so we avoid materialising the dense matrix.
library(fastNaiveBayes)
nb_model <- fnb.multinomial(x_nb, y = y, sparse = TRUE, laplace = 1)
stopImplicitCluster()
source("Model_functions.R")
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

barchart(
  prop ~ sentiment_nb,
  data = sentiment_nb_props |> arrange(desc(prop)),
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
