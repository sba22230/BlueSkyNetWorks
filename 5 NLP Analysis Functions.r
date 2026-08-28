# NLP Analysis Functions
# All functions accept an igraph graph object `g` and return results explicitly.
# Libraries are loaded via 0_functions.R; additional packages listed below must
# also be present: rsample, glmnet, doParallel, broom, forcats, text2vec, lattice.
# Edge contract: from is the reposter; to is the original post author.
# Edge contract: from is the reposter; to is the original post author.
#
# Module-level text-cleaning constants. Also consumed by plot_word_comparison_date()
# in 0_functions.R, so they must remain in scope when that helper is called.
replace_reg <- "https?://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
unnest_reg <- "([^\\p{L}\\d#@']|'(?![\\p{L}\\d#@]))"

# ---------------------------------------------------------------------------
# 1. Word frequency comparison between the top two authors in a graph
#
# Returns: a ggplot object (log-log frequency scatter)
# ---------------------------------------------------------------------------
plot_author_word_freq <- function(g, top_n = 2) {
  nodes <- igraph::as_data_frame(g, what = "vertices")
  edges <- igraph::as_data_frame(g, what = "edges")

  posts <- data.frame(
    author = edges$to,
    text = edges$text,
    date = lubridate::ymd(edges$created_at),
    stringsAsFactors = FALSE
  )

  top_authors <- nodes |>
    dplyr::group_by(name) |>
    dplyr::summarise(total_authored = sum(posts_authored, na.rm = TRUE)) |>
    dplyr::arrange(dplyr::desc(total_authored)) |>
    dplyr::slice(seq_len(top_n))

  name1 <- top_authors$name[1]
  name2 <- top_authors$name[2]

  tidy_posts <- posts |>
    dplyr::filter(!stringr::str_detect(text, "^RT")) |>
    dplyr::mutate(text = stringr::str_replace_all(text, replace_reg, "")) |>
    tidytext::unnest_tokens(
      word,
      text,
      token = "regex",
      pattern = unnest_reg
    ) |>
    dplyr::filter(
      !word %in% tidytext::stop_words$word,
      !word %in% stringr::str_remove_all(tidytext::stop_words$word, "'"),
      stringr::str_detect(word, "[a-z]")
    )

  frequency <- tidy_posts |>
    dplyr::count(author, word, sort = TRUE) |>
    dplyr::left_join(
      tidy_posts |> dplyr::count(author, name = "total"),
      by = "author"
    ) |>
    dplyr::mutate(freq = n / total) |>
    dplyr::select(author, word, freq) |>
    dplyr::filter(author %in% c(name1, name2)) |>
    tidyr::pivot_wider(names_from = author, values_from = freq)

  cols <- setdiff(names(frequency), "word")

  frequency <- frequency[stats::complete.cases(frequency[, cols]), ]
  x_vals <- frequency[[cols[1]]]
  y_vals <- frequency[[cols[2]]]

  list(
    data = frequency,
    x_label = paste0("Frequency by ", name1),
    y_label = paste0("Frequency by ", name2),
    x_vals = x_vals,
    y_vals = y_vals,
    labels = frequency$word
  )
}

render_author_word_freq <- function(
  g,
  top_n = 2,
  filename = "author-word-freq.svg",
  folder = "images"
) {
  result <- plot_author_word_freq(g, top_n = top_n)
  save_graph_svg(
    plot_or_expr = function() {
      plot(
        result$x_vals,
        result$y_vals,
        log = "xy",
        pch = 16,
        col = rgb(0.2, 0.4, 0.8, 0.2),
        cex = 0.8,
        xlab = result$x_label,
        ylab = result$y_label,
        main = "Word frequency comparison"
      )
      points(
        result$x_vals,
        result$y_vals,
        pch = 16,
        col = rgb(0.2, 0.4, 0.8, 0.2)
      )
      text(
        result$x_vals,
        result$y_vals,
        labels = result$labels,
        cex = 0.6,
        pos = 3
      )
      abline(0, 1, col = "red", lty = 2)
    },
    filename = filename,
    folder = folder
  )
}

# ---------------------------------------------------------------------------
# 2. Extract posts with community labels and compute word count summaries
#
# Returns: named list
#   $posts        — raw posts data frame with community column
#   $tidy_posts   — tokenised, stop-word filtered posts
#   $counts_long  — word counts per community (long form)
#   $totals       — total word count per community
#   $counts       — word counts per community (wide form, for log-odds)
# ---------------------------------------------------------------------------
get_community_posts <- function(g) {
  nodes <- igraph::as_data_frame(g, what = "vertices")
  edges <- igraph::as_data_frame(g, what = "edges")

  posts <- data.frame(
    name = edges$from,
    text = edges$text,
    created_at = lubridate::ymd(edges$created_at),
    stringsAsFactors = FALSE
  )

  posts <- merge(
    posts,
    nodes[, c("name", "community")],
    by = "name",
    all.x = TRUE
  )

  tidy_posts <- posts |>
    dplyr::filter(!stringr::str_detect(text, "^RT")) |>
    dplyr::mutate(text = stringr::str_replace_all(text, replace_reg, "")) |>
    tidytext::unnest_tokens(
      word,
      text,
      token = "regex",
      pattern = unnest_reg
    ) |>
    dplyr::filter(
      !word %in% tidytext::stop_words$word,
      !word %in% stringr::str_remove_all(tidytext::stop_words$word, "'"),
      stringr::str_detect(word, "[a-z]")
    )

  counts_long <- tidy_posts |>
    dplyr::count(community, word, name = "n")

  totals <- counts_long |>
    dplyr::group_by(community) |>
    dplyr::summarise(N = sum(n), .groups = "drop")

  counts <- counts_long |>
    tidyr::pivot_wider(
      names_from = community,
      values_from = n,
      values_fill = 0
    )

  list(
    posts = posts,
    tidy_posts = tidy_posts,
    counts_long = counts_long,
    totals = totals,
    counts = counts
  )
}

# ---------------------------------------------------------------------------
# 3. Posting timelines and word plots across all community subgraphs
#
# Args:
#   community_graphs — list of igraph subgraphs, one per community.
#                      Load with: rxReadObject(ds_Graphs, "Community Graphs")
#
# Returns: named list
#   $timeline_plot — single ggplot of posting dates faceted by community
#   $word_plots    — list of ggplots from plot_word_comparison_date()
#   $all_posts     — combined posts data frame with community_id column
# ---------------------------------------------------------------------------
plot_community_timelines <- function(community_graphs) {
  # global_freq must be in scope for plot_word_comparison_date(); compute it
  # from the combined posts so callers do not need to pre-build it.
  all_posts <- lapply(seq_along(community_graphs), function(i) {
    graph <- community_graphs[[i]]
    data.frame(
      text = igraph::E(graph)$text,
      created_at = lubridate::ymd(igraph::E(graph)$created_at),
      community_id = unique(igraph::V(graph)$community),
      stringsAsFactors = FALSE
    )
  }) |>
    dplyr::bind_rows()

  # Build global_freq in the calling environment so plot_word_comparison_date()
  # can reference it (it treats global_freq as a free variable).
  tidy_all <- all_posts |>
    dplyr::filter(!stringr::str_detect(text, "^RT")) |>
    dplyr::mutate(text = stringr::str_replace_all(text, replace_reg, "")) |>
    tidytext::unnest_tokens(
      word,
      text,
      token = "regex",
      pattern = unnest_reg
    ) |>
    dplyr::filter(
      !word %in% tidytext::stop_words$word,
      !word %in% stringr::str_remove_all(tidytext::stop_words$word, "'"),
      stringr::str_detect(word, "[a-z]")
    )

  global_freq <<- tidy_all |>
    dplyr::count(word, name = "global_n") |>
    dplyr::mutate(global_freq = global_n / sum(global_n))

  timeline_plot <- list(
    data = all_posts,
    x = all_posts$created_at,
    groups = all_posts$community_id
  )

  word_plots <- lapply(seq_along(community_graphs), function(i) {
    plot_word_comparison_date(
      community_graphs[[i]],
      unique(igraph::V(community_graphs[[i]])$community)
    )
  })

  list(
    timeline_plot = timeline_plot,
    word_plots = word_plots,
    all_posts = all_posts
  )
}

# Helper: render a page of word-comparison plots side by side
show_plots <- function(plots, page = 1, per_page = 4) {
  start <- (page - 1) * per_page + 1
  end <- min(page * per_page, length(plots))
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# 4. Binary community-membership classifier using penalised logistic regression
#
# Args:
#   g                — igraph graph object
#   target_community — character; community label to model as the positive class
#   cores            — number of parallel cores for cv.glmnet
#
# Returns: named list
#   $model      — fitted cv.glmnet object
#   $train_data — document IDs used for training
#   $test_data  — document IDs held out for testing
#   $coefs      — tidy coefficient table at lambda.1se
#   $coef_plot  — ggplot of top coefficients
# ---------------------------------------------------------------------------
build_community_classifier <- function(g, target_community = "1", cores = 16) {
  nodes <- igraph::as_data_frame(g, what = "vertices")
  edges <- igraph::as_data_frame(g, what = "edges")

  posts <- data.frame(
    name = edges$from,
    text = edges$text,
    created_at = lubridate::ymd(edges$created_at),
    stringsAsFactors = FALSE
  ) |>
    dplyr::mutate(document = dplyr::row_number())

  posts <- merge(
    posts,
    nodes[, c("name", "community")],
    by = "name",
    all.x = TRUE
  )

  tidy_posts <- posts |>
    tidytext::unnest_tokens(word, text) |>
    dplyr::group_by(word) |>
    dplyr::filter(dplyr::n() > 10) |>
    dplyr::ungroup()

  posts_split <- rsample::initial_split(dplyr::select(posts, document))
  train_data <- rsample::training(posts_split)
  test_data <- rsample::testing(posts_split)

  sparse_words <- tidy_posts |>
    dplyr::count(document, word) |>
    dplyr::inner_join(train_data, by = "document") |>
    tidytext::cast_sparse(document, word, n)

  # Double-transpose forces canonical dgCMatrix column order required by glmnet
  sparse_words <- Matrix::t(Matrix::t(sparse_words))

  posts_joined <- tibble::tibble(
    document = as.integer(rownames(sparse_words))
  ) |>
    dplyr::left_join(dplyr::select(posts, document, community), by = "document")

  doParallel::registerDoParallel(cores = cores)
  on.exit(doParallel::stopImplicitCluster(), add = TRUE)

  is_target <- posts_joined$community == target_community

  model <- glmnet::cv.glmnet(
    sparse_words,
    is_target,
    family = "binomial",
    parallel = TRUE,
    keep = TRUE
  )

  coefs <- broom::tidy(model$glmnet.fit) |>
    dplyr::filter(lambda == model$lambda.1se)

  coef_plot <- coefs |>
    dplyr::group_by(estimate > 0) |>
    dplyr::slice_max(abs(estimate), n = 10) |>
    dplyr::ungroup() |>
    ggplot2::ggplot(
      ggplot2::aes(
        forcats::fct_reorder(term, estimate),
        estimate,
        fill = estimate > 0
      )
    ) +
    ggplot2::geom_col(alpha = 0.8, show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL,
      title = paste0(
        "Coefficients that most increase/decrease P(community == ",
        target_community,
        ")"
      )
    ) +
    ggplot2::theme_minimal()

  list(
    model = model,
    train_data = train_data,
    test_data = test_data,
    coefs = coefs,
    coef_plot = coef_plot
  )
}

# ---------------------------------------------------------------------------
# 5. Train GLM and Naive Bayes sentiment models from a labelled CSV
#
# Args:
#   sentiment_path — path to the sentiment training CSV
#   cores          — parallel cores for cv.glmnet
#
# Returns: named list of all fitted objects needed by plot_sentiment_distributions()
#   $cvfit         — fitted cv.glmnet (multinomial)
#   $nb_model      — fitted fastNaiveBayes model
#   $vectorizer    — text2vec vectorizer (for GLM scoring)
#   $vectorizer_nb — text2vec vectorizer (for NB scoring; shares vocab with GLM)
#   $tfidf         — fitted TfIdf transformer
# ---------------------------------------------------------------------------
build_sentiment_models <- function(
  sentiment_path = project_path("data", "sentimentdataset.csv"),
  cores = 16
) {
  sentimentdataset <- readr::read_csv(
    sentiment_path,
    col_types = readr::cols(
      `Unnamed: 0` = readr::col_skip(),
      User = readr::col_skip(),
      Country = readr::col_skip(),
      Year = readr::col_skip(),
      Month = readr::col_skip(),
      Day = readr::col_skip(),
      Hour = readr::col_skip()
    )
  ) |>
    dplyr::rename(doc_id = 1, text = 2)

  sentimentdataset$text <- iconv(
    sentimentdataset$text,
    from = "",
    to = "UTF-8",
    sub = "byte"
  )

  sentimentdataset$Sentiment <- factor(make.names(sentimentdataset$Sentiment))

  tokens <- text2vec::word_tokenizer(tolower(sentimentdataset$text))
  it <- text2vec::itoken(
    tokens,
    ids = sentimentdataset$doc_id,
    progressbar = FALSE
  )

  # Build vocabulary with 1–4 ngrams; prune rare terms to keep the DTM tractable
  vocab <- text2vec::create_vocabulary(it, ngram = c(1L, 4L)) |>
    text2vec::prune_vocabulary(term_count_min = 5)

  vectorizer <- text2vec::vocab_vectorizer(vocab)
  # NB and GLM share the same vocabulary so the feature spaces stay identical
  vectorizer_nb <- vectorizer

  dtm <- text2vec::create_dtm(it, vectorizer)
  dtm_nb <- dtm

  tfidf <- text2vec::TfIdf$new()
  dtm_tfidf <- tfidf$fit_transform(dtm)

  y <- sentimentdataset$Sentiment

  doParallel::registerDoParallel(cores = cores)
  on.exit(doParallel::stopImplicitCluster(), add = TRUE)

  cvfit <- glmnet::cv.glmnet(
    x = dtm_tfidf,
    y = y,
    family = "multinomial",
    type.measure = "class",
    nfolds = 5,
    parallel = TRUE
  )

  # Naive Bayes trained on raw counts; fastNaiveBayes accepts dgCMatrix directly
  nb_model <- fastNaiveBayes::fnb.multinomial(
    dtm_nb,
    y = y,
    sparse = TRUE,
    laplace = 1
  )

  list(
    cvfit = cvfit,
    nb_model = nb_model,
    vectorizer = vectorizer,
    vectorizer_nb = vectorizer_nb,
    tfidf = tfidf
  )
}

# ---------------------------------------------------------------------------
# 6. Score posts extracted from a graph and visualise sentiment distributions
#
# Args:
#   g      — igraph graph object
#   models — list returned by build_sentiment_models()
#
# Returns: named list
#   $scored_posts       — posts data frame with $sentiment column (GLM)
#   $scored_nb          — posts data frame with $sentiment_nb column (NB)
#   $sentiment_props    — GLM sentiment proportions
#   $sentiment_nb_props — NB sentiment proportions
#   $glm_chart          — lattice barchart for GLM results
#   $nb_chart           — lattice barchart for NB results
# ---------------------------------------------------------------------------
plot_sentiment_distributions <- function(g, models) {
  nodes <- igraph::as_data_frame(g, what = "vertices")
  edges <- igraph::as_data_frame(g, what = "edges")

  posts <- data.frame(
    name = edges$from,
    text = edges$text,
    created_at = lubridate::ymd(edges$created_at),
    stringsAsFactors = FALSE
  ) |>
    dplyr::mutate(document = dplyr::row_number())

  posts <- merge(
    posts,
    nodes[, c("name", "community")],
    by = "name",
    all.x = TRUE
  )

  scored_posts <- score_sentiment(
    posts,
    models$cvfit,
    models$vectorizer,
    models$tfidf
  )
  scored_nb <- score_sentiment_nb(posts, models$nb_model, models$vectorizer_nb)

  sentiment_props <- scored_posts |>
    dplyr::count(sentiment) |>
    dplyr::mutate(prop = n / sum(n)) |>
    dplyr::arrange(dplyr::desc(prop))

  sentiment_nb_props <- scored_nb |>
    dplyr::count(sentiment_nb) |>
    dplyr::mutate(prop = n / sum(n)) |>
    dplyr::arrange(dplyr::desc(prop))

  glm_chart <- lattice::barchart(
    prop ~ sentiment,
    data = sentiment_props,
    xlab = "Sentiment",
    ylab = "Proportion of Posts",
    main = "Sentiment Distribution in Posts (GLM)",
    scales = list(
      y = list(
        at = seq(0, 1, by = 0.1),
        labels = paste0(seq(0, 100, by = 10), "%")
      )
    )
  )

  nb_chart <- lattice::barchart(
    prop ~ sentiment_nb,
    data = sentiment_nb_props,
    xlab = "Sentiment",
    ylab = "Proportion of Posts",
    main = "Sentiment Distribution in Posts (Naive Bayes)",
    scales = list(
      y = list(
        at = seq(0, 1, by = 0.1),
        labels = paste0(seq(0, 100, by = 10), "%")
      )
    )
  )

  list(
    scored_posts = scored_posts,
    scored_nb = scored_nb,
    sentiment_props = sentiment_props,
    sentiment_nb_props = sentiment_nb_props,
    glm_chart = glm_chart,
    nb_chart = nb_chart
  )
}
