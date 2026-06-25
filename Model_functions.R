# Shared text preparation used by both scoring functions.
# Cleans encoding, strips non-ASCII, lowercases, tokenises, and
# returns a text2vec iterator ready to be vectorised.
prepare_text_iterator <- function(posts) {
  posts$text <- posts$text |>
    iconv(from = "", to = "UTF-8", sub = "byte") |>
    (\(x) gsub("[^\x01-\x7F]", " ", x))() |>
    trimws()

  tokens <- tolower(posts$text) |>
    word_tokenizer()

  itoken(tokens, ids = posts$document, progressbar = FALSE)
}

score_sentiment <- function(posts, cvfit, vectorizer, tfidf) {
  it_new <- prepare_text_iterator(posts)

  # Apply the same vectorizer and TF-IDF transform used during training
  dtm_new_tfidf <- create_dtm(it_new, vectorizer) |>
    tfidf$transform()

  posts$sentiment <- predict(cvfit, dtm_new_tfidf,
                             s = "lambda.min",
                             type = "class") |>
    as.character()

  return(posts)
}

score_sentiment_nb <- function(posts, nb_model, vectorizer_nb) {
  it_new <- prepare_text_iterator(posts)

  # Predict directly from the sparse DTM — no dense conversion needed.
  # fastNaiveBayes::predict() accepts dgCMatrix, avoiding the memory cost
  # of materialising all the zero cells in a full dense matrix.
  dtm_new_nb <- create_dtm(it_new, vectorizer_nb)

  posts$sentiment_nb <- predict(nb_model, dtm_new_nb,
                                sparse = TRUE,
                                type = "class") |>
    as.character()

  return(posts)
}

