score_sentiment <- function(posts, cvfit, vectorizer, tfidf) {
  
  # 1. Clean text encoding
  posts$text <- posts$text |>
    iconv(from = "", to = "UTF-8", sub = "byte")
  
  # Replace non‑ASCII characters safely
  posts$text <- gsub("[^\x01-\x7F]", " ", posts$text)
  posts$text <- trimws(posts$text)
  
  # 2. Tokenize
  tokens_new <- posts$text |>
    tolower() |>
    word_tokenizer()
  
  # 3. Build iterator
  it_new <- itoken(tokens_new,
                   ids = posts$document,   # your posts use 'document' not 'doc_id'
                   progressbar = FALSE)
  
  # 4. Create DTM using the SAME vectorizer
  dtm_new <- create_dtm(it_new, vectorizer)
  
  # 5. Apply the SAME TF-IDF transformation
  dtm_new_tfidf <- tfidf$transform(dtm_new)
  
  # 6. Predict sentiment
  preds <- predict(cvfit, dtm_new_tfidf,
                   s = "lambda.min",
                   type = "class")
  
  # 7. Add predictions to dataframe
  posts$sentiment <- as.character(preds)
  
  return(posts)
}


score_sentiment_nb <- function(posts, nb_model, vectorizer_nb) {
  
  # 1. Clean text encoding
  posts$text <- posts$text |>
    iconv(from = "", to = "UTF-8", sub = "byte")
  
  # Replace non‑ASCII characters safely
  posts$text <- gsub("[^\x01-\x7F]", " ", posts$text)
  posts$text <- trimws(posts$text)
  
  # 2. Tokenize
  tokens_new <- posts$text |>
    tolower() |>
    word_tokenizer()
  
  # 3. Build iterator
  it_new <- itoken(tokens_new,
                   ids = posts$document,   # your posts use 'document'
                   progressbar = FALSE)
  
  # 4. Create DTM using the SAME vectorizer
  dtm_new_nb <- create_dtm(it_new, vectorizer_nb)
  
  # 5. Convert to matrix/data.frame for Naive Bayes
  x_new_nb <- as.matrix(dtm_new_nb)
  new_df <- as.data.frame(x_new_nb)
  
  # 6. Predict sentiment
  preds <- predict(nb_model, new_df)
  
  # 7. Add predictions to dataframe
  posts$sentiment_nb <- as.character(preds)
  
  return(posts)
}

