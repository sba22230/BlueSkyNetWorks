# Text analysis file

library(tm)
library(ggplot2)
library(dplyr)
library(tidytext)
library(textdata)

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
  inner_join(nrc, by = "word" , relationship = "many-to-many")

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
dtm_by_community <- tidy_posts |>
  filter(community == target_comm) |>
  count(document, word) |>
  cast_dtm(document, word, n)

lda_model <- LDA(dtm_by_community, k = 5, control = list(seed = 7843))

community_sentiment <- tidy_posts |>
  inner_join(get_sentiments("bing"), by = "word") |>
  count(community, sentiment) |>
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |>
  mutate(net_sentiment = positive - negative)