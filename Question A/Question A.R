# Data preparation
# Remember to download the required packages before running the code
load(file = "tweets.Rda")
any(is.na(tweets)) # To check whether there are any missing values
library(quanteda)
library(quanteda.textstats)
library(tidyverse)
library(RedditExtractoR)
library(lubridate)
library(ggplot2)
library(glmnet)
library(gridExtra)

# Create a corpus of tweets
tweetCorpus <- corpus(tweets$text, docvars = tweets)

# Turn the corpus into a document-term matrix, make everything lower-case, remove numbers, punctuation and stop words
dfm_tweet <- tweetCorpus %>% 
  tokens(remove_numbers=T, remove_punct=T, include_docvars=T) %>%
  tokens_remove(stopwords("en")) %>% 
  dfm() %>% 
  dfm(tolower=T)

# Weighted by document length
dfm_tweet_weighted <- dfm_tweet %>% dfm_weight(scheme = "prop")

nfeat(dfm_tweet_weighted)
dim(dfm_tweet_weighted)

# i)
# Use TF-IDF Weighting to list the top 40 words used in negative and positive tweets
dfm_tweet_tfidf <- dfm_tweet %>% dfm_tfidf()
words.tfidf <- textstat_frequency(dfm_tweet_tfidf, groups = sentiment, force = T)

# Plot graphs for top 40 words used in negative tweets
negative_words <- words.tfidf[words.tfidf$group == 1,]
top40_negative_words <- negative_words[order(negative_words$frequency, decreasing = TRUE), ][1:40, ]

negative_plot <- ggplot(top40_negative_words, aes(x = frequency, y = reorder(feature, frequency))) +
  geom_point() +
  ylab("") +
  xlab("Frequency (TF-IDF weighted)") +
  ggtitle("Top 40 words used in Negative Tweets")

# Plot graphs for top 40 words used in positive tweets
positive_words <- words.tfidf[words.tfidf$group == 0,]
top40_positive_words <- positive_words[order(positive_words$frequency, decreasing = TRUE), ][1:40, ]

positive_plot <- ggplot(top40_positive_words, aes(x = frequency, y = reorder(feature, frequency))) +
  geom_point() +
  ylab("") +
  xlab("Frequency (TF-IDF weighted)") +
  ggtitle("Top 40 words used in Positive Tweets")

# Combine graphs
combined_PositiveAndNegative_plot <- grid.arrange(negative_plot, positive_plot, nrow = 1)

# Word usage differ across the different airlines
words.tfidf.airline <- textstat_frequency(dfm_tweet_tfidf, groups = airline, force = T)

airline <- unique(tweets$airline)
view(airline) # There are 6 major American airline companies

# US Airways
words_USAirways <- words.tfidf.airline[words.tfidf.airline$group == "US Airways",]
words_USAirways <- words_USAirways[order(words_USAirways$frequency, decreasing = TRUE), ][1:15, ]
plot_USAirways <- ggplot(words_USAirways, aes(x = frequency, y = reorder(feature, frequency))) +
  geom_point() +
  ylab("") +
  xlab("Frequency (TF-IDF weighted)") +
  ggtitle("US Airways")

# United
words_United <- words.tfidf.airline[words.tfidf.airline$group == "United",]
words_United <- words_United[order(words_United$frequency, decreasing = TRUE), ][1:15, ]
plot_United <- ggplot(words_United, aes(x = frequency, y = reorder(feature, frequency))) +
  geom_point() +
  ylab("") +
  xlab("Frequency (TF-IDF weighted)") +
  ggtitle("United")

# Virgin America
words_VirginAmerica <- words.tfidf.airline[words.tfidf.airline$group == "Virgin America",]
words_VirginAmerica <- words_VirginAmerica[order(words_VirginAmerica$frequency, decreasing = TRUE), ][1:15, ]
plot_VirginAmerica <- ggplot(words_VirginAmerica, aes(x = frequency, y = reorder(feature, frequency))) +
  geom_point() +
  ylab("") +
  xlab("Frequency (TF-IDF weighted)") +
  ggtitle("Virgin America")

# Southwest
words_Southwest <- words.tfidf.airline[words.tfidf.airline$group == "Southwest",]
words_Southwest <- words_Southwest[order(words_Southwest$frequency, decreasing = TRUE), ][1:15, ]
plot_Southwest <- ggplot(words_Southwest, aes(x = frequency, y = reorder(feature, frequency))) +
  geom_point() +
  ylab("") +
  xlab("Frequency (TF-IDF weighted)") +
  ggtitle("Southwest")

# American
words_American <- words.tfidf.airline[words.tfidf.airline$group == "American",]
words_American <- words_American[order(words_American$frequency, decreasing = TRUE), ][1:15, ]
plot_American <- ggplot(words_American, aes(x = frequency, y = reorder(feature, frequency))) +
  geom_point() +
  ylab("") +
  xlab("Frequency (TF-IDF weighted)") +
  ggtitle("American")

# JetBlue
words_JetBlue <- words.tfidf.airline[words.tfidf.airline$group == "JetBlue",]
words_JetBlue <- words_JetBlue[order(words_JetBlue$frequency, decreasing = TRUE), ][1:15, ]
plot_Jetblue <- ggplot(words_JetBlue, aes(x = frequency, y = reorder(feature, frequency))) +
  geom_point() +
  ylab("") +
  xlab("Frequency (TF-IDF weighted)") +
  ggtitle("JetBlue")

# Combine graphs
combined_airline_plot <- grid.arrange(plot_USAirways, plot_United, plot_VirginAmerica, plot_Southwest, plot_American, plot_Jetblue, nrow = 2, ncol = 3 )

# ii)
# Create my own dictionary
neg.words <- c("thanks", "thank", "great", "love", "awesome", "best")
pos.words <- c("cancelled", "delayed", "flightled", "late", "never", "waiting")

mydict <- dictionary(list(negative = neg.words, positive = pos.words))

# Make everything lower-case, remove numbers, punctuation, stop words and words appear in 5 or fewer tweets
# Weighted by document length
dfm_tweets <- tweetCorpus %>% 
  tokens(remove_numbers=T, remove_punct=T, include_docvars=T) %>%
  tokens_remove(stopwords("en")) %>% 
  dfm() %>% 
  dfm_trim(min_docfreq = 6) %>%
  dfm_weight(scheme = "prop")

# Apply the dictionary
sentiment <- dfm_lookup(dfm_tweets, dictionary = mydict)

# Convert into a data frame
sentiment <- convert(sentiment, to = "data.frame")

# Classify tweets as ‘negative’ if they contain more negative than positive language, and ‘positive’ otherwise
sentiment$score <- ifelse((sentiment$positive - sentiment$negative) > 0, 0, 1)
view(sentiment)

# Samples of tweets scoring 1 and 0
# Scoring 1
Score1 <- which(sentiment$score == 1)
random_text1 <- sample(Score1, 2)
tweets$text[random_text1]

# Scoring 0
Score0 <- which(sentiment$score == 0)
random_text0 <- sample(Score0, 2)
tweets$text[random_text0]

# iii)
# Use the lasso logit method to classify the tweets into ‘negative’ and ‘positive’
# Turn the document-term matrix object into a matrix
dfm_tweets <- as.matrix(dfm_tweets)
dfm_tweets <- cbind(tweets$sentiment, dfm_tweets)
dim(dfm_tweets)

# Split the data into cross-validation and test sets
set.seed(1)
cv.rows <- sample(nrow(dfm_tweets), (nrow(dfm_tweets)/2))
cv.data <- dfm_tweets[cv.rows,]
test.data <- dfm_tweets[-cv.rows,]

# Logit lasso on the cross-validation dataset
lasso.tweet <- cv.glmnet(x = cv.data[,2:ncol(dfm_tweets)], y = cv.data[,1], family = "binomial", type.measure = "class")
tweet.preds <- predict(lasso.tweet, test.data[,2:ncol(dfm_tweets)], type = "class")

# Number of words used for optimised lasso
lasso.coef <- as.matrix(coef(lasso.tweet))
length(lasso.coef[lasso.coef!=0])

# Most important words
lasso.coef <- coef(lasso.tweet)
lasso.coef <- as.matrix(lasso.coef[lasso.coef[,1]!=0,])
lasso.coef <- as.matrix(lasso.coef[order(lasso.coef[,1], decreasing=TRUE),])
view(lasso.coef)

# iv)
# Compare the performance of two classifiers
# Evaluate the performance of the dictionary
dictionary.table <- table(sentiment$score, tweets$sentiment)

(dictionary.table[1,2] + dictionary.table[2,1]) / (dictionary.table[1,1] + dictionary.table[1,2] + dictionary.table[2,1] + dictionary.table[2,2]) # Test Error Rate 
dictionary.table[2,2] / (dictionary.table[1,2] + dictionary.table[2,2]) # Sensitivity (True Positive Rate)
dictionary.table[1,1] / (dictionary.table[1,1] + dictionary.table[2,1]) # Specificity (True Negative Rate)

# Evaluate the performance of the lasso logit method 
lasso.table <- table(tweet.preds,test.data[,1])

(lasso.table[1,2] + lasso.table[2,1]) / (lasso.table[1,1] + lasso.table[1,2] + lasso.table[2,1] + lasso.table[2,2]) # Test Error Rate
lasso.table[2,2] / (lasso.table[1,2] + lasso.table[2,2]) # Sensitivity (True Positive Rate)
lasso.table[1,1] / (lasso.table[1,1] + lasso.table[2,1]) # Specificity (True Negative Rate)

