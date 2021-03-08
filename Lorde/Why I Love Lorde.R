############################
# Title: Why I Love Lorde  #
# Written by: Michelle Cao #
# Date: 03/05/2021         #
############################

# Introduction: Lorde's Happy-Sadness
rm(list=ls())

# Loading Song Lyrics with Genius
library(tidyverse)
library(tidytext)
library(genius)
library(textdata)
library(wordcloud)
library(tm)
library(scales)
library(ggraph)
library(igraph)
library(widyr)

library(reshape2)

lorde <- tribble(~ artist, ~ title, ~ type,
                "Lorde", "Pure Heroine","album",
                "Lorde", "Melodrama", "album")

lorde_lyrics <- lorde %>% add_genius(artist, title, type) 

lorde_lyrics_unnest_stop <- lorde_lyrics %>% unnest_tokens(word, lyric) %>% anti_join(stop_words)

lorde_lyrics_by_album <- factor(lorde_lyrics_unnest_stop, levels = c("Pure Heroine", "Melodrama"))

lorde_lyrics_by_album

# Word Counts and Clouds
words_per_track <- lorde_lyrics_unnest_stop %>%
    group_by(track_title) %>% summarize(length(word))

# To change the column title to "word_count"
colnames(words_per_track)[colnames(words_per_track) == "length(word)"] <- "word_count"

# To get the number of words per song and sort from most to least
words_per_track_bar <- ggplot(words_per_track, aes(x = reorder(track_title, word_count),
                                                  y = word_count)) + 
                                                geom_text(aes(label = word_count), vjust = 4, size = 2.5) +
                                                geom_bar(stat = "identity", fill = "blue") + 
                                                ggtitle("Word Count Per Track") + 
                                                ylab("Word Count") + xlab("Track Title") + 
                                                theme_minimal() + coord_flip()

words_per_track_bar

# To get the most used word
lorde_lyrics_unnest_stop %>% count(word, sort = TRUE)

# Create raw corpus from genius lyrics
corpus_raw <- Corpus(VectorSource(lorde_lyrics$lyric))

# To lowercase
corpus <- tm_map(corpus_raw, content_transformer(tolower))

# Strip whitespace
corpus <- tm_map(corpus, stripWhitespace)

# Remove punctuation
corpus <- tm_map(corpus, removePunctuation)

# Remove stopwords
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# Stem the document
corpus <- tm_map(corpus, stemDocument)

# Create document term matrix
dtm <- (DocumentTermMatrix(corpus))

# Tidy dtm
corpus_tidy <- tidy(dtm)
    corpus_tidy %>%
    bind_tf_idf(term, document, count) %>%
    arrange(desc(tf_idf))

corpus_tidy

cloud <- wordcloud(corpus, max.words = 100, random.order = FALSE)

# Sentiment Analysis

# Analysis by album
lorde_lyrics_bing <- lorde_lyrics_unnest_stop %>% inner_join(get_sentiments("bing")) %>%
  count(title, index = line, sentiment) %>% spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

# Use the bing lexicon to analyze sentiment by album
lorde_lyrics_bing_by_album <- factor(lorde_lyrics_bing, levels = c("Pure Heroine", "Melodrama"))

lorde_lyrics_bing_album <- ggplot(lorde_lyrics_bing, aes(index, sentiment, fill = title)) +
  geom_col(show.legend = FALSE) + facet_wrap(~title, ncol = 3, scales = "free_x") + theme_minimal() + 
  scale_fill_manual(values = c("#0000ff", "#000000"))

lorde_lyrics_bing_album

# Raw number count of negative words per album
lorde_lyrics_bing_sent <- lorde_lyrics_unnest_stop %>%
  inner_join(get_sentiments("bing"))

lorde_lyrics_bing_sent_title <- lorde_lyrics_bing_sent %>%
  subset(sentiment == "negative") %>%
  group_by(title) %>% summarise(sentiment = n()) %>%
  ungroup()

lorde_lyrics_bing_sent_title_bar <- lorde_lyrics_bing_sent_title %>%
  ggplot(aes(x = title, y = sentiment)) + geom_bar(stat = "identity") +
  ggtitle("Negative Word Count Per Album") + theme_minimal() + xlab("Album")

lorde_lyrics_bing_sent_title_bar

# Analysis by album and song
lorde_sentiment_2 <- lorde_lyrics_unnest_stop %>% inner_join(get_sentiments("bing")) %>% 
    count(title, track_title, sentiment) %>% spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative)

lorde_sentiment_2$title <- factor(lorde_sentiment_2$title, levels = c("Pure Heroine", "Melodrama"))

sent_2 <- lorde_sentiment_2 %>% ggplot(aes(reorder(track_title, sentiment), sentiment, fill = title)) +
    geom_col(show.legend = FALSE) + facet_wrap(~title, ncol = 3, scales = "free") +
    labs(x = NULL, y = "Sentiment", title = "Lorde's songs ranked by sentiment") + 
    theme(plot.title = element_text(size = 13, hjust = 0.4, face = "bold"), 
         axis.title.y = element_text(hjust = 0.05, size = 7), 
         axis.title.x = element_text(size = 8)) + theme_minimal() + coord_flip() 

sent_2 + scale_fill_manual(values = c("#000000","#0000ff"))

# Analysis by emotion according to dictionary chosen
lorde_bing <- lorde_lyrics_unnest_stop %>% 
  inner_join(get_sentiments("bing")) %>% count(word, sentiment, sort = TRUE) %>% 
  ungroup() %>% group_by(sentiment) %>% top_n(10) %>% ungroup() %>% 
  mutate(word = reorder(word, n)) %>% ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) + facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment", x = NULL) + theme_minimal() + coord_flip()

lorde_bing

# NRC lexicon
lorde_nrc <- lorde_lyrics_unnest_stop %>% 
    inner_join(get_sentiments("nrc")) %>% count(word, sentiment, sort = TRUE) %>% 
    ungroup() %>% group_by(sentiment) %>% top_n(10) %>% ungroup() %>% 
    mutate(word = reorder(word, n)) %>% ggplot(aes(word, n, fill = sentiment)) +
    geom_col(show.legend = FALSE) + facet_wrap(~sentiment, scales = "free_y") +
    labs(y = "Contribution to sentiment", x = NULL) + theme_minimal() + coord_flip()

lorde_nrc

# Correlation and Network Analysis

# Correlation Analysis
lorde_frequency <- lorde_lyrics_unnest_stop %>% count(title, word) %>% group_by(title) %>% 
    mutate(proportion = n / sum(n)) %>% select(-n) %>% spread(title, proportion) %>% 
    gather(title, proportion, c(`Pure Heroine`))

lorde_frequency %>% ggplot(aes(x = proportion, y = `Melodrama`)) + 
    geom_abline(lty = 2) + geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) + 
    geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) + 
    scale_x_log10(labels = percent_format()) + scale_y_log10(labels = percent_format()) +
    facet_wrap(~title, nrow = 1, strip.position = "bottom") + coord_equal() + theme_minimal() +
    labs(x = "Word Frequency", y = "Melodrama", title = "Comparing Lorde's Albums")

# Network Analysis
lorde_cors <- lorde_lyrics_unnest_stop %>% pairwise_cor(track_title, word, sort = TRUE)

set.seed(123)

lorde_cors %>% filter(correlation > .05) %>% graph_from_data_frame() %>% ggraph(layout = "fr") + 
    geom_edge_link(show.legend = FALSE, aes(edge_alpha = correlation)) + 
    geom_node_point(size = 4) + geom_node_text(aes(label = name), repel = TRUE, size = 3.5) + 
    theme_void()