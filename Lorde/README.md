| Title  | Written by |  Date |
| ------------- | ------------- | ------------- |
| Why I Love Lorde  | Michelle Cao  |  03/05/2021 |


# Introduction

In 2018, I was obsessed with [Lorde](https://en.wikipedia.org/wiki/Lorde). I liked her breakout track [Royals](https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&cad=rja&uact=8&ved=2ahUKEwiFlu7h2Z7vAhWkm-AKHZ7DCzMQyCkwAHoECAYQAw&url=https%3A%2F%2Fwww.youtube.com%2Fwatch%3Fv%3DLFasFq4GJYM&usg=AOvVaw2qGWPnUmXVOH0jOXSZrhEk) well enough but couldn't get enough of her second album, [Melodrama](https://www.youtube.com/watch?v=zJuygTp7ydE&list=PLvm6B0LWgqu9pWrYmmC-6ETs7yDcfHyl9). She was with me on runs, on drives, in the shower, and before bed. Her music made me feel  electric and reflective all at once. In that year alone, I listened to 93 hours of Pure Heroine and Melodrama. 

![](img/IMG_56D95BEC7594-1.jpeg)

To this day, one of my most-played songs of all time is the Homemade Dynamite remix with Post Malone, Khalid, and SZA. A few of my other long-time favorites are Supercut, Green Light, Buzzcut Season, and Perfect Places. 

In thinking about why I like what I like and what it is about Lorde's music that connects with me, I realized that I didn't even know the answer to a basic question: is her music happy or sad? This [NPR article](https://www.npr.org/2018/08/31/638897130/lorde-is-the-21st-centurys-author-of-adolescent-evolution) describes her as minimalistic and dark pop, while this [Reddit thread](https://www.reddit.com/r/lorde/comments/gojch3/happy_songs/) decides her music is "happy sad, kind of like nostalgia." Was I drawn to her happy-sadness, nostalgia, or another emotion I couldn't yet identify?

I wanted to see if I could find an answer to this question before [her next album is released](https://www.nme.com/en_asia/news/music/lordes-third-album-title-is-inspired-by-her-trip-to-antarctica-2840657). For this analysis, I will be using R to analyze the Pure Heroine and Melodrama albums, and ignoring any EP's for simplicity and to prevent duplication.

## Loading Lyrics with Genius

```
library(tidyverse)
library(tidytext)
library(genius)  #to download lyrics from the Genius library
library(textdata)
library(wordcloud)
library(tm)
library(scales)  #for percentage scales in ggplot
library(ggraph)  #for network analysis
library(igraph)  #for network analysis
library(widyr) # for correlations between songs
```
I'll be using Josiah Parry's [Genius](https://github.com/JosiahParry/genius) package to easily access song lyrics.
```
lorde <- tribble(~ artist, ~ title, ~ type,
                "Lorde", "Pure Heroine","album",
                "Lorde", "Melodrama", "album")

lorde_lyrics <- lorde %>% add_genius(artist, title, type) 

lorde_lyrics_unnest_stop <- lorde_lyrics %>% unnest_tokens(word, lyric) %>% anti_join(stop_words)

lorde_lyrics_by_album <- factor(lorde_lyrics_unnest_stop, levels = c("Pure Heroine", "Melodrama"))
```

# Word Counts and Clouds
```
colnames(words_per_track)[colnames(words_per_track) == "length(word)"] <- "word_count"

words_per_track_bar <- ggplot(words_per_track, aes(x = reorder(track_title, word_count),
                                                  y = word_count)) + 
                                                geom_text(aes(label = word_count), vjust = 4, size = 2.5) +
                                                geom_bar(stat = "identity", fill = "blue") + 
                                                ggtitle("Word Count Per Track") + 
                                                ylab("Word Count") + xlab("Track Title") + 
                                                theme_minimal() + coord_flip()

words_per_track_bar
```
<img src="https://github.com/michellecow/media_analyses/blob/main/Lorde/img/words_per_track_bar.png" width="500">

```
# To get the most used word
lorde_lyrics_unnest_stop %>% count(word, sort = TRUE)
```

```
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

cloud <- wordcloud(corpus, max.words = 100, random.order = FALSE)
```
<img src="https://github.com/michellecow/media_analyses/blob/main/Lorde/img/Screen%20Shot%202021-03-07%20at%2012.55.12%20PM.png" width="400">

# Sentiment Analysis
```
# Analysis by album and song
lorde_sentiment_2 <- lorde_lyrics_unnest_stop %>% inner_join(get_sentiments("bing")) %>% 
    count(title, track_title, sentiment) %>% spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative)

lorde_sentiment_2
```
