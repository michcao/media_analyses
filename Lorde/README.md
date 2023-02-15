| Title  | Written by |  Date |   Modified On |
| ------------- | ------------- | ------------- | ------------- |
| Why I Love Lorde  | Michelle Cao  |  03/05/2021 |   02/14/2022  |


# Introduction: Lorde's Happy-Sadness

In 2018, I was obsessed with [Lorde](https://en.wikipedia.org/wiki/Lorde). I liked her breakout hit [Royals](https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&cad=rja&uact=8&ved=2ahUKEwiFlu7h2Z7vAhWkm-AKHZ7DCzMQyCkwAHoECAYQAw&url=https%3A%2F%2Fwww.youtube.com%2Fwatch%3Fv%3DLFasFq4GJYM&usg=AOvVaw2qGWPnUmXVOH0jOXSZrhEk) well enough but couldn't get enough of her second album, [Melodrama](https://www.youtube.com/watch?v=zJuygTp7ydE&list=PLvm6B0LWgqu9pWrYmmC-6ETs7yDcfHyl9). She was with me on runs, on drives, in the shower, and before bed. Her music made me feel  electric and reflective all at once. And in that year alone, I listened to 93 hours of _Pure Heroine_ and _Melodrama_. 

![](img/IMG_56D95BEC7594-1.jpeg)

To this day, one of my most-played songs of all time is the "Homemade Dynamite" remix with Post Malone, Khalid, and SZA. Once, I even made a rideshare driver play it on repeat for over an hour, singing along nonstop. Among her other songs, I'm also partial to "Supercut," "Green Light," "Buzzcut Season," and "Perfect Places." 

In thinking about why I like what I like and what it is about Lorde's music that connects with me, I realized that I didn't even know the answer to this basic question: is her music happy or sad? This [NPR article](https://www.npr.org/2018/08/31/638897130/lorde-is-the-21st-centurys-author-of-adolescent-evolution) describes her as minimalistic and dark pop, while this [Reddit thread](https://www.reddit.com/r/lorde/comments/gojch3/happy_songs/) decides her music is "happy sad, kind of like nostalgia." Was I drawn to her happy-sadness, nostalgia, or some other emotion I couldn't identify?

I wanted to see if I could find an answer to this question before [her next album release](https://www.nme.com/en_asia/news/music/lordes-third-album-title-is-inspired-by-her-trip-to-antarctica-2840657). For this analysis, I will be using R to analyze the _Pure Heroine_ and _Melodrama_ albums, and ignoring any EP's for simplicity and to prevent duplication.

## Loading Song Lyrics with Genius
First, I will load all the relevant packages and libraries needed.
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
Next, I'll be using Josiah Parry's [Genius](https://github.com/JosiahParry/genius) package to easily access song lyrics. He also [provides a great discussion and resource](https://josiahparry.com/post/2019-05-08-genius-learnr-tutorial/) for using his package.
```
lorde <- tribble(~ artist, ~ title, ~ type,
                "Lorde", "Pure Heroine","album",
                "Lorde", "Melodrama", "album")

lorde_lyrics <- lorde %>% add_genius(artist, title, type) 

lorde_lyrics_unnest_stop <- lorde_lyrics %>% unnest_tokens(word, lyric) %>% anti_join(stop_words)

lorde_lyrics_by_album <- factor(lorde_lyrics_unnest_stop, levels = c("Pure Heroine", "Melodrama"))
```

# Word Counts and Clouds
Now that we have the lyrics downloaded, it's time to look at some statistics and get to know the data.
```
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
```
<img src="https://github.com/michellecow/media_analyses/blob/main/Lorde/img/words_per_track_bar.png" width="500">
At a length of 6:07, "Hard Feelings/Loveless" is the longest song of both albums and makes sense as the song with the most words. Similarly, "Liability (Reprise)" is the shortest song from both albums and is found to be the song with the least number of words.

```
# To get the most used word
lorde_lyrics_unnest_stop %>% count(word, sort = TRUE)
```

Running the code above tells us that Lorde's five most-used words across both albums are:

- boom
- love
- call
- people
- yeah

While this doesn't tell us too much, it does imply a fairly consistent theme of love, or at least the mention of it. 

Next, I will generate a corpus and perform a few pre-processing steps (converting to lowercase, stemming, and removing whitespace, punctuation, stopwords) to create a DTM/DFM. This will allow me to generate a wordcloud and visualize her top words.

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
To analyze the sentiment of each album overall, I use the [bing tidytext lexicon](https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html) to categorize words into positive and negative groups. This lexicon, as well as the NRC lexicon used later on, is based on unigrams, or single words. An alternative approach could be to use bi- or even trigrams to provide more context.

```
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
```
<img src="https://github.com/michellecow/media_analyses/blob/main/Lorde/img/sent_bing_album.png" width="500">

Based on the above analysis, her lyrics are generally more negative than positive in sentiment. A further breakdown of the frequency of negative words, according to the bing lexicon, by album shows that _Melodrama_ uses many more words associated with a negative sentiment.

```
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
```
<img src="https://github.com/michellecow/media_analyses/blob/main/Lorde/img/neg_word_per_album.png" width="400">

Focusing on the sentiment of each song, organized by album, shows interesting results. The most negative song in the Pure Heroine album, "A World Alone," is in my opinion actually quite upbeat and considerably more face-paced than some of her other songs. This [Lorde fandom page](https://lorde.fandom.com/wiki/A_World_Alone) even says that it contains a "roaring dance beat." This could be a general weakness of text analysis, especially using unigrams, as it's difficult to get a sense of a song's mood, content, and overall feeling. Another song that doesn't seem to fit its categorization is "Sober II (Melodrama)."  It is shown as a song with relative positive sentiment, and is from my view actually a very sad song, one that I would describe as having negative sentiment.

```
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
```
<img src="https://github.com/michellecow/media_analyses/blob/main/Lorde/img/sent_album_and_song.png" width="500">

Zooming in more to see which lyrics exactly are contributing to the overall levels of positive and negative sentiment, I find that the two words "boom" and "love" are contributing very high levels of positive sentiment. And as shown earlier, these two words are used most frequently across both albums.

```
# Analysis by emotion according to dictionary chosen

# bing lexicon
lorde_bing <- lorde_lyrics_unnest_stop %>% 
    inner_join(get_sentiments("bing")) %>% count(word, sentiment, sort = TRUE) %>% 
    ungroup() %>% group_by(sentiment) %>% top_n(10) %>% ungroup() %>% 
    mutate(word = reorder(word, n)) %>% ggplot(aes(word, n, fill = sentiment)) +
    geom_col(show.legend = FALSE) + facet_wrap(~sentiment, scales = "free_y") +
    labs(y = "Contribution to sentiment", x = NULL) + theme_minimal() + coord_flip()

lorde_bing
```
<img src="https://github.com/michellecow/media_analyses/blob/main/Lorde/img/sent_bing_negpos.png" width="450">

As mentioned earlier, another option is to use the [NRC lexicon](http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm), which classifies words in a binary fashion into the designated categories: anger, anticipation, disgust, fear, joy, negative, positive, sadness, surprise, and trust. Similar to the code above, we can use the NRC lexicon to examine which lyrics are contributing to sentiment by category. Possibly due to the difference in sentiment dictionary and associated methodology, the word "boom" does not appear as contributing to positive sentiment as it does using the bing lexicon. 

```
# NRC lexicon
lorde_nrc <- lorde_lyrics_unnest_stop %>% 
    inner_join(get_sentiments("nrc")) %>% count(word, sentiment, sort = TRUE) %>% 
    ungroup() %>% group_by(sentiment) %>% top_n(10) %>% ungroup() %>% 
    mutate(word = reorder(word, n)) %>% ggplot(aes(word, n, fill = sentiment)) +
    geom_col(show.legend = FALSE) + facet_wrap(~sentiment, scales = "free_y") +
    labs(y = "Contribution to sentiment", x = NULL) + theme_minimal() + coord_flip()

lorde_nrc
```
<img src="https://github.com/michellecow/media_analyses/blob/main/Lorde/img/sent_nrc_emotions.png" width="500">


## Correlation and Network Analysis
The relationship between Lorde's albums can be visualized with the code below. The center diagonal in the graph shows us where lyrics would show up if used with the same frequency.

```
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
```
<img src="https://github.com/michellecow/media_analyses/blob/main/Lorde/img/correlation.png" width="450">

All of the words are spread out across both albums, meaning that the words or themes are not that similar across both after all. Comparing _Pure Heroine_ and _Melodrama_, Lorde often seems to sing about love, calls, and home. In _Melodrama_, she focuses more on light, waiting, dancing, and the dark. Of note in _Pure Heroine_, she spends more of the album singing about people and teeth, possibly starting with its prominence in "Royals." For these last two lyrics, we would probably need more context to figure out what that means.

Thinking more about how songs are related to each other, not just between albums, we can generate a network map to see how songs are related. While the correlation threshold I use is not very high (at least 0.05), we can still see generally how songs are related. 

```
# Network Analysis
lorde_cors <- lorde_lyrics_unnest_stop %>% pairwise_cor(track_title, word, sort = TRUE)

set.seed(123)

lorde_cors %>% filter(correlation > .05) %>% graph_from_data_frame() %>% ggraph(layout = "fr") + 
    geom_edge_link(show.legend = FALSE, aes(edge_alpha = correlation)) + 
    geom_node_point(size = 4) + geom_node_text(aes(label = name), repel = TRUE, size = 3.5) + 
    theme_void()
```
<img src="https://github.com/michellecow/media_analyses/blob/main/Lorde/img/network_analysis.png" width="500">

While songs from the two albums don't seem to overlap too much, there are a few that do cross over from the first album: "Glory and Gore" via "Buzzcut Season," "Ribs," "400 Lux," and "A World Alone." However, the connections from these _Pure Heroine_ songs don't seem to be as strong as between songs from _Melodrama_. 

Examining the relationships between the two albums through the frequency plot and network analysis doesn't tell us much about the sentiments of Lorde's music specifically but does show that the words she chooses are more unique to each album and used in different ways, contributing to different sentiments between albums.

# Conclusion
Is Lorde's music happy or sad? According to the different analyses, her songs are generally more negative than positive, with the _Melodrama_ album more negative overall than the _Pure Heroine_ album. While good to know, it's also important to keep in mind that these results depend entirely on the lexicon chosen and the approach to text analysis used. In addition, I don't agree with all of the sentiments calculated by song. In particular, I feel happy when I hear "Supercut" and "Green Light," and I'm sad when I hear "The Louvre" and "Sober II " – both in contrast to what we found using the NRC lexicon. In conclusion, I would describe Lorde's music as sad happy, which is a slight adjustment to the description used by the Redditors linked above.

## References
Primary references I leaned on were from [Sophn8](https://github.com/sophn8/music_analyses/tree/master/TSwift) and [aaumaitre](https://github.com/aaumaitre/taylor_swift/blob/master/README.md). Other resources I consulted were from [Deeply Trivial](http://www.deeplytrivial.com/2018/05/statistics-sunday-welcome-to-sentiment.html), [Cristobal Veas](https://towardsdatascience.com/how-to-analyze-emotions-and-words-of-the-lyrics-from-your-favorite-music-artist-bbca10411283), [Rosie](https://rpubs.com/RosieB/taylorswiftlyricanalysis), and this one from [PromptCloud](https://www.promptcloud.com/blog/data-visualization-text-mining-taylor-swift-song-lyrics/).
