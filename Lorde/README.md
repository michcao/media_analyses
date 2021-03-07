| Title  | Written by |  Date |
| ------------- | ------------- | ------------- |
| Why I Love Lorde  | Michelle Cao  |  03/05/2021 |


# Introduction

In 2018, I was obsessed with [Lorde](https://en.wikipedia.org/wiki/Lorde). I liked her breakout track [Royals](https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&cad=rja&uact=8&ved=2ahUKEwiFlu7h2Z7vAhWkm-AKHZ7DCzMQyCkwAHoECAYQAw&url=https%3A%2F%2Fwww.youtube.com%2Fwatch%3Fv%3DLFasFq4GJYM&usg=AOvVaw2qGWPnUmXVOH0jOXSZrhEk) well enough but couldn't get enough of her second album, [Melodrama](https://www.youtube.com/watch?v=zJuygTp7ydE&list=PLvm6B0LWgqu9pWrYmmC-6ETs7yDcfHyl9). She was with me on runs, on drives, in the shower, and before bed. Her music made me feel  electric and reflective all at once. In that year alone, I listened to 93 hours of Pure Heroine and Melodrama. 

![Lorde1](img/IMG_56D95BEC7594-1.jpeg)

To this day, one of my most-played songs of all time is the Homemade Dynamite remix with Post Malone, Khalid, and SZA. A few of my other long-time favorites are Supercut, Green Light, Buzzcut Season, and Perfect Places. 

In thinking about why I like what I like and what it is about Lorde's music that connects with me, I realize that I don't know the answer to a basic question: is her music happy or sad? This [NPR article](https://www.npr.org/2018/08/31/638897130/lorde-is-the-21st-centurys-author-of-adolescent-evolution) described her as minimalistic and dark pop, while this [Reddit thread](https://www.reddit.com/r/lorde/comments/gojch3/happy_songs/) decided her music as "happy sad, kind of like nostalgia." Was it nostalgia I felt, or a shapeless emotion that was all in my head?

Before Lorde's next album is released (~ hopefully soon ~), I wanted to see if I could find an answer to the first question. For this analysis, I will be using R to analyze the Pure Heroine and Melodrama albums. For the purposes of this research, I will be ignoring any EP's to prevent duplication.

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

