#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : processing pipeline to create matrix
# date            : 2020-05-12
# version         : 2 (Ju)
# ==============================================================================




rm(list=ls())

library(dplyr)
library(stringr)
library(tidytext)

input_file <- "../data/twitter_Tue May 12 15:31:03 2020.RData"
output_file <- "../results/raincloudplot_hashtag.pdf"

# load the twitter dataframe
load(input_file)

# put an id for each tweet
autis$id = rownames(autis)
autis_tweets = autis[,c("text","id")]

# split tweets and remove url
tidy_tweeter <- autis_tweets %>%
  unnest_tokens(word, text, token="tweets", strip_url=TRUE)

# keep only hashtag
tidy_hashtag <- tidy_tweeter[str_detect(tidy_tweeter$word,"#"),]
# remove autis* and covid19 words
tidy_hashtag2 <- tidy_hashtag[!str_detect(tidy_hashtag$word,"autis"),]
tidy_hashtag3 <- tidy_hashtag2[tidy_hashtag2$word != "#asd",]
tidy_hashtag3 <- tidy_hashtag3[tidy_hashtag3$word != "#covid19",]

# Save the 15 most frequent words
tidy_hashtag4 <- count(tidy_hashtag3,word,sort=TRUE)
words15 = tidy_hashtag4$word[1:15]


