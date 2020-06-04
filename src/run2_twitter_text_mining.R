#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : processing pipeline to create matrix
# date            : 2020-05-12
# version         : 2 (Ju)
# ==============================================================================

library(dplyr)
library(stringr)
library(tidytext)

input_file <- "../data/twitter.Rdata"
output_file <- "../data/twitter_tdm.Rdata"

# load the twitter dataframe
load(input_file)

# put an id for each tweet
autis$id <- rownames(autis)
autis_tweets <- autis[,c("text","id")]

# remove URL from tweets
autis_tweets$text <- gsub("http.*","",  autis_tweets$text)
autis_tweets$text <- gsub("https.*","", autis_tweets$text)

# split tweets and remove url
tidy_twitter <- autis_tweets %>%
  	unnest_tokens(word, text)

# cleaning
data("stop_words")
stop_words = rbind (stop_words,"amp","sm",”asd”)
tidy_twitter2 <- words %>% 
            anti_join(stop_words)

tidy_twitter3 <- tidy_tweeter2[!str_detect(tidy_twitter2$word,"autis"),]
tidy_twitter4 <- tidy_tweeter3[!str_detect(tidy_twitter3$word,c("1","2","3","4","5","6","7","8","9"))]

# count frequency of each word 
tidy_twitter5 <- dplyr::count(tidy_twitter4, word,sort=TRUE)

# convert to a dtm
dtm_twitter <- cast_dtm(tidy_twitter5, word, id, n)

# save
save(tidy_twitter5,dtm_twitter,autis_tweets, file = output_file)

