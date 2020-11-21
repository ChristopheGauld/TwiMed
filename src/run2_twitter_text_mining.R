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
results_twitter$id <- rownames(results_twitter)
autis_tweets <- results_twitter[,c("text","id")]

# remove URL from tweets
autis_tweets$text <- gsub("http.*","",  autis_tweets$text)
autis_tweets$text <- gsub("https.*","", autis_tweets$text)

# split tweets and remove url
tidy_twitter <- autis_tweets %>%
  	unnest_tokens(word, text)

# cleaning
data("stop_words")
stop_words = rbind (stop_words,"amp","sm","asd","autism","asd", "1", "2", "3", "4", "5", "6", "7", "8", "9", "covid", "2020", "behavioral","increased","found","identified","patients","reported","including","developing","examined","participants","suggest","compared","significantly","based","na","findings","related","results","children","significant","spectrum","study","control","provide","review","studies","effects","analysis","specific","age","data","behaviors","observed","potential","lower","included","scale","it...s","it's")
tidy_twitter2 <- tidy_twitter %>% 
            anti_join(stop_words)

tidy_twitter3 <- tidy_twitter2[!str_detect(tidy_twitter2$word,"autis"),]

# count frequency of each word 
tidy_twitter4 <- dplyr::count(tidy_twitter3, id, word, sort=TRUE)

# convert to a dtm 
dtm_twitter <- cast_dtm(tidy_twitter4, id, word, n)

# save
save(tidy_twitter5, dtm_twitter, autis_tweets, file = output_file)
