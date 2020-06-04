#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : extract Twitter data used in the study
# date            : 2020-04-24
# version         : 1
# ==============================================================================

library(rtweet)

output_file <- "../data/twitter.Rdata"

# Tutorial: https://rtweet.info/articles/intro.html

results_twitter <- search_tweets(q = "#autism", n = 10000,
                                 lang = "en",
                                 include_rts = FALSE,
                                 retryonratelimit = TRUE)

save(results_twitter, file = output_file)
