#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : extract Twitter data used in the study
# date            : 2020-04-24
# version         : 1
# ==============================================================================

library(rtweet)

output_file <- paste0("../data/twitter", format(Sys.time(),'_%Y-%m-%d'), ".csv")

# Tutorial: https://rtweet.info/articles/intro.html

autis <- search_tweets(q = "#autism", n = 10000,
                        lang = "en",
                        include_rts = FALSE, retryonratelimit = TRUE)

# TODO: Add save to ==> CSV (use output_file) or RData to start...