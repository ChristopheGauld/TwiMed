#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : processing pipeline to create matrix
# date            : 2020-05-12
# version         : 2
# ==============================================================================




rm(list=ls())

library(tidytext)
library(stringr)

setwd("~/Desktop/R/TwiMed/DATAS")
input_file <- "data/pubmed.Rdata" # Le premier dataframe du prg 1
output_file <- "data/pubmed_tdm.Rdata"

# load the pubmed dataframe
load(input_file)

# select abstracts and PMID
ab.pubmed <- results_pubmed[,c("PMID","AB")]

# split abstracts into words
tidy.pubmed <- unnest_tokens(ab.pubmed,word, AB)

# remove common words and words containing "autism" or "disorder"
data("stop_words")
stop_words = rbind(stop_words,"autism","asd", "1", "2", "3", "4", "5", "6", "7", "8", "9")
tidy.pubmed2 <- dplyr::anti_join(tidy.pubmed,stop_words)
tidy.pubmed2 <- tidy.pubmed2[!str_detect(tidy.pubmed2$word,"autis"),]
tidy.pubmed2 <- tidy.pubmed2[!str_detect(tidy.pubmed2$word,"disorder"),]

# count frequency of each word in each abstract
tidy.pubmed3 <- dplyr::count(tidy.pubmed2, PMID, word)

# convert to a dtm
dtm_pubmed <- cast_dtm(tidy.pubmed3, PMID, word, n)

# save
save(dtm_pubmed, file = output_file)

