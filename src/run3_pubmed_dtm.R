#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : processing pipeline to create matrix
# date            : 2020-05-12
# version         : 3
# ==============================================================================



rm(list=ls())

library(topicmodels)
library(tidytext)
library(dplyr)

setwd("~/Desktop/R/TwiMed/DATAS")
input_file <- "data/pubmed_tdm.Rdata" 
output_file <- "data/pubmed_tdm_group.Rdata"

# load the pubmed dataframe
load(input_file)

# set a seed so that the output of the model is predictable
lda_pubmed <- LDA(dtm_pubmed, k = 7, control = list(seed = 1234))

# compute word-topic probabilities
topics_prob_pubmed <- tidy(lda_pubmed, matrix = "beta")

# select for each term the topic with the highest probability of generating it
topic_max <- topics_prob_pubmed %>%
  group_by(term) %>%
  filter(beta == max(beta)) %>%
  ungroup()

# Put a list of the 4 groups of terms, class by topic selected above
topic1 <- topic_max$term[topic_max$topic==1]
topic2 <- topic_max$term[topic_max$topic==2]
topic3 <- topic_max$term[topic_max$topic==3]
topic4 <- topic_max$term[topic_max$topic==4]
group <- list(topic1,topic2,topic3,topic4)

# create a cor matrix of the dtm
matrix_pubmed <- as.matrix(dtm_pubmed)

# save
save(matrix_pubmed, group, file = output_file)


