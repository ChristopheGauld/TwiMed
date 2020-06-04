#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : processing pipeline for qgraph from Twitter matrix
# date            : 2020-06-04
# version         : 4
# ==============================================================================

rm(list=ls())

input_file <- "../data/twitter_tdm_group.Rdata"
output_file <- "../data/twitter_qgraph.Rdata"

library(qgraph)

# load the pubmed tidy object and the terms order by group after topic modeling
load(input_file)

# Selection of the 300 most frequent words to plot the graph
nNode <- 300
freq_word <- dplyr::top_n(dplyr::count(tidy_twitter4, word),nNode, n)

matrix_reduite <- matrix_twitter[,freq_word$word]
cor_matrix_reduite <- cor(matrix_reduite)
group1_reduit <- which(colnames(cor_matrix_reduite) %in% group[[1]]) 
group2_reduit <- which(colnames(cor_matrix_reduite) %in% group[[2]]) 
group3_reduit <- which(colnames(cor_matrix_reduite) %in% group[[3]]) 
group4_reduit <- which(colnames(cor_matrix_reduite) %in% group[[4]]) 
group_matrix_reduite <- list(group1_reduit,group2_reduit,group3_reduit,group4_reduit)

# create a qgraph object
Q <- qgraph(cor_matrix_reduite, layout = "spring", posCol = "blue", negCol = "red",
           nodeNames = colnames(cor_matrix_reduite), legend.cex = 0.2,
           groups = group,
           #minimum = .15, # Do not display edges with weak correlation to facilitate graphic loading
           #repulsion = 100, # Increase the distance between the nodes to improve the visualization in "cluster"
           legend.mode = "groups",
           threshold = "bonferroni",sampleSize = nrow(cor_matrix_reduite), alpha = 0.05) # To take into account only statistically significant correlations, taking into account the inflation of alpha risk by multiple tests via the Bonferroni method.
           pastel = TRUE,
           #labels =TRUE)
           #vsize = log(freq_word$n),
           #color = color,
           #title = titre)

# save
save(Q, file = output_file)

