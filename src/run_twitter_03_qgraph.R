#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : processing pipeline for qgraph from Twitter matrix
# date            : 2020-05-12
# version         : 1
# ==============================================================================

# Use tidy_hashtag4 from run_twitter_02_matrix.R



# create a igraph object
library(igraph)
g <- graph_from_data_frame(tidy_hashtag4, directed = FALSE, vertices = NULL)
# extract the adjacency matrix of the graph
r <- as_adjacency_matrix(g)
# create a qgraph object
library(qgraph)
Q <- qgraph(r,layout="spring")
# save
save(Q, file = output_file)
