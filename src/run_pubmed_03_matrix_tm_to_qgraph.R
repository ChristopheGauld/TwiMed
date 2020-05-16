#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : processing pipeline for Pubmed data
# date            : 2020-05-12
# version         : 2 (Ju)
# ==============================================================================


rm(list=ls())

library(qgraph)

input_file <- "data/pubmed_matrix.Rdata"
output_file <- "data/pubmed_qgraph.Rdata"

# load the pubmed term document matrix (matrix format)
load(input_file)

# transpose the matrix 
matrix_pubmed2 <- t(matrix_pubmed)

# compute a correlation matrix
cor_pubmed <- cor(matrix_pubmed2)
# plot the graph
e <- qgraph(cor_pubmed, 
       layout = "spring",labels=TRUE,posCol = "blue", negCol = "red",
       nodeNames = colnames(cor_pubmed),
       legend = TRUE)

# save to data folder
save(e, file = output_file)






