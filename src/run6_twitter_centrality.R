#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : processing pipeline for centralities with qgraph object
# date            : 2021-07-30
# version         : 5 (Guillaume Dumas)
# ==============================================================================

rm(list=ls())

library(qgraph)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)

input_file <- "../data/twitter_qgraph_50.Rdata"
output_file <- "../fig/fig4a.pdf"
load(input_file)

# matrix ponderation 
S <- getWmat(Q, directed = FALSE)

# centrality and clustering
pdf(file = output_file, width=14, height=14)
centralityPlot(S,
               include = c("Strength","ExpectedInfluence","Betweenness",
                          "Closeness"), theme_bw = TRUE, print = TRUE, verbose = TRUE, 
                          weighted = TRUE,signed = TRUE,
               orderBy = "Strength", decreasing = FALSE)
dev.off()
