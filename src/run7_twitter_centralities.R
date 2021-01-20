#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : processing pipeline for centralities with qgraph object
# date            : 2021-01-19
# version         : 4 (Guillaume Dumas)
# ==============================================================================

rm(list=ls())

library(qgraph)

input_file <- "../data/twitter_qgraph_50.Rdata"
output_file1 <- "../fig/fig3c.pdf"
output_file2 <- "../fig/fig3d.pdf"
load(input_file)

# matrix ponderation 
S <- getWmat(Q,directed = FALSE)

# Measures of centrality
deg <- centrality(Q)$OutDegree
bet <- centrality(Q)$Betweenness
clo <- centrality(Q)$Closeness

# centrality and clustering
pdf(file = output_file1, width=14, height=14)
centralityPlot(S, labels = Labels, scale = c("z-scores", "raw", "raw0","relative"),
               include = c("Degree","Strength","OutDegree","InDegree","OutStrength",
                          "InStrength"), theme_bw = TRUE, print = TRUE, verbose = TRUE,
               standardized, relative, weighted = TRUE,signed = TRUE,
               orderBy = "default", decreasing = FALSE)
dev.off()

pdf(file = output_file2, width=14, height=14)
clusteringPlot(S, scale = c("z-scores", "raw", "raw0","relative"), labels , signed = FALSE, theme_bw = TRUE, print = TRUE,verbose = TRUE)
dev.off()