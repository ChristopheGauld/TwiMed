#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : processing pipeline for centralities with qgraph object
# date            : 2020-05-04
# version         : 4
# ==============================================================================

library(qgraph)
# matrix ponderation 
S <- getWmat(Q,directed = FALSE)
# Measures of centrality
deg <- centrality(Q)$OutDegree
bet <- centrality(Q)$Betweenness
clo <- centrality(Q)$Closeness
# centrality and clustering
centralityPlot(S, labels = Labels, scale = c("z-scores", "raw", "raw0","relative"),
               include =c("Degree","Strength","OutDegree","InDegree","OutStrength",
                          "InStrength"), theme_bw = TRUE, print = TRUE, verbose = TRUE,
               standardized, relative, weighted = TRUE,signed = TRUE,
               orderBy = "default", decreasing = FALSE)
clusteringPlot(S, scale = c("z-scores", "raw", "raw0","relative"), labels , signed = FALSE, theme_bw = TRUE, print = TRUE,verbose = TRUE)



