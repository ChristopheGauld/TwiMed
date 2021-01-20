#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : processing pipeline for qgraph from Twitter matrix
# date            : 2021-01-19
# version         : 5 (Guillaume Dumas)
# ==============================================================================

rm(list=ls())

input_file1 <- "../data/twitter_tdm.Rdata"
input_file2 <- "../data/twitter_tdm_group.Rdata"
output_file1 <- "../fig/twitter_graph_50.pdf"
output_file2 <- "../data/twitter_qgraph_50.Rdata"

library(qgraph)
library(NbClust)

# load the pubmed tidy object and the terms order by group after topic modeling
load(input_file1)
load(input_file2)

# Selection of the 50 most frequent words to plot the graph
nNode <- 50
freq_word <- dplyr::top_n(dplyr::count(tidy_twitter2, word), nNode, n)
matrix_reduite <- matrix_twitter[, freq_word$word]
cor_matrix_reduite <- cor(matrix_reduite)

# Best numbers of Clusters
rest<-NbClust(cor_matrix_reduite, distance = "euclidean", min.nc=2, max.nc=8, 
             method = "complete", index = "ch")
rest$All.index
rest$Best.nc
rest$Best.partition

# Groups
group1_reduit <- which(colnames(cor_matrix_reduite) %in% group[[1]]) 
group2_reduit <- which(colnames(cor_matrix_reduite) %in% group[[2]]) 
group3_reduit <- which(colnames(cor_matrix_reduite) %in% group[[3]]) 
group4_reduit <- which(colnames(cor_matrix_reduite) %in% group[[4]]) 
group_matrix_reduite <- list(group1_reduit,group2_reduit,group3_reduit,group4_reduit)

# create a qgraph object
pdf(file = output_file1)
Q <- qgraph(cor_matrix_reduite, layout = "groups", posCol = "red", negCol = "NA",
            nodeNames = colnames(cor_matrix_reduite), legend.cex = 0.2,
            groups = group_matrix_reduite,
            label.scale = TRUE,
            curveAll = TRUE,
            labels = TRUE,
            minimum = 0.08, 
            repulsion = 1., 
            legend.mode = "style2",
            nodeNames= TRUE,
            sampleSize = nrow(cor_matrix_reduite),
            alpha = 0.05, #inflation du risque alpha par les tests multiples via la méthode de Bonferroni.
            palette = "pastel",
            vsize = 4,
            cut = 0.3)
dev.off()

# save
save(Q, file = output_file2)


rm(list=ls())

input_file1 <- "../data/twitter_tdm.Rdata"
input_file2 <- "../data/twitter_tdm_group.Rdata"
output_file1 <- "../fig/twitter_graph_2000.pdf"
output_file2 <- "../data/twitter_qgraph_2000.Rdata"

# load the pubmed tidy object and the terms order by group after topic modeling
load(input_file1)
load(input_file2)

# Selection of the 50 most frequent words to plot the graph
nNode <- 2000
freq_word <- dplyr::top_n(dplyr::count(tidy_twitter2, word), nNode, n)
matrix_reduite <- matrix_twitter[, freq_word$word]

cor_matrix_reduite <- cor(matrix_reduite)
group1_reduit <- which(colnames(cor_matrix_reduite) %in% group[[1]]) 
group2_reduit <- which(colnames(cor_matrix_reduite) %in% group[[2]]) 
group3_reduit <- which(colnames(cor_matrix_reduite) %in% group[[3]]) 
group4_reduit <- which(colnames(cor_matrix_reduite) %in% group[[4]]) 
group_matrix_reduite <- list(group1_reduit,group2_reduit,group3_reduit,group4_reduit)

# create a qgraph object
pdf(file = output_file1, width=14, height=14)
Q <- qgraph(cor_matrix_reduite, layout = "groups", posCol = "red", negCol = "NA",
            nodeNames = colnames(cor_matrix_reduite), legend.cex = 0.2,
            vsize = 3,
            curveAll = TRUE,
            esize = 3,
            label.cex = 2,
            groups = group_matrix_reduite,
            label.scale = TRUE,
            labels = TRUE,
            minimum = 0.06, # 0.15 quand 500 noeuds
            repulsion = 1., # Augmenter la distance entre les noeuds pour améliorer la visualisation en "cluster"
            legend.mode = "style2", # groupe quand 500 noeuds
            nodeNames= TRUE,
            sampleSize = nrow(cor_matrix_reduite),
            alpha = 0.05, # Pour ne prendre en compte que les corrélations statistiquement significatifs, en tenant compte de l'inflation du risque alpha par les tests multiples via la méthode de Bonferroni.
            palette = "pastel",
            vsize = 4,
            cut = 0.3)
dev.off()

# save
save(Q, file = output_file2)
