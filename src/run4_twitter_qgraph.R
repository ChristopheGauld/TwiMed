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

library(tidyverse)
library(qgraph)
library(NbClust)
library(dplyr)
library(tidyr)
library(ggplot2)

# load the pubmed tidy object and the terms order by group after topic modeling
load(input_file1)
load(input_file2)

# definition of the reduced matrix with 50 nodes
nNode <- 50
freq_word <- dplyr::top_n(dplyr::count(tidy_twitter2, word), nNode, n)
matrix_reduite <- matrix_twitter[, freq_word$word]
cor_matrix_reduite <- cor(matrix_reduite)

# choose the best numbers of clusters
rest<-NbClust(cor_matrix_reduite, distance = "euclidean", min.nc=2, max.nc=8, 
             method = "complete", index = "ch")
rest$All.index
rest$Best.nc
rest$Best.partition

# determine the number of groups
group1_reduit <- which(colnames(cor_matrix_reduite) %in% group[[1]]) 
group2_reduit <- which(colnames(cor_matrix_reduite) %in% group[[2]]) 
group3_reduit <- which(colnames(cor_matrix_reduite) %in% group[[3]]) 
group4_reduit <- which(colnames(cor_matrix_reduite) %in% group[[4]]) 
group_matrix_reduite <- list(group1_reduit,group2_reduit,group3_reduit,group4_reduit)

# determine the names of the clusters
types_names <- str_to_upper(c("A.Integration and Social Support",
                              "B.Understanding and Mental Health",
                              "C.Child Welfare",
                              "D.Daily Challenges and Difficulties"))
names(group_matrix_reduite) <- types_names

# create a qgraph object
pdf(file = output_file1)
Q <- qgraph(cor_matrix_reduite, layout = "groups", posCol = "black", negCol = "NA",
            nodeNames = colnames(cor_matrix_reduite), legend.cex = 0.2,
            groups = group_matrix_reduite,
            label.scale = TRUE,
            label.norm = "OOOOOOOOOO",
            curveAll = TRUE,
            vsize = 3,
            label.cex = 3,
            label.prop = 1,
            shape = "circle",
            labels = colnames(cor_matrix_reduite),
            minimum = 0.08, 
            repulsion = 1., 
            legend.mode = "groups",
            nodeNames = TRUE,
            sampleSize = nrow(cor_matrix_reduite),
            alpha = 0.05, #inflation du risque alpha par les tests multiples via la méthode de Bonferroni.
            palette = "pastel",
            cut = 0.3)
dev.off()

# save
save(Q, file = output_file2)


rm(list=ls())

input_file1 <- "../data/twitter_tdm.Rdata"
input_file2 <- "../data/twitter_tdm_group.Rdata"
output_file1 <- "../fig/twitter_graph_500.pdf"
output_file2 <- "../data/twitter_qgraph_500.Rdata"

# load the pubmed tidy object and the terms order by group after topic modeling
load(input_file1)
load(input_file2)

# definition of the reduced matrix with 500 nodes
nNode <- 500
freq_word <- dplyr::top_n(dplyr::count(tidy_twitter2, word), nNode, n)
matrix_reduite <- matrix_twitter[, freq_word$word]
cor_matrix_reduite <- cor(matrix_reduite)


# determine the number of groups
group1_reduit <- which(colnames(cor_matrix_reduite) %in% group[[1]]) 
group2_reduit <- which(colnames(cor_matrix_reduite) %in% group[[2]]) 
group3_reduit <- which(colnames(cor_matrix_reduite) %in% group[[3]]) 
group4_reduit <- which(colnames(cor_matrix_reduite) %in% group[[4]]) 
group_matrix_reduite <- list(group1_reduit,group2_reduit,group3_reduit,group4_reduit)

# create a qgraph object
pdf(file = output_file1, width=50, height=50)
Q <- qgraph(cor_matrix_reduite, layout = "spring", posCol = "black", negCol = "NA",
            nodeNames = colnames(cor_matrix_reduite), legend.cex = 0.2,
            groups = group_matrix_reduite,
            vsize = 1,
            curveAll = FALSE,
            esize = 1.,
            label.cex = 1,
            label.scale = TRUE,
            label.fill.horizontal = 0.6,
            labels = colnames(cor_matrix_reduite),
            borders = FALSE,
            minimum = 0.15, # 0.15 quand 500 noeuds
            repulsion = 1, # Augmenter la distance entre les noeuds pour améliorer la visualisation en "cluster"
            legend.mode = "groups",
            nodeNames= TRUE,
            sampleSize = nrow(cor_matrix_reduite),
            alpha = 0.05, # Pour ne prendre en compte que les corrélations statistiquement significatifs, en tenant compte de l'inflation du risque alpha par les tests multiples via la méthode de Bonferroni.
            palette = "pastel",
            cut = 0.3)
dev.off()

# save
save(Q, file = output_file2)
