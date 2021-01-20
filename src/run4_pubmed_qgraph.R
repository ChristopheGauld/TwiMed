#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : processing pipeline to create matrix
# date            : 2020-05-30
# version         : 3
# ==============================================================================

rm(list=ls())

input_file1 <- "../data/pubmed_tdm.Rdata"
input_file2 <- "../data/pubmed_tdm_group.Rdata"
output_file1 <- "../fig/pubmed_graph_50.pdf"
output_file2 <- "../data/pubmed_qgraph_50.Rdata"

library(qgraph)
library(NbClust)

# load the pubmed tidy object and the terms order by group after topic modeling
load(input_file1)
load(input_file2)

# definition de la matrice réduite à 50 noeuds
nNode <- 50
freq_word <- dplyr::top_n(dplyr::count(tidy.pubmed3, word), nNode, n)
matrix_reduite <- matrix_pubmed[, freq_word$word]
cor_matrix_reduite <- cor(matrix_reduite)

# Best numbers of Clusters
resp<-NbClust(cor_matrix_reduite, distance = "euclidean", min.nc=2, max.nc=8, 
             method = "complete", index = "ch")
resp$All.index
resp$Best.nc
resp$Best.partition

# Groups
group1_reduit <- which(colnames(cor_matrix_reduite) %in% group[[1]]) 
group2_reduit <- which(colnames(cor_matrix_reduite) %in% group[[2]]) 
group3_reduit <- which(colnames(cor_matrix_reduite) %in% group[[3]]) 
group4_reduit <- which(colnames(cor_matrix_reduite) %in% group[[4]]) 
group_matrix_reduite <- list(group1_reduit,group2_reduit,group3_reduit,group4_reduit)

# Définition de la matrice de corrélation de taille normale
# cor_matrix <- cor(matrix_pubmed)

# create a qgraph object
pdf(file = output_file1)
Q <- qgraph(cor_matrix_reduite, layout = "groups", posCol = "blue", negCol = "NA",
            vsize = 3,
            label.cex = 2,
            curveAll = TRUE,
            nodeNames = colnames(cor_matrix_reduite), legend.cex = 0.2,
            groups = group_matrix_reduite,
            label.scale = TRUE,
            labels = TRUE,
            esize = 3,
            minimum = 0.2,
            repulsion = 1., 
            legend.mode = "style2", # groupe quand 500 noeuds
            nodeNames= TRUE,
            sampleSize = nrow(cor_matrix_reduite),
            alpha = 0.05, # inflation du risque alpha par les tests multiples via la méthode de Bonferroni.
            palette = "pastel",
            vsize = 4,
            cut = 0.3)
dev.off()

# save
save(Q, file = output_file2)

rm(list=ls())

input_file1 <- "../data/pubmed_tdm.Rdata"
input_file2 <- "../data/pubmed_tdm_group.Rdata"
output_file1 <- "../fig/pubmed_graph_2000.pdf"
output_file2 <- "../data/pubmed_qgraph_2000.Rdata"

library(qgraph)

# load the pubmed tidy object and the terms order by group after topic modeling
load(input_file1)
load(input_file2)

# definition de la matrice réduite à 50 noeuds
nNode <- 2000
freq_word <- dplyr::top_n(dplyr::count(tidy.pubmed3, word), nNode, n)
matrix_reduite <- matrix_pubmed[, freq_word$word]

cor_matrix_reduite <- cor(matrix_reduite)
group1_reduit <- which(colnames(cor_matrix_reduite) %in% group[[1]]) 
group2_reduit <- which(colnames(cor_matrix_reduite) %in% group[[2]]) 
group3_reduit <- which(colnames(cor_matrix_reduite) %in% group[[3]]) 
group4_reduit <- which(colnames(cor_matrix_reduite) %in% group[[4]]) 
group_matrix_reduite <- list(group1_reduit,group2_reduit,group3_reduit,group4_reduit)

# Définition de la matrice de corrélation de taille normale
# cor_matrix <- cor(matrix_pubmed)

# create a qgraph object
pdf(file = output_file1)
Q <- qgraph(cor_matrix_reduite, layout = "spring", posCol = "red", negCol = "NA",
            nodeNames = colnames(cor_matrix_reduite), legend.cex = 0.1,
            groups = group_matrix_reduite,
            label.scale = TRUE,
            labels = FALSE,
            minimum = 0.08, # 0.15 quand 500 noeuds
            repulsion = 1., # Augmenter la distance entre les noeuds pour améliorer la visualisation en "cluster"
            legend.mode = "style2", # groupe quand 500 noeuds
            nodeNames= TRUE,
            sampleSize = nrow(cor_matrix_reduite),
            alpha = 0.05, # Pour ne prendre en compte que les corrélations statistiquement significatifs, en tenant compte de l'inflation du risque alpha par les tests multiples via la méthode de Bonferroni.
            palette = "pastel",
            vsize = 0.5,
            cut = 0.5)
dev.off()

# save
save(Q, file = output_file2)
