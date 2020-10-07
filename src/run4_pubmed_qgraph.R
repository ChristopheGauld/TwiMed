#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : processing pipeline to create matrix
# date            : 2020-05-30
# version         : 3
# ==============================================================================

rm(list=ls())

input_file <- "data/pubmed_tdm_group.Rdata"
output_file <- "data/pubmed_qgraph.Rdata"

library(qgraph)

# load the pubmed tidy object and the terms order by group after topic modeling
load(input_file)

# definition de la matrice réduite à 50 noeuds
#nNode <- 50
nNode <- 500
freq_word <- dplyr::top_n(dplyr::count(tidy.pubmed2, word),nNode, n)

matrix_reduite <- matrix_pubmed[,freq_word$word]
cor_matrix_reduite <- cor(matrix_reduite)
group1_reduit <- which(colnames(cor_matrix_reduite) %in% group[[1]]) 
group2_reduit <- which(colnames(cor_matrix_reduite) %in% group[[2]]) 
group3_reduit <- which(colnames(cor_matrix_reduite) %in% group[[3]]) 
group4_reduit <- which(colnames(cor_matrix_reduite) %in% group[[4]]) 
group5_reduit <- which(colnames(cor_matrix_reduite) %in% group[[5]]) 
group6_reduit <- which(colnames(cor_matrix_reduite) %in% group[[6]]) 
group7_reduit <- which(colnames(cor_matrix_reduite) %in% group[[7]]) 
group_matrix_reduite <- list(group1_reduit,group2_reduit,group3_reduit,group4_reduit,group5_reduit,group6_reduit,group7_reduit)

# Définition de la matrice de corrélation de taille normale
#cor_matrix <- cor(matrix_pubmed)

# create a qgraph object
Q <- qgraph(cor_matrix_reduite, layout = "spring", posCol = "blue", negCol = "NA",
            nodeNames = colnames(cor_matrix_reduite), legend.cex = 0.2,
            groups = group_matrix_reduite,
            label.scale = TRUE,
            labels = TRUE,
            minimum = 0.1, # 0.15 quand 500 noeuds
            #repulsion = 100, # Augmenter la distance entre les noeuds pour améliorer la visualisation en "cluster"
            legend.mode = "style2", # groupe quand 500 noeuds
            nodeNames= TRUE,
            sampleSize = nrow(cor_matrix_reduite),
            alpha = 0.05, # Pour ne prendre en compte que les corrélations statistiquement significatifs, en tenant compte de l'inflation du risque alpha par les tests multiples via la méthode de Bonferroni.
            palette = "pastel",
            vsize = 6)

#labels=TRUE)
#color = color,
#title = titre)

# save
save(Q, file = output_file)

