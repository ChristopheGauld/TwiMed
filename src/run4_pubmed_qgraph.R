#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : processing pipeline to create matrix
# date            : 2021-01-19
# version         : 4 (Guillaume Dumas)
# ==============================================================================

rm(list=ls())

input_file1 <- "../data/pubmed_tdm.Rdata"
input_file2 <- "../data/pubmed_tdm_group.Rdata"
output_file1 <- "../fig/pubmed_graph_50.pdf"
output_file2 <- "../data/pubmed_qgraph_50.Rdata"

library(tidyverse)
library(qgraph)
library(NbClust)
library(dplyr)
library(tidyr)
library(ggplot2)

# load the pubmed tidy object and the terms order by group after topic modeling
load(input_file1)
load(input_file2)

# definition of the reduced matrix with 50 noeuds
nNode <- 50
freq_word <- dplyr::top_n(dplyr::count(tidy.pubmed2, word), nNode, n)
matrix_reduite <- matrix_pubmed[, freq_word$word]
save(matrix_reduite, file="../data/pubmed_matrix_reduite.Rdata")
cor_matrix_reduite <- cor(matrix_reduite)

# choose the best numbers of Clusters
resp<-NbClust(cor_matrix_reduite, distance = "euclidean", min.nc=2, max.nc=8, 
             method = "complete", index = "ch")
resp$All.index
resp$Best.nc
resp$Best.partition

# determine the number of groups
group1_reduit <- which(colnames(cor_matrix_reduite) %in% group[[1]]) 
group2_reduit <- which(colnames(cor_matrix_reduite) %in% group[[2]]) 
group3_reduit <- which(colnames(cor_matrix_reduite) %in% group[[3]]) 
group4_reduit <- which(colnames(cor_matrix_reduite) %in% group[[4]]) 
group_matrix_reduite <- list(group1_reduit,group2_reduit,group3_reduit,group4_reduit)


# eventually define the reduced correlation matrix 
# cor_matrix <- cor(matrix_pubmed)

# set the type names
types_names <- str_to_upper(c("A.Diagnosis and Skills",
                              "B.Research Challenges",
                              "C.Clinical and Therapeutical Challenges",
                              "D.Neuropsychology and Behaviors"))
names(group_matrix_reduite) <- types_names

# create a qgraph object
pdf(file = output_file1)
Q <- qgraph(cor_matrix_reduite, layout = "groups", posCol = "black", negCol = "NA",
            vsize = 3,
            label.cex = 3,
            label.prop = 1,
            shape = "circle",
            curveAll = TRUE,
            label.norm = "OOOOOOOOOO",
            nodeNames = colnames(cor_matrix_reduite), legend.cex = 0.2,
            groups = group_matrix_reduite,
            label.scale = TRUE,
            esize = 3,
            minimum = 0.1,
            repulsion = 1.,
            labels = colnames(cor_matrix_reduite),
            label.scale= FALSE,
            legend.mode = "groups",
            nodeNames= TRUE,
            legend.cex = 3,
            sampleSize = nrow(cor_matrix_reduite),
            layoutOffset = c(-0.24,0),
            alpha = 0.05, # inflation of the alpha risk (Bonferroni method)
            palette = "pastel",
            vsize = 4,
            cut = 0.3)
dev.off()

# save
save(Q, file = output_file2)


rm(list=ls())

input_file1 <- "../data/pubmed_tdm.Rdata"
input_file2 <- "../data/pubmed_tdm_group.Rdata"
output_file1 <- "../fig/pubmed_graph_500.pdf"
output_file2 <- "../data/pubmed_qgraph_500.Rdata"

# load the pubmed tidy object and the terms order by group after topic modeling
load(input_file1)
load(input_file2)

# definition of the reduced matrix with 500 noeuds
nNode <- 500
freq_word <- dplyr::top_n(dplyr::count(tidy.pubmed2, word), nNode, n)
matrix_reduite <- matrix_pubmed[, freq_word$word]

# determine the number of groups
cor_matrix_reduite <- cor(matrix_reduite)
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
