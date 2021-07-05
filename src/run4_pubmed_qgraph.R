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

# load the pubmed tidy object and the terms order by group after topic modeling
load(input_file1)
load(input_file2)

# definition de la matrice réduite à 50 noeuds
nNode <- 50
freq_word <- dplyr::top_n(dplyr::count(tidy.pubmed2, word), nNode, n)
matrix_reduite <- matrix_pubmed[, freq_word$word]
save(matrix_reduite, file="../data/pubmed_matrix_reduite.Rdata")
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


types_names <- str_to_upper(c("A.Diagnosis and Skills",
                              "B.Research Challenges",
                              "C.Clinical and Therapeutical Challenges",
                              "D.Neuropsychology and Behaviors"))
names(group_matrix_reduite) <- types_names


# Définition de la matrice de corrélation de taille normale
# cor_matrix <- cor(matrix_pubmed)


label_nodes <- c("Communication",
                 "Developmental",
                 "Diagnosis",
                 "Diagnostic",
                 "Evidence",
                 "Individuals",
                 "Mental",
                 "Parents",
                 "Research",
                 "Skills",
                 "Subjects",
                 "Syndrome",
                 "ADHD",
                 "Adults",
                 "Controls",
                 "Genetic",
                 "Human",
                 "Intervention",
                 "Mice",
                 "Neurodevelopmental",
                 "Social",
                 "Symptoms",
                 "Time",
                 "Clinical",
                 "Deficits",
                 "Development",
                 "Disease",
                 "Expression",
                 "Factors",
                 "Function",
                 "Functioning",
                 "Gene",
                 "Language",
                 "Levels",
                 "Model",
                 "Population",
                 "Processing",
                 "Risk",
                 "Treatment",
                 "Association",
                 "Attention",
                 "Behavior",
                 "Brain",
                 "Child",
                 "Cognitive",
                 "Differences",
                 "Functional",
                 "Genes",
                 "Health",
                 "Role")

label_nodes_alphab <- sort(label_nodes, decreasing = FALSE)



# create a qgraph object
pdf(file = output_file1)
Q <- qgraph(cor_matrix_reduite, layout = "groups", posCol = "black", negCol = "NA",
            vsize = 3,
            label.cex = 5,
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
            labels = label_nodes_alphab,
            label.scale= FALSE,
            legend.mode = "groups", # groupe quand 500 noeuds
            nodeNames= label_nodes,
            legend.cex = 3,
            sampleSize = nrow(cor_matrix_reduite),
            layoutOffset = c(-0.24,0),
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
output_file1 <- "../fig/pubmed_graph_500.pdf"
output_file2 <- "../data/pubmed_qgraph_500.Rdata"

library(qgraph)

# load the pubmed tidy object and the terms order by group after topic modeling
load(input_file1)
load(input_file2)

# definition de la matrice réduite à 50 noeuds
nNode <- 500
freq_word <- dplyr::top_n(dplyr::count(tidy.pubmed2, word), nNode, n)
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
pdf(file = output_file1, width=50, height=50)
Q <- qgraph(cor_matrix_reduite, layout = "spring", posCol = "red", negCol = "NA",
            nodeNames = colnames(cor_matrix_reduite), legend.cex = 0.2,
            groups = group_matrix_reduite,
            vsize = 1,
            curveAll = FALSE,
            esize = 1.,
            label.cex = 2,
            label.scale = TRUE,
            label.fill.horizontal = 0.7,
            labels = TRUE,
            minimum = 0.08, # 0.15 quand 500 noeuds
            repulsion = 1., # Augmenter la distance entre les noeuds pour améliorer la visualisation en "cluster"
            legend.mode = "style2", # groupe quand 500 noeuds
            nodeNames= TRUE,
            sampleSize = nrow(cor_matrix_reduite),
            alpha = 0.05, # Pour ne prendre en compte que les corrélations statistiquement significatifs, en tenant compte de l'inflation du risque alpha par les tests multiples via la méthode de Bonferroni.
            palette = "pastel",
            cut = 0.5)
dev.off()

# save
save(Q, file = output_file2)
