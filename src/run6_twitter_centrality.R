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
library(writexl)

input_file <- "../data/twitter_qgraph_50.Rdata"
output_file1 <- "../fig/fig4a.pdf"
output_file2 <- "../data/order_fig4a.xls"
load(input_file)

# matrix ponderation 
S <- getWmat(Q, directed = FALSE)

# centrality and clustering
pdf(file = output_file1, width=14, height=14)
centralityPlot(S,
               include = c("Strength","ExpectedInfluence","Betweenness",
                          "Closeness"), theme_bw = TRUE, print = TRUE, verbose = TRUE, 
                          weighted = TRUE,signed = TRUE,
               orderBy = "Strength", decreasing = FALSE)
dev.off()

labels <- colnames(S)
strengths <- centrality_auto(Q)$node.centrality$Strength
df <- data.frame(labels, strengths)
df <- df[order(df$stre, decreasing = TRUE),]

nodes <- Q$graphAttributes$Nodes$labels
groups <- Q$graphAttributes$Graph$groups

groups_indexes<- list(groups$"A.INTEGRATION AND SOCIAL SUPPORT",
                      groups$"B.UNDERSTANDING AND MENTAL HEALTH",
                      groups$"C.CHILD WELFARE",
                      groups$"D.DAILY CHALLENGES AND DIFFICULTIES")

df[df$labels %in% nodes[groups_indexes[1][[1]]],'Group'] <- "A.INTEGRATION AND SOCIAL SUPPORT"
df[df$labels %in% nodes[groups_indexes[2][[1]]],'Group'] <- "B.UNDERSTANDING AND MENTAL HEALTH"
df[df$labels %in% nodes[groups_indexes[3][[1]]],'Group'] <- "C.CHILD WELFARE"
df[df$labels %in% nodes[groups_indexes[4][[1]]],'Group'] <- "D.DAILY CHALLENGES AND DIFFICULTIES"

write_xlsx(df, output_file2)