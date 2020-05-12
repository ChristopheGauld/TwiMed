  
#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : processing pipeline for qgraph from Twitter matrix
# date            : 2020-05-12
# version         : 2
# ==============================================================================


# Use fo draw the Twitter graph 


library(qgraph)
g <- Idiese[2:101]
Names <- names(Idiese)[2:101]
Names
e <- qgraph(cor(g))
deg <- centrality(e)$OutDegree
bet <- centrality(e)$Betweenness
clo <- centrality(e)$Closeness
