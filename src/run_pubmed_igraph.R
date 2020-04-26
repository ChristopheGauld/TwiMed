#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : Pubmed (old TwiMed.R)
# date            : 2020-04-24
# version         : 1
# ==============================================================================



#### PUBMED 
library(qgraph)
library("lavaan")

# Datas from an "export" from PubMed
d  <- DatasOKNOTTS

# convert character
summary(d)
d$out_degree = as.numeric(as.character(d$out_degree))
d$in_degree = as.numeric(as.character(d$in_degree))
d$betweeness = as.numeric(as.character(d$betweeness))
View(d)

# Convert dataframe
gh <- na.omit(d)
View(gh)
gt <- gh %>% select(3, 4, 5, 8)
View(gt)

# graph1
gt %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "PubMed - #autism", subtitle = "Text mining PubMed data ", x = "", y = "")


# convert to igraph
p <- graph_from_data_frame(gt, directed = FALSE, vertices = NULL)
plot(p)
o <- as_adjacency_matrix(p,edges = TRUE, names = TRUE) ## si on veut 
plot(o)


### coefficient de clustering
transitivity(p, type = "average") 
transitivity(o, type = "average") 
ml <- cor_auto(t)


