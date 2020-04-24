#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : Twitter centrality
# date            : 2020-04-24
# version         : 1
# ==============================================================================



library(tidyr)
library(dplyr)
library(plyr)
library(ggplot2)
library(stringr)
library(tidytext)
library(igraph)
library(tidyverse)
library(plyr)
library(qgraph)
library(wesanderson)




################### Mesures de centralité
################### Sur 100 termes 
################### Sur igraph mesures 

transitivity(b, type = "average") ### coefficient de clustering
Degree <- degree(g)

Betweenness <- betweenness(g)
b <- data.frame(centralitiesOK$...1, centralitiesOK$Betweenness) ### j'ai exporté en faisant cc de View Betweenness en csv et réimporté ensuite pour convertir en data.frame
b <- graph_from_data_frame(b, directed = FALSE, vertices = NULL)


Closeness <- closeness(g)# pas pour disconnected ? 
c <- data.frame(Closenness)
c <- graph_from_data_frame(c, directed = FALSE, vertices = NULL)


# centrality comparaison      (need to do that after with PubMed)
centralities <- cbind(Degree, Betweenness, Closeness)
head(centralities)
write.csv(centralities, file="centralities.csv")
round(cor(centralities), 2)


# Centrality ok en plot     https://rpubs.com/pjmurphy/313180      vax.herokuapp.com
lay <- layout_with_fr(b)
plot(b, layout = lay, 
     vertex.label = NA)
plot.igraph(b, layout=lay)


### Degree for data
lay <- layout_with_fr(g)
gplo <- plot.igraph(g, layout=lay,
                    vertex.color="#42B540FF",
                    vertex.size=2.5,
                    vertex.label.cex=0.4,
                    vertex.color="#AD00DAFF",
                    curved=TRUE,
                    vertex.label.dist=0.5,
                    vertex.size=degree(g), 
                    main="Betweenness")



### Betweenness for data
j <- plot.igraph(b, layout=lay,
                 vertex.color="#42B540FF",
                 vertex.size=2.5,
                 vertex.label.cex=0.4,
                 vertex.color="#AD00DAFF",
                 curved=TRUE,
                 vertex.label.dist=0.5,
                 vertex.size=degree(b), 
                 main="Betweenness")



### DegreeCloseness for data
k <- plot.igraph(c, layout=lay,
                 vertex.size=2.5,
                 vertex.color="#ED000099",
                 vertex.label.color="#925E9FFF",
                 edge.color= "#FDAF9199",
                 vertex.label.cex=0.4,
                 curved=TRUE,
                 vertex.label.dist=0.5,
                 vertex.size=degree(g), 
                 main="Closeness")


# Others Layouts
laygraphop <- layout_with_graphopt(g)
laygem <- layout_with_gem(g)
laylgl <- layout_with_lgl(g)













