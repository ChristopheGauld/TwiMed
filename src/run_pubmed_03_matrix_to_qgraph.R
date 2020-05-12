#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : processing pipeline for Pubmed data
# date            : 2020-05-12
# version         : 2 (Ju)
# ==============================================================================


rm(list=ls())
# Charger les packages
library(qgraph)

# Définir l'espace de travail, nommer les input et output
input_file <- "data/pubmed_matrix.Rdata"
output_file <- "data/pubmed_qgraph.Rdata"

# load the pubmed term document matrix (tdm))
load(input_file)

# Transposer la matrice (pour avoir les word en variable et les PMID en ligne)
matrix_pubmed2 <- t(matrix_pubmed)
View(matrix_pubmed2)
# Réduire la taille de la matrice (100 mots et 50 articles, choix inadequat : premier arrivé premier servi)
# pour ne pas faire planter mon ordinateur.
matrix_pubmed3 <- matrix_pubmed2[1:50,1:100]

# Enregistrer les words du réseau dans un vecteur "word"
word <- colnames(matrix_pubmed3)
# Calculer la matrice de corrélation de matrix_pubmed
cor_pubmed <- cor(matrix_pubmed3)
# Ploter le qgraph avec un layout "spring" et l'enregistrer en même temps dans l'objet "e"
e <- qgraph(cor_pubmed, 
       layout = "spring",labels=TRUE,posCol = "blue", negCol = "red",
       nodeNames=word, # pour les label de la légende
       legend = TRUE)

# save to data folder
save(e, file = output_file)








#################
################# Ancienne version 2020.05.12
#################




library(tidytext)
library(igraph)
library(qgraph)

input_file <- "../data/pubmed_tdm.Rdata"
output_file <- "../data/pubmed_qgraph.Rdata"

# load the pubmed term document matrix (tdm))
load(input_file)
# transform into dataframe describing connections
matrix_pubmed <- tidy(tdm_pubmed)
# create a igraph object
g <- graph_from_data_frame(matrix_pubmed, directed = FALSE, vertices = NULL)
# extract the adjacency matrix of the graph
r <- as_adjacency_matrix(g)
# create a qgraph object
Q <- qgraph(r)
# plot the graph
plot(Q)
# ???
qgraph(Q)
# ???
P <- qgraph(Q, minimum = 0.25, cut = 0.4, vsize = 1.5, 
            legend = FALSE, borders = FALSE, pastel = TRUE)

### Suite à enlever ou à faire fonctionner?
#
# Méthode équivalente de text mining et similaire à celle utilisée pour Twitter = non pas library(tm) mais library(tidytext)
# Créer un data_frame
tidytext <- data_frame(line = 1:nrow(tdm_pubmed), text = tdm_pubmed) #### ici!!!! -> tdm_pubmed ou autre ???? 
#Analyse de fréquence 
tidytext <- tidytext %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) 

ggplot(tidytext[1:25,], aes(reorder(word, n), n)) + 
  geom_bar(stat = "identity", fill = "#E3693E") +
  geom_text(aes(label= as.character(n)), check_overlap = TRUE, size = 4) + 
  coord_flip() + 
  xlab(" ") + 
  ylab("Volume") + 
  labs(title = "25 mots les plus récurrents avec le package tidytext",
       caption = capt) + 
  tkrtheme                     

          
#####
### NOTES SUPPLEMENTAIRES
####
                         
                         # Visualisation de la matrice dans ggplot : déf du thème 
tkrtheme <- theme(plot.title=element_text(margin=margin(0,0,20,0), size=20, hjust = 0.5),
        plot.subtitle = element_text(margin=margin(0,0,20,0), size = 15, hjust = 0.5),
        panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(colour = "grey"), 
        plot.margin = margin(20,50,20,50)) 
                         
# Plot lui même               
ggplot(matrix_pubmed[1:25,], aes(reorder(name, `1`), `1`)) +        #### que remplacer à la place de '1' ????
  geom_bar(stat = "identity", fill = "#E3693E") +
  geom_text(aes(label= as.character(`1`)), check_overlap = TRUE, size = 4) + 
  coord_flip() + 
  xlab(" ") + 
  ylab("Volume") + 
  labs(title = "First 25 words most frequents",
       caption = capt) + 
  tkrtheme

                         
# Methode 1 (trop long pour 1 computer) 
library(qgraph)
# après avoir fait nécessairement as.data.frame.matrix
e <- qgraph(cor(tdm_pubmed))
