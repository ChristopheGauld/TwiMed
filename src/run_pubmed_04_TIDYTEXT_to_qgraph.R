#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : processing pipeline for Pubmed data
# date            : 2020-05-12
# version         : 2 (Ju)
# ==============================================================================



### Deuxième version : utiliser directement le TDM du programme numéro 2, en le convertissant simplement 
# au format matrice standard de R avec as.matrix(), après

# Réinitialiser l'espace de travail
rm(list=ls())
# Charger les packages
library(tidytext)
library(igraph)
library(qgraph)

# Définir l'espace de travail, nommer les input et output
input_file <- "data/pubmed.Rdata" # Le premier dataframe du prg 1
output_file <- "data/pubmed_qgraph.Rdata"

# load the pubmed dataframe
load(input_file)

# Sélectionner les variables d'intérêt (les keywords et les PMID)
keyword.pubmed <- results_pubmed[,c("PMID","DE")]

# Nettoyage du texte avec tidytext et organisation en objet tidy (à la place du programme 2) ####
# On sépare les mots clé de chaque article (qui sont séparés par un point virgule dans results_pubmed).
tidy.pubmed <- unnest_tokens(keyword.pubmed, keyword, DE ,token = stringr::str_split, pattern = ";")
# Compter le nombre d'occurence de chaque mot clé par article (par définition normalement une seule fois)
tidy.pubmed3 <- dplyr::count(tidy.pubmed, PMID, keyword)


# Faire un qgraph en créant d'abord un igraph à l'aide d'un objet tidy, puis une matrice adjacente (économise 
# énormément de taille ++++ puis un objet qgraph)
# create a igraph object
g <- graph_from_data_frame(tidy.pubmed3, directed = FALSE, vertices = NULL)
# extract the adjacency matrix of the graph
r <- as_adjacency_matrix(g)
# create a qgraph object
Q <- qgraph(r,layout="spring")

# save
save(Q, file = output_file)










##############
############## MEME QUE PRÉCÉDENT MAIS AVEC ABSTRACT AU LIEU DE MESH
##############


### Deuxième version : utiliser directement le TDM du programme numéro 2, en le convertissant simplement 
# au format matrice standard de R avec as.matrix(), après

# Réinitialiser l'espace de travail
rm(list=ls())
# Charger les packages
library(tidytext)
library(igraph)
library(qgraph)

# Définir l'espace de travail, nommer les input et output
input_file <- "data/pubmed.Rdata" # Le premier dataframe du prg 1
output_file <- "data/pubmed_qgraph.Rdata"

# load the pubmed dataframe
load(input_file)

# Sélectionner les variables d'intérêt (les abstracts et les PMID)
ab.pubmed = results_pubmed[,c("PMID","AB")]
rownames(ab.pubmed) = ab.pubmed$PMID

# Nettoyage du texte avec tidytext et organisation en objet tidy (à la place du programme 2) ####
tidy.pubmed = unnest_tokens(ab.pubmed,word, AB)
# Enlever les mots courants
data("stop_words")
tidy.pubmed2 <- dplyr::anti_join(tidy.pubmed,stop_words)
# Compter le nombre d'occurence de chaque mot par abstract
tidy.pubmed3 <- dplyr::count(tidy.pubmed2, PMID, word) # 112 137 association mots-tweet


# Faire un qgraph en créant d'abord un igraph à l'aide d'un objet tidy, puis une matrice adjacente (économise 
# énormément de taille ++++ puis un objet qgraph)
# create a igraph object
g <- graph_from_data_frame(tidy.pubmed3, directed = FALSE, vertices = NULL)
# extract the adjacency matrix of the graph
r <- as_adjacency_matrix(g)
# create a qgraph object
Q <- qgraph(r)

# save
save(Q, file = output_file)





















############
########### ANCIEN
############







load(results_pubmed)


# TEXT MINING
library(tidytext)
words <- results_pubmed %>%
  select(stripped_text) %>%
  unnest_tokens(word, stripped_text)
data("stop_words")
stop_words = rbind (stop_words,"autism","amp")
words <- words %>% 
  anti_join(stop_words)
words$word = words$word %>%
  str_to_lower()
unique.words = words %>%
  count(word, sort = TRUE)


###########
########### QGRAPH depuis results_pubmed (voir getdata)
###########


g <- graph_from_data_frame(unique.words, directed = FALSE, vertices = NULL)
r <- as_adjacency_matrix(g) ## si on veut 
Q <- qgraph(r)



###########
########### Matrice de 0 et de 1  (1 colonne = 1 mot   / 1 ligne = 1 article)  
###########

# fonction de Idiese (voir fichier = run_clean_matrix_twitter) (!!!!!!! TITRE ICI)
pubm <- results_pubmed$TI
View(pubm)
J=as.data.frame(pubm)
View(J)

# nettoyage des mots
results_pubmed$stripped_text <- gsub("http.*","",  results_pubmed$TI)
results_pubmed$stripped_text <- gsub("https.*","", results_pubmed$stripped_text)
results_pubmed$stripped_text <- gsub("amp","", results_pubmed$stripped_text)
results_pubmed$stripped_text <- gsub("auti*","", results_pubmed$stripped_text)




# Matrice O et 1
library(stringr)
library(tidytext)
Jdiese = J
Jdiese[,2:101]=0
nTweetDiese=NA
compteur=1
split = str_split(Jdiese[,1]," ")
for (j in unique.words$word[1:100]){
  compteur = compteur + 1
  for (i in 1:dim(Jdiese)[1]){
    if( j %in% split[[i]] | paste("#",j,sep = "") %in% split[[i]]){ Jdiese[i,compteur] = 1
    }
  }
  names(Jdiese)[compteur]=j
  nTweetDiese[compteur]=sum(Jdiese[,compteur])
  print(compteur)
}
View(Jdiese)
write.table(Jdiese,"tweet.binaire.1000.#",sep="\t")






