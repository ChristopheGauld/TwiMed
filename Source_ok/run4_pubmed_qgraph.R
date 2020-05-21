rm(list=ls())

setwd("~/Documents/Amis/Christophe/Twimed")
input_file <- "data/pubmed_tdm_group.Rdata"
output_file <- "data/pubmed_qgraph.Rdata"

library(qgraph)

# load the pubmed tidy object and the terms order by group after topic modeling
load(input_file)

# Pour réduire la taille de la matrice à 300*300 au lieu de 13000*13000 pour pouvoir plotter en prenant les 300 mots les plus fréquents
# Exécuter prog1 d'abord pour avoir l'objet tidy.pubmed2
#nbMots <- 300
#freq_word <- dplyr::top_n(dplyr::count(tidy.pubmed2, word),nbMots, n)

#matrix_reduite <- matrix_pubmed[,freq_word$word]
#cor_matrix_reduite <- cor(matrix_reduite)
#group1_reduit <- which(colnames(cor_matrix_reduite) %in% group[[1]]) 
#group2_reduit <- which(colnames(cor_matrix_reduite) %in% group[[2]]) 
#group3_reduit <- which(colnames(cor_matrix_reduite) %in% group[[3]]) 
#group4_reduit <- which(colnames(cor_matrix_reduite) %in% group[[4]]) 
#group_matrix_reduite <- list(group1_reduit,group2_reduit,group3_reduit,group4_reduit)

# Définition de la matrice de corrélation de taille normale
cor_matrix <- cor(matrix_pubmed)

# create a qgraph object
Q <- qgraph(cor_matrix, layout = "spring", posCol = "blue", negCol = "red",
           nodeNames = colnames(cor_matrix), legend.cex = 0.2,
           groups = group,
           #minimum = .15, # Ne pas afficher les edges avec faible corrélation pour faciliter le chargement graphique
           #repulsion = 100, # Augmenter la distance entre les noeuds pour améliorer la visualisation en "cluster"
           legend.mode = "groups",
           threshold = "bonferroni",sampleSize = nrow(cor_matrix_reduite), alpha = 0.05) # Pour ne prendre en compte que les corrélations statistiquement significatifs, en tenant compte de l'inflation du risque alpha par les tests multiples via la méthode de Bonferroni.
           #pastel = TRUE,
           #labels=TRUE)
           #vsize = taille,
           #color = color,
           #title = titre)

# save
save(Q, file = output_file)

