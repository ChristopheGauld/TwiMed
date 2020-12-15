#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : processing pipeline for bootstrat PubMed data
# date            : 2020-05-30
# version         : 3
# ==============================================================================

library(boot)
library(dplyr)
library(tidyr)
library(ggplot2)
library(boot)
source("load_function_violin.R")

#### Use run_pubmed to obtain the matrix_pubmed
input_file <- "../data/pubmed_tdm.Rdata"
output_file <- "../fig/fig1b.pdf"
load(input_file)

# Save the 15 most frequent words (repeat)
tidy.pubmed3 <- count(tidy.pubmed2,word,sort=TRUE)
words_pubmed15 = tidy.pubmed3$word[1:15]

# Bootsrap des occurences et raincloud plot #####
# Je définis la fonction occurence qui retourne le nombre d’occurence d’un word dans le vecteur data. Je laisse la ligne d =  data[indices] qui semble être nécessaire pour sélectionner les échantillons de data avec remise.
occurence_random_words <- function(data, indices, word) {
  ab.pubmed <- data[indices,] # allows boot to select sample
  tidy_pub <- ab.pubmed %>%
    unnest_tokens(word, AB)
  return(sum(tidy_pub$word == word))
}
# initialisation d'un tableau de résultat vide
results=matrix(NA,1000,15)
results=as.data.frame(results)
# Pour chacun des 15 mots, on calcule les 1000 valeurs de bootsrap et on les mets dans une colonne du tableau de résultats
for (i in 1:15) {
  result <- boot(data=ab.pubmed, statistic=occurence_random_words, R=1000, word=words_pubmed15[i])
  results[,i]=result$t # On remplit une colonne du dataframe des résultats avec les 1000 valeurs de bootsrap
  names(results)[i]=words_pubmed15[i] # On donne le mot en nom de variable du dataframe
  print(names(results)[i])
}

## Reformat the data for ggplot
plotData <- gather(results,words,occurency
                   ,factor_key = TRUE) #sert à avoir un plot ordonné ensuite

## And plot the data
source("/geom_flat_violin.R") # cf run_twitter_03b
p <- ggplot(plotData,aes(x=plotData[,1],y=plotData[,2], 
                          fill = plotData[,1], colour = plotData[,1]),trim = TRUE)+
  geom_flat_violin(position = position_nudge(x = .3, y = 0), adjust = 1)+
  geom_point(position = position_jitter(width = .2,height = 0), size = .3) +
  guides(fill=FALSE, colour = FALSE)+
  ylab("ylab") +
  xlab("xlab") +
  coord_flip() 

# plot in a pdf file
pdf(file = output_file)
p
dev.off()
