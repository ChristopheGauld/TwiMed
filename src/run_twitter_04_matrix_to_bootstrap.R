 #!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : processing pipeline for bootstrat Twitter data
# date            : 2020-05-12
# version         : 2
# ==============================================================================


#### Use run_twitter_02 to obtain the matrix tidy_hastag_4

# Save the 15 most frequent words
tidy_hashtag4 <- count(tidy_hashtag3,word,sort=TRUE)
words15 = tidy_hashtag4$word[1:15]

# Bootsrap des occurences et raincloud plot #####

library(boot)
# Je définis la fonction occurence qui retourne le nombre d’occurence d’un word dans le vecteur data. Je laisse la ligne d =  data[indices] qui semble être nécessaire pour sélectionner les échantillons de data avec remise.
occurence_random_tweet <- function(data, indices, word) {
  autis_tweets <- data[indices,] # allows boot to select sample
  tidy_tweeter <- autis_tweets %>%
    unnest_tokens(word, text, token="tweets", strip_url=TRUE)
  return(sum(tidy_tweeter$word == word))
}
# initialisation d'un tableau de résultat vide
results=matrix(NA,1000,15)
results=as.data.frame(results)
# Pour chacun des 15 mots, on calcule les 1000 valeurs de bootsrap et on les mets dans une colonne du tableau de résultats
for (i in 1:15) {
  result <- boot(data=autis_tweets, statistic=occurence_random_tweet, R=1000, word=words15[i])
  results[,i]=result$t # On remplit une colonne du dataframe des résultats avec les 1000 valeurs de bootsrap
  names(results)[i]=words15[i] # On donne le mot en nom de variable du dataframe
  print(names(results)[i])
}
summary(results[,1])
quantile(results[,1],c(.025,.975)) #IC bootsrap
PropCIs::exactci(281,15378,.95)$conf.int*15378 #IC méthode exacte de Clopper et Pearson (en piochant au hasard dans les 15378 hashtag et non dans tweets)

# Boostrap en tirant au sort les mots et non les tweets :
# Je définis la fonction occurence qui retourne le nombre d’occurence d’un word dans le vecteur data. Je laisse la ligne d =  data[indices] qui semble être nécessaire pour sélectionner les échantillons de data avec remise.
#occurence_random_word <- function(data, indices, word) {
#  words <- data[indices] # allows boot to select sample
#  return(sum(words == word))
#}
# initialisation d'un tableau de résultat vide
#results2=matrix(NA,1000,15)
#results2=as.data.frame(results2)
# Pour chacun des 15 mots, on calcule les 1000 valeurs de bootsrap et on les mets dans une colonne du tableau de résultats
#for (i in 1:15) {
#  result <- boot(data=tidy_hashtag3$word, statistic=occurence_random_word, R=1000, word=words15[i])
#  results2[,i]=result$t # On remplit une colonne du dataframe des résultats avec les 1000 valeurs de bootsrap
#  names(results2)[i]=words15[i] # On donne le mot en nom de variable du dataframe
#  print(i)
#}
#summary(results2[,1])
#quantile(results2[,1],c(.025,.975))
#PropCIs::exactci(281,15378,.95)$conf.int*15378


## Reformat the data for ggplot
library(dplyr)
library(tidyr)
plotData <- gather(results2,hashtag,occurency
                   ,factor_key = TRUE) #sert à avoir un plot ordonné ensuite

## And plot the data
library(ggplot2)
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
