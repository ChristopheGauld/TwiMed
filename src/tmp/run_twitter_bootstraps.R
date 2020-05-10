#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : bootstraps analyses on the Twitter data
# date            : 2020-04-25
# version         : 1
# ==============================================================================

autism <- read.csv("../data/autismclean", sep="")

library(boot)
View(autism_clea)
# Nombre de mot
length(unique(autism_clea$word))

# boot() not function with a data frame with facteur : je converti le dataframe en vecteur de chaîne de caractères
autism_clea = as.character(autism_clea$word)

# Cleaning common words
library(tidytext)
data("stop_words")
stop_words[1150,1]="autism"
stop_words[1150,2]="onix"
autism.clea = autism_clea[!(autism_clea %in% stop_words$word)] #102732 éléments


### Calcul du nombre d'occurence, plus rapide
# Initialisation vecteur des mots uniques
list.word = unique(autism.clea) #19 972
# Initialisation vecteur du nombre d'occurence par mot unique
n.word = rep(0,length(list.word))
# Initialisation compteur du nombre de tour de la boucle
ntour=0
# Pour chaque case de la base totale, j'augmente le compteur du mot correspondant de 1 (plus rapide)
for (i in autism.clea)
{n.word[which(list.word == i)] = n.word[which(list.word == i)] + 1
ntour = ntour+1 ; print(ntour)}


#On met le vecteur du nombre d'occurence dans un data frame avec la liste des mots uniques :
summary(n.word)
data.word = data.frame(list.word, n.word)
names(data.word)=c("word","n")

# On sélectionne les 50 mots les plus fréquents
# Calcul du seuil d'occurence correspondant aux 50 mots les plus fréquents

seuil.occurence = quantile(data.word$n,1-(50/dim(data.word)[1])) # 188 : On va donc garder tous les mots apparaissant plus de 188 fois
data.50 = data.word[data.word$n >= seuil.occurence ,] # 50 observations, ça a marché
data.50$word=as.character(data.50$word)
View(data.50)

# On enregistre data.50 pour gagner du temps si le bootsrap bug après
write.table(data.50, "data.50" , sep="\t")

# bootstrapping with 1000 replications



# Je définis la fonction occurence qui retourne le nombre d’occurence d’un word dans le vecteur data. Je laisse la ligne d =  data[indices] qui semble être nécessaire pour sélectionner les échantillons de data avec remise.
occurence <- function(data, indices, word) {
  d <- data[indices] # allows boot to select sample
  return(sum(d == word))
}
# initialisation d'un tableau de résultat vide
results=matrix(NA,1000,50)
results=as.data.frame(results)

# Pour chacun des 50 mots, on calcule les 1000 valeurs de bootsrap et on les mets dans une colonne du tableau de résultats
for (i in 1:50) {
  result <- boot(data=autism.clea, statistic=occurence, R=1000, word=data.50$word[i])
  results[,i]=result$t # On remplit une colonne du dataframe des résultats avec les 1000 valeurs de bootsrap
  names(results)[i]=data.50$word[i] # On donne le mot en nom de variable du dataframe
  print(i)
}

library(prettyR)

describe(results)
data.50$mean = apply(results,2,mean)
data.50$median = apply(results,2,median)
for (i in 1:50) data.50$ic.025[i] = quantile(results[,i],.025) #IC
for (i in 1:50) data.50$ic.975[i] = quantile(results[,i],.975) #IC

write.table(results, "results" ,sep="\t")
write.table(data.50, "data.50" , sep="\t")
write.csv(data.50, "results")
View(results)
