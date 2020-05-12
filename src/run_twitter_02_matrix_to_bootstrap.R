#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : processing pipeline for bootstrat Twitter data
# date            : 2020-05-12
# version         : 2 (Ju)
# ==============================================================================



# Réinitialiser l'espace de travail
rm(list=ls())
# Charger les packages
library(dplyr)
library(stringr)
library(tidytext)
date()
# Définir l'espace de travail, nommer les input et output
setwd("~/...Twimed")
input_file <- "data/twitter_Tue May 12 15:31:03 2020.RData"
output_file <- "results/raincloudplot_hashtag.pdf"

# load the twitter dataframe
load(input_file)

# put an id for each tweet
autis$id = rownames(autis)
autis_tweets = autis[,c("text","id")]

# split tweets and remove url
tidy_tweeter <- autis_tweets %>%
  unnest_tokens(word, text, token="tweets", strip_url=TRUE)

# keep only hashtag
tidy_hashtag <- tidy_tweeter[str_detect(tidy_tweeter$word,"#"),]
# remove autis* and covid19 words
tidy_hashtag2 <- tidy_hashtag[!str_detect(tidy_hashtag$word,"autis"),]
tidy_hashtag3 <- tidy_hashtag2[tidy_hashtag2$word != "#asd",]
tidy_hashtag3 <- tidy_hashtag3[tidy_hashtag3$word != "#covid19",]

# Save the 15 most frequent words
tidy_hashtag4 <- count(tidy_hashtag3,word,sort=TRUE)
words15 = tidy_hashtag4$word[1:15]




















###########
########### Ancienne méthode = QUI FONCTIONNE ÉGALEMENT
###########





#### Use for create a binary matrix with the get_data from Twitter


load("../data/twitter.RData")

autis$stripped_text <- gsub("http.*","",  autis$text)
autis$stripped_text <- gsub("https.*","", autis$stripped_text)
autis$stripped_text <- gsub("amp","", autis$stripped_text)
autis$stripped_text <- gsub("auti*","", autis$stripped_text)

# data frame de tweet en caractère
I=as.data.frame(autis$stripped_text)
names(I)[1]="tweet"
I$tweet=as.character(I$tweet)
library(dplyr)
library(stringr)
I$tweet = I$tweet %>% str_to_lower() # Enlever les majuscules

# liste de mots triés : 20206 mots uniques
library(tidytext)
words <- autis %>%
  select(stripped_text) %>%
  unnest_tokens(word, stripped_text)
data("stop_words")
stop_words = rbind (stop_words,"autism","amp","sm")
words <- words %>% 
  anti_join(stop_words)
words$word = words$word %>%
  str_to_lower()
unique.words = words %>%
  count(word, sort = TRUE)
rm.word = function(word,data){
  data=data[-which(str_detect(data$word,word)),]
  return(data)}
unique.words=rm.word("covid",unique.words)
unique.words=rm.word("sm",unique.words)
unique.words=rm.word("autism*",unique.words)
unique.words=rm.word("awaren*",unique.words)
unique.words=rm.word("1",unique.words)
unique.words=rm.word("2",unique.words)
unique.words=rm.word("3",unique.words)
unique.words=rm.word("4",unique.words)


# On rempli le data.frame avec les #
Idiese <- I
Idiese[,2:101 <- 0
nTweetDiese <- NA
compteur <- 1
split <- str_split(Idiese[,1]," ")
for (j in unique.words$word[1:100]){
  compteur = compteur + 1
  for (i in 1:dim(Idiese)[1]){
    if( j %in% split[[i]] | paste("#",j,sep = "") %in% split[[i]]){ Idiese[i,compteur] = 1
    }
  }
  names(Idiese)[compteur]=j
  nTweetDiese[compteur]=sum(Idiese[,compteur])
  print(compteur)
}
write.table(Idiese,"tweet.binaire.1000.#",sep="\t")

# apriori les hashtag n'ont pas été comptés dans le dataframe.
nTweetDiese = nTweetDiese[-1]
plot(nTweetDiese,unique.words$n[1:100],ylim = c(0,2200),xlim = c(0,1600))
lines(x = c(0,2000),y=c(0,2000))

