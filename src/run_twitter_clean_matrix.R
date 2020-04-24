#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : Clean and Matrix from the pure Twitter data
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


############### New datas with QGRAPH
load("~/Desktop/Attente/TwiMed/....RData")

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
stop_words = rbind (stop_words,"autism","amp")
words <- words %>% 
  anti_join(stop_words)
words$word = words$word %>%
  str_to_lower()
unique.words = words %>%
  count(word, sort = TRUE)


# On rempli le data.frame avec les #
Idiese = I
Idiese[,2:1001]=0
nTweetDiese=NA
compteur=1
split = str_split(Idiese[,1]," ")
for (j in unique.words$word[1:1000]){
  compteur = compteur + 1
  for (i in 1:dim(Idiese)[1]){
    if( j %in% split[[i]] | paste("#",j,sep = "") %in% split[[i]]){ Idiese[i,compteur] = 1
    }
  }
  names(Idiese)[compteur]=j
  nTweetDiese[compteur]=sum(Idiese[,compteur])
  print(compteur)
}


autis$stripped_text <- gsub("http.*","",  autis$text)
autis$stripped_text <- gsub("https.*","", autis$stripped_text)
autis$stripped_text <- gsub("autis","", autis$stripped_text)
autism_clea <- autis %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

View(autism_clea)


# View(autism_clea)
data("stop_words")
sx <- tibble(word = "autism")
sw <- tibble(word = "amp")
sw <- tibble(word = "awareness")

nrow(autism_clea)
#213767
cleaned_autism <- autism_clea %>%
  anti_join(stop_words) %>% anti_join(sx)
nrow(autism_clea)
#112943
cleaned_tweet_words <- autism_clea %>%  anti_join(sx)


#### Matrix
autism_paired_words <- autis %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(paired_words, stripped_text, token = "ngrams", n = 2)

autism_paired_words %>%
  dplyr::count(paired_words, sort = TRUE)

autism_separated_words <- autism_paired_words %>%
  tidyr::separate(paired_words, c("word1", "word2"), sep = " ")

autism_filtered <- autism_separated_words %>%
  dplyr::filter(!word1 %in% stop_words$word)

autism_words_counts <- autism_filtered %>%
  dplyr::count(word1, word2, sort = TRUE)

View(autism_words_counts)
autii <- tibble(autism_words_counts)
cor(autii)
