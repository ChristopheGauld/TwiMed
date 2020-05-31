#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : preprocess Pubmed data into a Term Document Matrix
# date            : 2020-05-12
# version         : 1
# ==============================================================================





run_twitter_02_matrix_OLD

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






















library(tm)

input_file <- "../data/pubmed.Rdata"
output_file <- "../data/pubmed_matrix.Rdata"

# load the pubmed data
load(input_file)
# convert to a corpus
datatxt.corpus <- Corpus(DataframeSource(data.frame(doc_id=results_pubmed$PMID, text=results_pubmed$AB)))
# remove punctuations
datatxt.corpus <- tm_map(datatxt.corpus, removePunctuation)
# remove white space
datatxt.corpus <- tm_map(datatxt.corpus, stripWhitespace)
# lowercase
datatxt.corpus <- tm_map(datatxt.corpus, tolower)
# remove common english word, terms used and human brain mapping
datatxt.corpus <- tm_map(datatxt.corpus,function(x) removeWords(x, c(stopwords("english"))))
# create corpus table
tdm_pubmed <- TermDocumentMatrix(datatxt.corpus)
# convert to a matrix
matrix_pubmed <- as.matrix(tdm_pubmed)
# save to data folder
save(matrix_pubmed, file = output_file)
