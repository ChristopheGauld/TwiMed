#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : processing pipeline for Pubmed data
# date            : 2020-05-08
# version         : 1
# ==============================================================================




##### https://amunategui.github.io/pubmed-query/

library(RISmed)
search_topic <- 'autis*'
search_query <- EUtilsSummary(search_topic, retmax=100, mindate=2020, maxdate=2020)
View(search_query)

records<- EUtilsGet(search_query)

pubmed_data <- data.frame('Title'=ArticleTitle(records),'Abstract'=AbstractText(records))

pubmed_data$Abstract <- as.character(pubmed_data$Abstract)
pubmed_data$Abstract <- gsub(",", " ", pubmed_data$Abstract, fixed = TRUE)
str(pubmed_data)



### Text mining   = similaire à run_twitter_clean_matrix.R


library(tidyr)
library(dplyr)
library(tidytext)
library(dplyr)
library(rtweet)

### Strip
pubmed_data$stripped_text <- gsub("http.*","",  pubmed_data)
words <- pubmed_data %>%
  select(stripped_text) %>%
  unnest_tokens(word, stripped_text)


## stop words
data("stop_words")
stop_words = rbind (stop_words,"autism","amp", "covid")
words <- words %>% 
  anti_join(stop_words)

############# ........continuer comme dans run_twitter_clean_matrix.R si on veut les mêmes analyses, le bootstrap, etc. 



####### et pour le graph : inspiré de run_twitter_network_igraph.R
autism_pubmed_clean <- pubmed_data %>%
  dplyr::select(stripped_text) %>%     unnest_tokens(word, stripped_text)


autism_tweets_paired_words <- autism_tweets_clean %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(paired_words, stripped_text, token = "ngrams", n = 2)

autism_tweets_paired_words <- autism_pubmed_clean %>%
  unnest_tokens(paired_words, token = "ngrams", n = 2)


















###### ANCIENNE VERSION (avril 2020)

update.packages(checkBuilt = TRUE)
library(qgraph)
library(igraph)
library(bibliometrix)
library(strip)
library(qdap)

load("../data/pubmed.Rdata")

# Transformation en matrice AU (auteurs) x Articles
WA <- cocMatrix(results_pubmed, Field = "AU", type = "matrix", sep = ";")
View(WA)

# matrice de corrélation et qgraph (long +++++++++)
WB <- cor(WA)
qgraph(WB)

WC <- biblioAnalysis(results_pubmed, sep = ";")
summary(WC)

# nombre de fois ou un papier a été cité chaque année
plot(WC$MostCitedPapers)
plot(WC$Countrie)
# essayer les mres options ++

# map thématique sur le sujet 
Wm <- thematicMap(results_pubmed, field = "ID", n = 450, minfreq = 2,
                  stemming = FALSE, size = 0.5, n.labels = 1, repel = TRUE)
plot(Wm$map)

# structure conceptuelle
conceptualStructure(results_pubmed, field = "ID", method = "mCA",
                    quali.supp = NULL, quanti.supp = NULL, minDegree = 2,
                    clust = "mo", k.max = 5, stemming = FALSE, labelsize = 10,
                    documents = 10, graph = TRUE)

# réseaux des cocitations (ne fonctionne pas)
histResults <- histNetwork(results_pubmed, min.citations = 1, sep = ";", verbose = TRUE)
summary(histResults)
View(histResults)
histPlot(histResults, n = 20, size = 5, labelsize = 5, verbose = TRUE)

# Co-word network analysis and clustering
Nodes <- thematicEvolution(results_pubmed, field = "ID", 2014, n = 250, minFreq = 2,
                  size = 0.5, stemming = FALSE, n.labels = 1, repel = TRUE)

## EDGES ??????
nexus <- plotThematicEvolution(Nodes, Edges, measure = "inclusion", min.flow = 0)
plot(nexus)


# Network Statistics et igraph    (a reprendre !!!)
bibNet <- biblioNetwork(results_pubmed, analysis = "collaboration", network = "mhors", sep = ";", shortlabel = TRUE)

bibStat <- networkStat(bibNet, stat = "network", type = "degree")
plot(bibStat$graph)

bib2qg <- bibStat$graph
str(bib2qg)
bib2qg <- data.frame(bib2qg)
bibgraph <- graph_from_data_frame(bib2qg)

View(bib2qg)
corbib <- cor(bib2qg, method = "spearman")

#####################
##################### STOP CAR QDAP NE FONCTIONNE PAS SANS JAVA PATH
#####################

# text-mining = Quantitative Discourse Analysis Package = qdap package


myFunc<-function(argument){
  abstracts1<-data.frame('Abstract'=AbstractText(D), 'Year'=YearPubmed(D))
  abstracts1<-data.frame(table(abstracts1))
  # fin: dessous marche pas car pas qdap
  abstractsOnly<-as.character(abstracts1$Abstract)
  abstractsOnly<-paste(abstractsOnly, sep="", collapse="")    # concatenate Vectors by converting into character
  abstractsOnly<-as.vector(abstractsOnly)
  abstractsOnly<-strip(abstractsOnly) #deletes components of R model outputs that are useless for specific purposes
  ord<-as.data.frame(table(abstractsOnly))
  ord<-ord[order(ord$Freq, decreasing=TRUE),]
  head(ord,20)
}

# reprendre là 
ord<-ord[order(abstracts1$Freq, decreasing=TRUE),]
head(ord,20)
