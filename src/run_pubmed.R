#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : processing pipeline for Pubmed data
# date            : 2020-04-24
# version         : 1
# ==============================================================================

library(qgraph)
library(igraph)
library(RISmed)
library(bibliometrix)

load("../data/pubmed.Rdata")

# Transformation en matrice AU (auteurs) x Articles
WA <- cocmatrix(m, Field = "AU", type = "matrix", sep = ";")
View(WA)

# matrice de corrélation et qgraph (long +++++++++=)
WB <- cor(WA)
plot(qgraph(WB))

WC <- biblioAnalysis(m, sep = ";")
summary(WC)

# nombre de fois ou un papier a été cité chaque année
plot(WC$mostCitedPapers)
plot(WC$Countrie)
# essayer les mres options ++

# map thématique sur le sujet 
Wm <- thematicmap(m, field = "ID", n = 450, minfreq = 2,
            stemming = FALSE, size = 0.5, n.labels = 1, repel = TRUE)
plot(Wm$map)

# structure conceptuelle
conceptualStructure(m, field = "ID", method = "mCA",
                    quali.supp = NULL, quanti.supp = NULL, minDegree = 2,
                    clust = "mo", k.max = 5, stemming = FALSE, labelsize = 10,
                    documents = 10, graph = TRUE)


# réseaux des cocitations (fonctionne pas )
histResults <- histNetwork(m, min.citations = 1, sep = ";", verbose = TRUE)
summary(histResults)
View(histResults)
histPlot(histResults, n = 20, size = 5, labelsize = 5, verbose = TRUE)



# Co-word network analysis and clustering
Nodes <- thematicEvolution(m, field = "ID", 2014, n = 250, minFreq = 2,
                  size = 0.5, stemming = FALSE, n.labels = 1, repel = TRUE)

## EDGES ??????

nexus <- plotThematicEvolution(Nodes, Edges, measure = "inclusion",
                      min.flow = 0)

plot(nexus)


# Network Statistics et igraph    (a reprendre !!!)
bibNet <- biblioNetwork(m, analysis = "collaboration", network = "mhors",
              sep = ";", shortlabel = TRUE)

bibStat <- networkStat(bibNet, stat = "network", type = "degree")
plot(bibStat$graph)

bib2qg <- bibStat$graph
str(bib2qg)
bib2qg <- data.frame(bib2qg)
bibgraph <- graph_from_data_frame(bib2qg)

View(bib2qg)
corbib <- cor(bib2qg,method = "spearman")

#####################
##################### STOP CAR QDAP NE FOCNTIONNE PAS SANS JAVA PATH
#####################

# text-mining = Quantitative Discourse Analysis Package = qdap package
library(strip)
library(qdap)

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
.library(jdx)
sudo R CmD javareconf
if(!require(devtools)) install.packages("devtools")
devtools::install_github("", build_vignettes = FALSE)
update.packages(checkBuilt = TRUE)