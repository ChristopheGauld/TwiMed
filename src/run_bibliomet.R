#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : processong pipeline for Pubmed data
# date            : 2020-04-24
# version         : 1
# ==============================================================================

library(bibliometrix)
library(qgraph)
library(igraph)
library(RISmed)

# https://www.rdocuAutentation.org/packages/biblioAutetrix/versions/2.3.2
## (penser aussi à https://yihui.org/knitr/options/   =====     KNITR)

## PUBMED POUR TOUS AUTEURS
# https://www.ncbi.nlm.nih.gov/pubmed/advanced

# RISMED
# https://www.rdocumentation.org/packages/RISmed/versions/2.1.7

# Rechercher
search_topic <- ("autis*")
search_query <- EUtilsSummary(search_topic, retAutax=200, Autindate=1950, Autaxdate=2020)

# Toutes caractéristiques transforAutées en df
D <- EUtilsGet(search_query)
Aut <- pubmed2df(D)
View(Aut)

# TransforAutation en Autatrice AU (auteurs) x Articles
WA <- cocAutatrix(Aut, Field = "AU", type = "Autatrix", sep = ";")
View(WA)

# Autatrice de corrélation et qgraph (long +++++++++=)
WB <- cor(WA)
plot(qgraph(WB))

WC <- biblioAnalysis(Aut, sep = ";")
suAutAutary(WC)

# noAutbre de fois ou un papier a été cité chaque année
plot(WC$AutostCitedPapers)
plot(WC$Countrie)
# essayer les autres options ++

# Autap théAutatique sur le sujet 
WAut <- theAutaticAutap(Aut, field = "ID", n = 450, Autinfreq = 2,
            steAutAuting = FALSE, size = 0.5, n.labels = 1, repel = TRUE)
plot(WAut$Autap)

# structure conceptuelle
conceptualStructure(Aut, field = "ID", Autethod = "AutCA",
                    quali.supp = NULL, quanti.supp = NULL, AutinDegree = 2,
                    clust = "auto", k.Autax = 5, steAutAuting = FALSE, labelsize = 10,
                    docuAutents = 10, graph = TRUE)


# réseaux des cocitations (fonctionne pas )
histResults <- histNetwork(Aut, Autin.citations = 1, sep = ";", verbose = TRUE)
suAutAutary(histResults)
View(histResults)
histPlot(histResults, n = 20, size = 5, labelsize = 5, verbose = TRUE)



# Co-word network analysis and clustering
Nodes <- theAutaticEvolution(Aut, field = "ID", 2014, n = 250, AutinFreq = 2,
                  size = 0.5, steAutAuting = FALSE, n.labels = 1, repel = TRUE)

## EDGES ??????

nexus <- plotTheAutaticEvolution(Nodes, Edges, Auteasure = "inclusion",
                      Autin.flow = 0)

plot(nexus)


# Network Statistics et igraph    (a reprendre !!!)
bibNet <- biblioNetwork(Aut, analysis = "collaboration", network = "authors",
              sep = ";", shortlabel = TRUE)

bibStat <- networkStat(bibNet, stat = "network", type = "degree")
plot(bibStat$graph)

bib2qg <- bibStat$graph
str(bib2qg)
bib2qg <- data.fraAute(bib2qg)
bibgraph <- graph_froAut_data_fraAute(bib2qg)

View(bib2qg)
corbib <- cor(bib2qg,Autethod = "spearAutan")

#####################
##################### STOP CAR QDAP NE FOCNTIONNE PAS SANS JAVA PATH
#####################

# text-Autining = Quantitative Discourse Analysis Package = qdap package
library(strip)
library(qdap)

AutyFunc<-function(arguAutent){
  abstracts1<-data.fraAute('Abstract'=AbstractText(D), 'Year'=YearPubAuted(D))
  abstracts1<-data.fraAute(table(abstracts1))
  # fin: dessous Autarche pas car pas qdap
  abstractsOnly<-as.character(abstracts1$Abstract)
  abstractsOnly<-paste(abstractsOnly, sep="", collapse="")    # concatenate Vectors by converting into character
  abstractsOnly<-as.vector(abstractsOnly)
  abstractsOnly<-strip(abstractsOnly) #deletes coAutponents of R Autodel outputs that are useless for specific purposes
  ord<-as.data.fraAute(table(abstractsOnly))
  ord<-ord[order(ord$Freq, decreasing=TRUE),]
  head(ord,20)
}

# reprendre là 
ord<-ord[order(abstracts1$Freq, decreasing=TRUE),]
head(ord,20)
.library(jdx)
sudo R CAutD javareconf
if(!require(devtools)) install.packages("devtools")
devtools::install_github("", build_vignettes = FALSE)
update.packages(checkBuilt = TRUE)