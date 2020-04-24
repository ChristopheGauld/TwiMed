#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : processing pipeline for Twitter data (old TwiMed_OK.R)
# date            : 2020-04-24
# version         : 1
# ==============================================================================

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
Idiese = I
Idiese[,2:101]=0
nTweetDiese=NA
compteur=1
split = str_split(Idiese[,1]," ")
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

# Graph
library(qgraph)
g=Idiese[2:101]
Names = names(Idiese)[2:101]
Names
e <- qgraph(cor(g))
deg = centrality(e)$OutDegree
bet = centrality(e)$Betweenness
clo = centrality(e)$Closeness



# Taille = fréquence
# Couleur = degré
heat.colors(20)
library(wesanderson)
wes_palette("heat.colors", n=20, type = "continuous")
wes_palette("GrandBudapest1", n=20, type = "continuous")
wes_palette("Zissou1", 20, type = "continuous")


summary(deg*6)
deg.col=NA
for (i  in 1:4){
  iDeg = deg[i]
  deg.col[i] = wes_palette("Zissou1", n=20, type = "continuous")[iDeg*6]
}

summary(ceiling(bet/75)+1)
bet.col=NA
compteur = 1
for (i in bet){
  bet.col[compteur] = wes_palette("Zissou1", n=20, type = "continuous")[ceiling(i/75)+1]
  compteur = compteur+1
}

summary(clo*57000)
clo.col=NA
compteur = 1
for (i in clo){
  clo.col[compteur] = wes_palette("Zissou1", n=20, type = "continuous")[i*57000]
  compteur = compteur+1
}


par(mfrow=c(1,1))
es <- qgraph(e, layout = "spring",
             borders = TRUE,
             posCol = "blue", negCol = "red",
             labels = TRUE,
             curveScale = FALSE,
             nodeNames = Names,
             #groups=group,
             legend=TRUE,
             legend.cex = .07,
             minimum=.035,
             vsize = nTweetDiese[1:100]/150,
             color = clo.col
)

ec <- qgraph(e, layout = "spring",
             borders = TRUE,
             posCol = "blue", negCol = "red",
             labels = TRUE,
             curveScale = FALSE,
             legend.cex = .07,
             nodeNames = Names,
             #groups=group,
             legend=TRUE,
             minimum=.035,
             vsize = deg*2,
             color = bet.col
)

eb <- qgraph(e, layout = "spring",
             borders = TRUE,
             posCol = "blue", negCol = "red",
             labels = TRUE,
             curveScale = TRUE,
             legend.cex = .07,
             nodeNames = Names,
             #groups=group,
             legend=TRUE,
             minimum=.035,
             vsize = (bet)/200,
             color = deg.col
)




#Centraliry Plot

centralityPlot(e, include = c("Strength", "Betweenness","Closeness"),orderBy ="Strength")


# Bootnet pour stabilité du réseau 
library(bootnet)
BootTwit <- bootnet(e, statistics = "Strength", nBoots = 1000, nCores = 1)




########## Predictibility of each node

library(mgm)
set.seed(1)
p <- ncol(g)
dim(g)

# estimer un modèle de variables continues
predi <- mgm(g, 
               type = rep('g', p),
               level = rep(1, p),   # nombre de catégories pour chaque var = 1
               lambdaSel = 'CV',
               ruleReg = 'OR', 
               pbar = FALSE)

pred_Twit <- predict(predi, 
                    data = g, 
                    errorCon = 'R2')


epred <- qgraph(pred_Twit$pairwise$wadj, # utiliser la matrice de corrélation
         layout = 'spring', 
         pie = pred_Twit$error[,2], # provide errors as input
         pieColor = rep('#377EB8',p),
         edge.color = predi$pairwise$edgecolor,
         labels = colnames(g),
         borders = TRUE,
         posCol = "blue", negCol = "red",
         labels = TRUE,
         curveScale = FALSE,
         nodeNames = Names,
         legend=TRUE,
         minimum=.035,
         vsize = nTweetDiese[1:100]/150,
         color = clo.col)





##### MARCHE PAS !!!!!

library(topicmodels)
library(tm)
# https://www.tidytextmining.com/topicmodeling.html
# https://cran.r-project.org/web/packages/topicmodels/topicmodels.pdf
# les datas doivent être un documenTermMatrix fait sous tm
data(crude)
tdm <- TermDocumentMatrix(e,
                          control = list(removePunctuation = TRUE,
                                         stopwords = TRUE))

ap_lda <- LDA(I, k = 2, control = list(seed = 1234))