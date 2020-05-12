#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : processing pipeline for centralities with qgraph object
# date            : 2020-05-12
# version         : 2
# ==============================================================================


library(qgraph)
# matrix ponderation 
S <- getWmat(g,directed = FALSE)
# Measures of centrality
deg <- centrality(g)$OutDegree
bet <- centrality(r)$Betweenness
clo <- centrality(r)$Closeness
# centrality and clustering
centralityPlot(S, labels = Labels, scale = c("z-scores", "raw", "raw0","relative"),
               include =c("Degree","Strength","OutDegree","InDegree","OutStrength",
                          "InStrength"), theme_bw = TRUE, print = TRUE, verbose = TRUE,
               standardized, relative, weighted = TRUE,signed = TRUE,
               orderBy = "default", decreasing = FALSE)
clusteringPlot(S, scale = c("z-scores", "raw", "raw0","relative"), labels , signed = FALSE, theme_bw = TRUE, print = TRUE,verbose = TRUE)








#############
############# Non vérifié pour le moment car matrice trop importante (bug)
#############

#############
############# Centrality in the graph
#############

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


# Graph with centrality = fonction de color = clo.col = color is closeness
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



# Graph with centrality = fonction de color = bet.col = color is betweenness
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


# Graph with centrality = fonction de color = deg.col = color is strength
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




#Centrality Plot
centralityPlot(e, include = c("Strength", "Betweenness","Closeness"),orderBy ="Strength")

clo <- centrality(e)$Closeness


################### Mesures de centralité
################### Sur 100 termes 
################### Sur igraph mesures 

transitivity(b, type = "average") ### coefficient de clustering
Degree <- degree(g)

Betweenness <- betweenness(g)
b <- data.frame(centralitiesOK$...1, centralitiesOK$Betweenness) ### j'ai exporté en faisant cc de View Betweenness en csv et réimporté ensuite pour convertir en data.frame
b <- graph_from_data_frame(b, directed = FALSE, vertices = NULL)


Closeness <- closeness(g)# pas pour disconnected ? 
c <- data.frame(Closenness)
c <- graph_from_data_frame(c, directed = FALSE, vertices = NULL)


# centrality comparaison      (need to do that after with PubMed)
centralities <- cbind(Degree, Betweenness, Closeness)
head(centralities)
write.csv(centralities, file="centralities.csv")
round(cor(centralities), 2)


# Centrality ok en plot     https://rpubs.com/pjmurphy/313180      vax.herokuapp.com
lay <- layout_with_fr(b)
plot(b, layout = lay, 
     vertex.label = NA)
plot.igraph(b, layout=lay)


### Degree for data
lay <- layout_with_fr(g)
gplo <- plot.igraph(g, layout=lay,
                    vertex.color="#42B540FF",
                    vertex.size=2.5,
                    vertex.label.cex=0.4,
                    vertex.color="#AD00DAFF",
                    curved=TRUE,
                    vertex.label.dist=0.5,
                    vertex.size=degree(g), 
                    main="Betweenness")



### Betweenness for data
j <- plot.igraph(b, layout=lay,
                 vertex.color="#42B540FF",
                 vertex.size=2.5,
                 vertex.label.cex=0.4,
                 vertex.color="#AD00DAFF",
                 curved=TRUE,
                 vertex.label.dist=0.5,
                 vertex.size=degree(b), 
                 main="Betweenness")



### DegreeCloseness for data
k <- plot.igraph(c, layout=lay,
                 vertex.size=2.5,
                 vertex.color="#ED000099",
                 vertex.label.color="#925E9FFF",
                 edge.color= "#FDAF9199",
                 vertex.label.cex=0.4,
                 curved=TRUE,
                 vertex.label.dist=0.5,
                 vertex.size=degree(g), 
                 main="Closeness")


# Others Layouts
laygraphop <- layout_with_graphopt(g)
laygem <- layout_with_gem(g)
laylgl <- layout_with_lgl(g)



