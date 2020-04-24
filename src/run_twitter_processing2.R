#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : processing pipeline (old TwiMed.R)
# date            : 2020-04-24
# version         : 1
# ==============================================================================

#recherche

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


############### New with QGRAPH
library(wesanderson)

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


# Non utile mais laissé
cleaned_tweet_words %>%
  count(word, sort = TRUE) %>%
  top_n(50) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in tweets",
       subtitle = "Stop words removed from the list")


####Network1 OK à faire

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


# code de Ju pour faire le réseau de base résumant la féquence des mots en fonction des couleurs et des tailles
# s'y ajoute le vertew.color avec 'b' qui vient de la mesure de betweenness réalisé dessous

seuil = 20
g=  autism_words_counts %>%
  filter(n >= seuil) %>%
  graph_from_data_frame()
plot(g)
lay <- layout_with_fr(g)
gplo <- plot.igraph(g, layout=lay,
                    vertex.color=vertexCol,
                    vertex.size=autism_words_counts$n[1:sum(autism_words_counts$n>=seuil)]/60,
                    vertex.label.cex=0.5,
                    vertex.color="#AD00DAFF",
                    curved=TRUE,
                    vertex.label.dist=0.5,
                    #vertex.size=degree(g), 
                    edge.arrow.width=.1,
                    edge.arrow.size=.1,
                    main="Connections")


colors = b # cf les mesures de centralité ci dessous pour comprendre
vertexCol= colors

# en fait le vertex.size il fait
# autism_words_counts$n
# autism_words_counts$n[1:33]
# vertex.size = c(261,218,199,74,,,)


################### Mesures de centralité     sur 100 termes 

# sur igraph mesures 
transitivity(b, type = "average") ### coefficient de clustering
Degree <- degree(g)

Betweenness <- betweenness(g)
b <- data.frame(centralitiesOK$...1, centralitiesOK$Betweenness) ### j'ai exporté en faisant cc de View Betweenness en csv et réimporté ensuite pour convertir en data.frame
b <- graph_from_data_frame(b, directed = FALSE, vertices = NULL)


Closeness <- closeness(g)# pas pour disconnected ? 
c <- data.frame(Closenness)
c <- graph_from_data_frame(c, directed = FALSE, vertices = NULL)


### degree.cent <- centr_degree(g, mode = "all")
#degree.cent$res
#degree(g)

#f<-degree(g, v = V(g), 
mode = c("all", "out", "in", "total"), 
loops = TRUE, normalized = FALSE)
#plot(f)


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



j <- plot.igraph(b, layout=lay,
                 vertex.color="#42B540FF",
                 vertex.size=2.5,
                 vertex.label.cex=0.4,
                 vertex.color="#AD00DAFF",
                 curved=TRUE,
                 vertex.label.dist=0.5,
                 vertex.size=degree(b), 
                 main="Betweenness")

pdf(file = "/Users/Christophe/Desktop/degree.pdf", width = 15,   height = 15)

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


laygraphop <- layout_with_graphopt(g)

closene <- plot.igraph(g, layout=laygraphop,
                       vertex.size=2.5,
                       vertex.color="#ADB6B699",
                       vertex.label.color="#1B1919FF",
                       edge.color= "#AD00DAFF",
                       vertex.label.cex=0.4,
                       curved=TRUE,
                       vertex.label.dist=0.5,
                       vertex.size=degree(g), 
                       main="Closeness (X 1000)")


laygem <- layout_with_gem(g)

between <- plot.igraph(g, layout=laygem,
                       vertex.size=2.5,
                       vertex.color="#42B540FF",
                       vertex.label.color="#0099B4FF",
                       edge.color= "#FDAF91FF",
                       vertex.label.cex=0.4,
                       curved=TRUE,
                       vertex.label.dist=0.5,
                       vertex.size=degree(g), 
                       main="Betweenness")


laylgl <- layout_with_lgl(g)

hu <- plot.igraph(g, layout=laylgl,
                  vertex.size=2.5,
                  vertex.color="#42B540FF",
                  vertex.label.color="#AD00DAFF",
                  edge.color= "#FDAF91FF",
                  vertex.label.cex=0.4,
                  curved=TRUE,
                  vertex.label.dist=0.5,
                  vertex.size=degree(g), 
                  main="Hubs")


# layout graphopt lgl
# colors : AD00DAFF    0099B4FF      ADB6B6FF
# https://nanx.me/ggsci/reference/pal_lancet.html pour couleur Lancet
# https://f.hypotheses.org/wp-content/blogs.dir/2996/files/2017/02/visualiseR.pdf 


# mon code qui marche pas 
  r <- graph_from_data_frame(autism_words_counts, filter(n >= 24) )
  plot(r)
  
  lay <- layout_with_fr(r)
  graphi <- plot.igraph(r, layout=lay,
                      vertex.color="#42B540FF",
                      vertex.size=2.5,
                      vertex.label.cex=0.4,
                      vertex.color="#AD00DAFF",
                      curved=TRUE,
                      vertex.label.dist=0.5,
                      main="Connection")

  
#  vertex.size=autism_words_counts$n[1:33]/10,   car 33 mots
  
  
#### Network ok 2


autis$stripped_text <- gsub("http.*","",  autis$text)
autis$stripped_text <- gsub("https.*","", autis$stripped_text)

autism_tweets <- autis

library(dplyr)
autism_tweets_clean <- autism_tweets %>%
    dplyr::select(stripped_text) %>%     unnest_tokens(word, stripped_text)

library(dplyr)
autism_tweets_paired_words <- autism_tweets_clean %>%
       dplyr::select(stripped_text) %>%
        unnest_tokens(paired_words, stripped_text, token = "ngrams", n = 2)

autism_tweets_paired_words %>%     count(paired_words, sort = TRUE)

#enlever rows
autism_tweets_separated_words <- autism_tweets_paired_words %>%
     separate(paired_words, c("word1", "word2"), sep = " ")

autism_tweets_filtered <- autism_tweets_separated_words %>%
       filter(!word1 %in% stop_words$word) %>%
       filter(!word2 %in% stop_words$word)

# new bigram counts: (blue points)
autism_words_counts <- autism_tweets_filtered %>%
       count(word1, word2, sort = TRUE)
autism_words_counts %>%
       filter(n >= 2) %>%
       graph_from_data_frame() %>%
       ggraph(layout = "fr") +
       geom_node_point(color = "darkslategray4", size = 3) +
       geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
       labs(title = "Word Network: Tweets using the hashtag - #autism", subtitle = "Text mining twitter data ", x = "", y = "")


#best network avec Guillaume ++++++

# données en igraph
g <- graph_from_data_frame(autism_words_counts, directed = FALSE, vertices = NULL)
plot(g) # long
View(g)


library("scales")


# sur qgraph
library(qgraph)
r <- as_adjacency_matrix(g) ## si on veut 
View(r)

Q <- qgraph(r)
plot(Q)
qgraph(Q, layout=lay)
P <- qgraph(Q, minimum = 0.25, cut = 0.4, vsize = 1.5, 
            legend = FALSE, borders = FALSE, pastel = TRUE)



S <- getWmat(Q,directed = FALSE)
centralityPlot(S, labels = Labels, scale = c("z-scores", "raw", "raw0","relative"),
               include =c("Degree","Strength","OutDegree","InDegree","OutStrength",
                          "InStrength"), theme_bw = TRUE, print = TRUE, verbose = TRUE,
               standardized, relative, weighted = TRUE,signed = TRUE,
               orderBy = "default", decreasing = FALSE)
  


clusteringPlot(S, scale = c("z-scores", "raw", "raw0","relative"), labels , signed = FALSE, theme_bw = TRUE, print = TRUE,verbose = TRUE)
centrality(Q)


### ancien essai non fructueux

bigram_graph <- autism_words_counts %>%
       filter(n > 3) %>%
       graph_from_data_frame()

graphou <- ggraph(bigram_graph, layout = "fr") +
       geom_edge_link() +
       geom_node_point() +
       geom_node_text(aes(label = name), vjust = 1, hjust = 1)

plot(graphou)
View(bigram_graph)


## utiliser le ggraph pour mesure de centralité ?    https://www.data-imaginist.com/2017/introducing-tidygraph/
library(tidygraph)
iris_tree <- as_tbl_graph(graphou$data)
iris_tree %>% activate(edges)
as_tibble(iris_tree)

r <- as_adjacency_matrix(bigram_graph)
plot(r)

r <- as_adjacency_matrix(bigram_graph, type = c("both", "upper", "lower"),
                    attr = NULL, edges = FALSE, names = TRUE,
                    sparse = igraph_opt("sparsematrices"))

plot(r)


a <- ggraph(graphou, layout = 'linear') + 
  geom_edge_arc(aes(colour = red))


create_notable('bull') %>%
  activate(nodes) %>%
  mutate(importance = centrality_alpha())


#### autre technique depuis graphou   https://www.rpubs.com/Steven_Surya/tidygraph-ggraph


graphou$layout_ggraph %>%
  ggraph(layout = "gem") +
  geom_node_point() +
  geom_edge_diagonal() 


## NON
ggraph(bigram_graph, layout = "fr") +
       geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                                          arrow = a, end_cap = circle(.07, 'inches')) +
       geom_node_point(color = "lightblue", size = 5) +
       geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
       theme_void()



# or schema dark blue non oriented

autism_tweets$stripped_text <- gsub("http.*","",  autism_tweets$text)
autism_tweets$stripped_text <- gsub("https.*","", autism_tweets$stripped_text)

autism_tweets_clean <- autism_tweets %>%
       dplyr::select(stripped_text) %>%
       unnest_tokens(word, stripped_text)

autism_tweets_paired_words <- autism_tweets %>%
       dplyr::select(stripped_text) %>%
       unnest_tokens(paired_words, stripped_text, token = "ngrams", n = 2)

autism_tweets_paired_words %>%
       count(paired_words, sort = TRUE)

autism_tweets_separated_words <- autism_tweets_paired_words %>%
       separate(paired_words, c("word1", "word2"), sep = " ")

autism_tweets_filtered <- autism_tweets_separated_words %>%
       filter(!word1 %in% stop_words$word) %>%
       filter(!word2 %in% stop_words$word)

autism_words_counts <- autism_tweets_filtered %>%
       count(word1, word2, sort = TRUE)

autism_words_counts %>%
       filter(n >= 24) %>%
       graph_from_data_frame() %>%
       ggraph(layout = "fr") +
       # geom_edge_link(aes(edge_alpha = n, edge_width = n))
       # geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
       geom_node_point(color = "darkslategray4", size = 3) +
       geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
       labs(title = "Word Network: Tweets using the hashtag - #autism",
          subtitle = "Text mining twitter data ",
          x = "", y = "")


#schema dark oriented
bigram_graph <- autism_words_counts %>%
       filter(n > 28) %>%
       graph_from_data_frame()

ggraph(bigram_graph, layout = "fr") +
       geom_edge_link() +
       geom_node_point() +
       geom_node_text(aes(label = name), vjust = 1, hjust = 1)



# adapted for light blue

set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()










# Network graph en igraph

library(ggraph)
library(igraph)
rstats <- search_tweets("#autism", n = 1000)

## create from-to data frame representing retweet/mention/reply connections
rstats_net <- network_data(rstats, "retweet,mention,reply")

## view edge data frame
rstats_net

## view user_id->screen_name index
attr(rstats_net, "idsn")

## if igraph is installed...
if (requireNamespace("iraph", quietly = TRUE)) {
  
  ## (1) convert directly to graph object representing semantic network
  rstats_net <- network_graph(rstats)
  
  ## (2) plot graph via igraph.plotting
  plot(rstats_net)
}



View(rstats_net)


strength(rstats, vids = V(graph), mode = c("all", "out", "in", "total"),
         loops = TRUE, weights = NULL)













#### PUBMED 




library(qgraph)
library("lavaan")

d  <- DatasGephiOKNOTTS

# convert character
summary(d)
d$out_degree = as.numeric(as.character(d$out_degree))
d$in_degree = as.numeric(as.character(d$in_degree))
d$betweeness = as.numeric(as.character(d$betweeness))
View(d)

# Convert dataframe
gh <- na.omit(d)
View(gh)
gt <- gh %>% select(3, 4, 5, 8)
View(gt)

# graph1
gt %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "PubMed - #autism", subtitle = "Text mining PubMed data ", x = "", y = "")



# convert to igraph
p <- graph_from_data_frame(gt, directed = FALSE, vertices = NULL)
plot(p)
o <- as_adjacency_matrix(p,edges = TRUE, names = TRUE) ## si on veut 
plot(o)



transitivity(p, type = "average") ### coefficient de clustering


ml <- cor_auto(t)



### coefficient de clustering
transitivity(o, type = "average") 




library(igrap)
data(karate)
karate <- karate %>%
  add_layout_(with_kk()) %>%
  set_vertex_attr("size", value = 10)

V(karate)$color <- scales::dscale(degree(karate) %>% cut(5), diverging_pal)
plot(karate)













## Voir site sur RainCloud

## Load the required libraries
library("ggplot2") 
library("tidyr") 
library("dplyr") 
library("cowplot") 
library("ggsci") 
source("halfViolinPlots.R") 

## Load the data
inputData <- read.delim2("Wordstwitter.csv",
                         header = TRUE,
                         sep = ';',
                         quote = '',
                         check.names = FALSE,
                         dec = ',')

## Select the columns and reorder the data if needed
inputData <- select(inputData,c("awareness","time","son","love","child","acceptance","health","education","school","understanding","limitations","information","read","neurodiversity","challenges","families"))

## Reformat the data for ggplot
plotData <- gather(inputData,
                   condition,
                   value,
                   colnames(inputData),
                   factor_key = TRUE) %>%
  filter(value != "") 

## And plot the data
ggplot(plotData, aes(x = condition, y = value, fill = condition, color = condition)) +
  ggtitle("Bootstrap of the unique words count find in tweets") +
  ylab("Assigned probability score") +
  xlab("Top sixteen words") +
  theme_cowplot() +
  scale_shape_identity() +
  theme(legend.position = "none",
        plot.title = element_text(size = 20),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = 0, 
                                   hjust = 0,
                                   vjust = 0)) +
  scale_color_simpsons() +
  scale_fill_simpsons() +
  geom_point(position = position_jitter(0.15), 
             size = 1, 
             alpha = 1, 
             aes(shape = 16)) +
  geom_flat_violin(position = position_nudge(x = 0.25, y = 0),
                   adjust = 3,
                   alpha = 0.4, 
                   trim = TRUE, 
                   scale = "width") +
  geom_boxplot(aes(x = as.numeric(condition) + 0.25, y = value), 
               notch = TRUE, 
               width = 0, 
               varwidth = FALSE, 
               outlier.shape = NA, 
               alpha = 0.3, 
               colour = "black", 
               show.legend = FALSE) +
  coord_flip()








#### Geolocalisation
autist <- lat_lng(autis)

par(mar = c(0, 0, 0, 0))

#chargement carte
maps::map('world',col="grey", fill=TRUE, bg="white", lwd=0.05, mar=rep(0,4),border=0, ylim=c(-80,80) )

with(autist, points(lng, lat, pch = 20, cex = .25, col = rgb(0, .3, .7, .75)))

(ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +     coord_fixed(1.3)
  par(mar = c(0, 0, 0, 0)))



## search for 10,000 tweets sent from the US
autist <- search_tweets("lang:en", geocode = lookup_coords("usa"), n = 10000)
autist <- lat_lng(autist)
par(mar = c(0, 0, 0, 0))
maps::map("state", lwd = .25,col="grey",fill=TRUE)

##maps::map('usa',col="grey", fill=TRUE, bg="white", lwd=0.05, mar=rep(0,4),border=0, ylim=c(-80,80) )

with(autist, points(lng, lat, pch = 10, cex = .25, col = rgb(0, .3, .7, .75)))

(ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +     coord_fixed(1.3)
  par(mar = c(0, 0, 0, 0)))




##### Most frequent
###### https://cran.r-project.org/web/packages/TSstudio/vignettes/Plotting_Time_Series.html
cleaned_tweet_words %>%
  ts_plot("hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #autism Twitter statuses from past 9 days",
    subtitle = "Aggregated using three-hour intervals")

library(plotly)
ts_plot(autism, slider = TRUE)
ts_info(autism)

#of the year

tmls <- search_tweets("#autism", n = 10000, include_rts = FALSE,retryonratelimit = TRUE )



### of the year ?????

cleaned_tweet_words %>%
  dplyr::filter(created_at > "2012-01-01") %>%
  ts_plot("weeks") +
  ggplot2::geom_point() +
  ggplot2::theme_light() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(x = NULL, y = NULL,
                title = "Frequency of #autism tweets",         subtitle = "Aggregated by weeks from October 2019" )



#clean 

autism_clea %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in tweets")