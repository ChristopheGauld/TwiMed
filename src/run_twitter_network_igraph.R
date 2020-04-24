#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : Basic Twitter Network with igraph 
# date            : 2020-04-24
# version         : 1
# ==============================================================================



#### Network
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


# Other network (same but different layout) = schema dark blue non oriented
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


# Réseau de base
# Résumant la féquence des mots en fonction des couleurs et des tailles
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




