#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : processing pipeline for Pubmed data
# date            : 2020-05-08
# version         : 1
# ==============================================================================

library(tm)

input_file <- "../data/pubmed.Rdata"
output_file <- "../data/pubmed_tdm.Rdata"

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
tdm_pubmed = TermDocumentMatrix(datatxt.corpus)
# save to data folder
save(tdm_pubmed, file = output_file)
                         
#Transformer en matrice 
tdm_pubmed %>%
  as.data.frame.matrix() %>%
  mutate(name = row.names(.)) %>%
  arrange(desc(`1`))
                         
# Visualisation de la matrice dans ggplot : déf du thème 
tkrtheme <- theme(plot.title=element_text(margin=margin(0,0,20,0), size=20, hjust = 0.5),
        plot.subtitle = element_text(margin=margin(0,0,20,0), size = 15, hjust = 0.5),
        panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(colour = "grey"), 
        plot.margin = margin(20,50,20,50)) 
                         
# Plot lui même               
ggplot(booktm[1:25,], aes(reorder(name, `1`), `1`)) + 
  geom_bar(stat = "identity", fill = "#E3693E") +
  geom_text(aes(label= as.character(`1`)), check_overlap = TRUE, size = 4) + 
  coord_flip() + 
  xlab(" ") + 
  ylab("Volume") + 
  labs(title = "First 25 words most frequents",
       caption = capt) + 
  tkrtheme

library(qgraph)
# après avoir fait nécessairement as.data.frame.matrix
e <- qgraph(cor(tdm_pubmed))

        
                         
# Méthode équivalente et similaire à celle utilisée pour Twitter = non pas library(tm) mais library(tidytext)
# Créer un data_frame
tidytext <- data_frame(line = 1:nrow(tdm_pubmed), text = tdm_pubmed) #### ici!!!! -> tdm_pubmed ou autre ???? 
#Analyse de fréquence 
tidytext <- tidytext %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) 

ggplot(tidytext[1:25,], aes(reorder(word, n), n)) + 
  geom_bar(stat = "identity", fill = "#E3693E") +
  geom_text(aes(label= as.character(n)), check_overlap = TRUE, size = 4) + 
  coord_flip() + 
  xlab(" ") + 
  ylab("Volume") + 
  labs(title = "25 mots les plus récurrents avec le package tidytext",
       caption = capt) + 
  tkrtheme                     
