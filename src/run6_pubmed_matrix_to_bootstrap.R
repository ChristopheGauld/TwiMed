#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : processing pipeline for bootstrat PubMed data
# date            : 2021-01-16
# version         : 4
# ==============================================================================

library(boot)
library(dplyr)
library(tidyr)
library(ggplot2)
library(boot)
library(cowplot) 
library(ggsci) 
source("load_function_violin.R")

#### Use run_pubmed to obtain the matrix_pubmed
input_file <- "../data/pubmed_tdm.Rdata"
output_file <- "../fig/fig1b.pdf"
load(input_file)

# Save the 15 most frequent words (repeat)
tidy.pubmed3 <- count(tidy.pubmed2,word,sort=TRUE)
words_pubmed15 = tidy.pubmed3$word[1:15]

# Bootsrap des occurences et raincloud plot #####
# Je définis la fonction occurence qui retourne le nombre d’occurence d’un word dans le vecteur data. Je laisse la ligne d =  data[indices] qui semble être nécessaire pour sélectionner les échantillons de data avec remise.
occurence_random_words <- function(data, indices, word) {
  ab.pubmed <- data[indices,] # allows boot to select sample
  tidy_pub <- ab.pubmed %>%
    unnest_tokens(word, AB)
  return(sum(tidy_pub$word == word))
}
# initialisation d'un tableau de résultat vide
results=matrix(NA,1000,15)
results=as.data.frame(results)
# Pour chacun des 15 mots, on calcule les 1000 valeurs de bootsrap et on les mets dans une colonne du tableau de résultats
for (i in 1:15) {
  result <- boot(data=ab.pubmed, statistic=occurence_random_words, R=1000, word=words_pubmed15[i])
  results[,i]=result$t # On remplit une colonne du dataframe des résultats avec les 1000 valeurs de bootsrap
  names(results)[i]=words_pubmed15[i] # On donne le mot en nom de variable du dataframe
  print(names(results)[i])
}

## And plot the data
source("/geom_flat_violin.R") # cf run_twitter_03b

## Select  columns + reorder
inputData <- select(results,c("social",	"brain"	,"parents",	"clinical","behavior",	"genetic",	"development",	"genes",	"risk",	"diagnosis",	"health",	"individuals",	"treatment",	"gene",	"protein"))

## Reformater 
plotData <- gather(inputData,
                   condition,
                   value,
                   colnames(inputData),
                   factor_key = TRUE) %>%
  filter(value != "") 

## Plot
ggplot(plotData, aes(x = condition, y = value, fill = condition, color = condition)) +
  ggtitle("Bootstrap of the unique words count find in PubMed") +
  ylab("Top fifteen words") +
  xlab("Assigned Probability Score") +
  theme_cowplot() +
  scale_shape_identity() +
  theme(legend.position = "none",
        plot.title = element_text(size = 20),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 0, 
                                   hjust = 0,
                                   vjust = 0)) +
  scale_color_igv() +
  scale_fill_igv() +
  geom_point(position = position_jitter(0.2), 
             size = 0, 
             alpha = 1, 
             aes(shape = 16)) +
  geom_flat_violin(position = position_nudge(x = 0.25, y = 0),
                   adjust = 2,
                   alpha = 0.6, 
                   trim = TRUE, 
                   scale = "width") +
  geom_boxplot(aes(x = as.numeric(condition) + 0.25, y = value), 
               notch = TRUE, 
               width = 0.1, 
               varwidth = FALSE, 
               outlier.shape = NA, 
               alpha = 0.3, 
               colour = "black", 
               show.legend = FALSE) +
  coord_flip()





# plot in a pdf file
pdf(file = output_file)
p
dev.off()
