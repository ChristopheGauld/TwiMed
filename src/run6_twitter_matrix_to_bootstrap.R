#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : processing pipeline for bootstrat Twitter data
# date            : 2020-06-04
# version         : 4
# ==============================================================================

input_file <- "../data/twitter_tdm.Rdata"
output_file <- "../fig/fig1.pdf"

library(boot)
library(dplyr)
library(tidyr)
library(ggplot2)

#### load the tidy objet of twitter words
load(input_file)

words15 <- tidy_twitter4$word[1:15]

# Occurences Bootstrap and raincloud plot
# Define the occurence function which returns the number of occurrences of a word in the vector data. Leave the line of data (indices seems to be necessary to select the samples of data with discount)
occurence_random_tweet <- function(data, indices, word) {
  autis_tweets <- data[indices,] # allows boot to select sample
  tidy_twitter <- autis_tweets %>%
    unnest_tokens(word, text)
  return(sum(tidy_twitter$word == word))
}

# initialisation d'un tableau de résultat vide
results2 <- matrix(NA,1000,15)
results2 <- as.data.frame(results)
# For each of the 15 words, we calculate the 1000 values of bootstrap and put them in a column of the results table
for (i in 1:15) {
  result2 <- boot(data=autis_tweets, statistic=occurence_random_tweet, R=1000, word=words15[i])
  results2[,i] <- result$t # We fill a column of the dataframe of the results with the 1000 values of bootsrap
  names(results2)[i] <- words15[i] 
print(names(results2)[i])
}
summary(results2[,1])
quantile(results2[,1],c(.025,.975))
PropCIs::exactci(281,15378,.95)$conf.int*15378

## Reformat the data for ggplot
library(dplyr)
library(tidyr)
plotData <- gather(results,wordname,occurency
                   ,factor_key = TRUE) #to have a sort plot

## And plot the data
library(ggplot2)
source("../src/run5_twitter_load_function_violin.R") 
p <- ggplot(plotData,aes(x=plotData[,1],y=plotData[,2], 
                          fill = plotData[,1], colour = plotData[,1]),trim = TRUE)+
  geom_flat_violin(position = position_nudge(x = .3, y = 0), adjust = 1)+
  geom_point(position = position_jitter(width = .2,height = 0), size = .3) +
  guides(fill=FALSE, colour = FALSE)+
  ylab("ylab") +
  xlab("xlab") +
  coord_flip() 

# plot in a pdf file
pdf(file = output_file)
p
dev.off()
