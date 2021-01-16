#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : processing pipeline for bootstrat Twitter data
# date            : 2020-06-04
# version         : 4
# ==============================================================================

library(boot)
library(dplyr)
library(tidyr)
library(ggplot2)
source("load_function_violin.R")

#### load the tidy objet of twitter words
input_file <- "../data/twitter_tdm.Rdata"
output_file <- "../fig/fig1a.pdf"

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


## And plot the data
source("../src/run5_twitter_load_function_violin.R") 

inputData_twitter <- select(results,colnames(results))

## Reformater 
plotData_twit <- gather(inputData_twitter,
                   condition,
                   value,
                   colnames(inputData_twitter),
                   factor_key = TRUE) %>%
  filter(value != "") 

## Plot
ggplot(plotData_twit, aes(x = condition, y = value, fill = condition, color = condition)) +
  ggtitle("Bootstrap of the unique words count find in Twitter") +
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
