#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : processing pipeline for bootstrat Twitter data
# date            : 2021-01-19
# version         : 5 (Guillaume Dumas)
# ==============================================================================

rm(list=ls())

library(boot)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot) 
library(ggsci)
library(tidytext)
source("load_function_violin.R")

# Load the tidy objects of twitter words
input_file <- "../data/twitter_tdm.Rdata"
output_file1 <- "../fig/fig2a.pdf"
output_file2 <- "../data/twitter_bs.Rdata"
load(input_file)
runFlag <- FALSE

if (runFlag) {
    # Number of bootstraps
    n_bs <- 1000

    # Take only the 15 most frequent words (repeat)
    words_twitter15 <- tidy_twitter4$word[1:15]

    # Occurences Bootstrap and raincloud plot
    # Define the occurence function which returns the number of occurrences of a word in the vector data. Leave the line of data (indices seems to be necessary to select the samples of data with discount)
    occurence_random_tweet <- function(data, indices, word) {
      autis_tweets <- data[indices,] # allows boot to select sample
      tidy_twitter <- autis_tweets %>%
        unnest_tokens(word, text)
      return(sum(tidy_twitter$word == word))
    }

    # Initialization with an empty table
    results2 <- matrix(NA,n_bs,15)
    results2 <- as.data.frame(results2)
    # For each of the 15 words, we calculate the values of bootstrap and put them in a column of the results table
    for (i in 1:15) {
      result2 <- boot(data=autis_tweets, statistic=occurence_random_tweet, R=n_bs, word=words_twitter15[i])
      results2[,i] <- result2$t # We fill a column of the dataframe of the results with the values of bootsrap
      names(results2)[i] <- words_twitter15[i] 
      print(names(results2)[i])
    }
    save(results2, file = output_file2)
} else {
    load(output_file2)
}

# Reformat the data
inputData_twitter <- select(results2,colnames(results2))
plotData_twit <- gather(inputData_twitter,
                   condition,
                   value,
                   colnames(inputData_twitter),
                   factor_key = TRUE) %>% filter(value != "") 

# Plot
p <- ggplot(plotData_twit, aes(x = condition, y = value, fill = condition, color = condition)) +
            ggtitle("Bootstrap of the unique words count find in Twitter") +
            xlab("Top fifteen words") +
            ylab("Assigned Probability Score") +
            theme_half_open() +
            scale_shape_identity() +
            theme(legend.position = "none",
                  plot.title = element_text(size = 20),
                  axis.title = element_text(size = 15),
                  axis.text = element_text(size = 12),
                  axis.text.x = element_text(angle = 0, 
                                             hjust = 0,
                                             vjust = 0),
                  axis.text.y = element_text(angle = 0, 
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
            geom_boxplot(aes(x = as.numeric(condition) + 0.25, y = rev(value)),
                         notch = TRUE, 
                         width = 0.1, 
                         varwidth = FALSE, 
                         outlier.shape = NA, 
                         alpha = 0.3, 
                         colour = "black", 
                         show.legend = FALSE) + coord_flip() +
            scale_x_discrete(limits = unique(rev(plotData_twit$condition))) +
            background_grid()

pdf(file = output_file1, width=14, height=14)
print(p)
dev.off()
