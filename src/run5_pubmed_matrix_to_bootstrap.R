#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : processing pipeline for bootstrat PubMed data
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

#### Use run_pubmed to obtain the matrix_pubmed
input_file <- "../data/pubmed_tdm.Rdata"
output_file1 <- "../fig/pubmed_bs.pdf"
output_file2 <- "../data/pubmed_bs.Rdata"
load(input_file)
runFlag <- FALSE

if (runFlag) {
    # Number of bootstraps
    n_bs <- 1000

    # Take only the 15 most frequent words (repeat)
    words_pubmed15 = tidy.pubmed4$word[1:15]

    # Occurrency bootsrap and raincloud plot
    # Occurency function returns the occurence number for a word in the 'data' vector
    occurence_random_words <- function(data, indices, word) {
      ab.pubmed <- data[indices,] # allows boot to select sample
      tidy_pub <- ab.pubmed %>%
        unnest_tokens(word, AB)
      return(sum(tidy_pub$word == word))
    }

    # initialisation of a empty result table
    results=matrix(NA,n_bs,15)
    results=as.data.frame(results)
# For each of the 15 words, we calculate the 1000 values of bootsrap and we put them in a column of the results table
    
    for (i in 1:15) {
      result <- boot(data=ab.pubmed, statistic=occurence_random_words, R=n_bs, word=words_pubmed15[i])
      results[,i]=result$t # We fill a column of the dataframe of the results with the values of bootsrap
      names(results)[i]=words_pubmed15[i] # We give the word as the variable name of the dataframe      print(names(results)[i])
    }
    save(results, file = output_file2)
} else {
   load(output_file2)
}

## And plot the data
#source("load_function_violin.R")
# cf run_twitter_03b

## Select  columns + reorder
inputData_pubmed <- select(results,colnames(results))

## Reformate
plotData_pubmed <- gather(inputData_pubmed,
                   condition,
                   value,
                   colnames(inputData_pubmed),
                   factor_key = TRUE) %>%
  filter(value != "") 

## Plot
p <- ggplot(plotData_pubmed, aes(x = condition, y = value, fill = condition, color = condition)) +
            ggtitle("Bootstrap of the unique words count find in PubMed") +
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
                                             vjust = -0.25)) +
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
            scale_x_discrete(limits = unique(rev(plotData_pubmed$condition))) +
            background_grid()

pdf(file = output_file1, width=14, height=14)
print(p)
dev.off()
