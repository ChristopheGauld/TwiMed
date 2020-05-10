#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : How to make a nice Twitter Raincloud 
# date            : 2020-04-24
# version         : 1
# ==============================================================================



## RainCloud



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



