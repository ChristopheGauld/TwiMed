#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : Violin plot analyses on the Twitter data
# date            : 2020-04-25
# version         : 1
# ==============================================================================



################
################ FIRST METHOD
################

# Libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(hrbrthemes)
library(viridis)

# dataset
data <- results
data <- data %>%
  gather(key="text", value="value") %>%
  mutate(value = round(as.numeric(value),0))

# Plot
p <- data %>%
  mutate(text = fct_reorder(text, value)) %>% # Reorder data
  ggplot( aes(x=text, y=value, fill=text, color=text)) +
  geom_violin(width=2.1, size=0.2) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme(
    legend.position="none"
  ) +
  coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  xlab("") +
  ylab("Assigned Probability (%)")

p




################
################ SECOND METHOD
################



## Load the required libraries
library("ggplot2") 
library("tidyr") 
library("dplyr") 
library("cowplot") 
library("RColorBrewer") 
library("ggpubr") 
library("Hmisc") 
source("halfViolinPlots.R") 


## Load the data
inputData <- read.delim2("../data/Wordstwitter.csv",
                         header = TRUE,
                         sep = ';',
                         quote = '',
                         check.names = FALSE,
                         dec = ',')

## Select the columns and reorder the data if needed
inputData <- select(inputData,c("autismawareness","amp","people","education","spectrum","school","child","acceptance","understanding","love","time"))

## Reformat the data for ggplot
plotData <- gather(inputData,
                   condition,
                   value,
                   colnames(inputData),
                   factor_key = TRUE) %>%
  filter(value != "") 

## And plot the data
ggplot(plotData, aes(x = condition, y = value, fill = condition, color = condition)) +
  ggtitle("Violin Twitter") +
  ylab("") +
  xlab("") +
  theme_cowplot() +
  scale_shape_identity() +
  theme(legend.position = "none",
        plot.title = element_text(size = 20),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = 0, 
                                   hjust = 0,
                                   vjust = 0)) +
  scale_colour_brewer(palette = "Reds") +
  scale_fill_brewer(palette = "Reds") +
  geom_point(position = position_jitter(0.15), 
             size = 2, 
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
