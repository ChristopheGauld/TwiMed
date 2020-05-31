rm(list=ls())

library(topicmodels)
library(tidytext)
library(dplyr)

setwd("~/Desktop/R/TwiMed/DATAS")
input_file <- "data/pubmed_tdm.Rdata" 
output_file <- "data/pubmed_tdm_group.Rdata"

# load the pubmed dataframe
load(input_file)

# set a seed so that the output of the model is predictable
lda_pubmed <- LDA(dtm_pubmed, k = 7, control = list(seed = 1234))

# compute word-topic probabilities
topics_prob_pubmed <- tidy(lda_pubmed, matrix = "beta")

# select for each term the topic with the highest probability of generating it
topic_max <- topics_prob_pubmed %>%
  group_by(term) %>%
  filter(beta == max(beta)) %>%
  ungroup()

# Put a list of the 4 groups of terms, class by topic selected above
topic1 <- topic_max$term[topic_max$topic==1]
topic2 <- topic_max$term[topic_max$topic==2]
topic3 <- topic_max$term[topic_max$topic==3]
topic4 <- topic_max$term[topic_max$topic==4]
group <- list(topic1,topic2,topic3,topic4)

# create a cor matrix of the dtm
matrix_pubmed <- as.matrix(dtm_pubmed)

# save
save(matrix_pubmed, group, file = output_file)



# Visualisation #####
# Visualisation of 10 most common terms in each topic
library(ggplot2)
library(tidyr)

top_terms_topic_pubmed <- topics_prob_pubmed %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_topic_pubmed %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

# Visualisation of the 20 words with the greatest difference between topic 1 and topic 2
beta_spread <- topics_prob_pubmed %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))
beta_spread

top_beta_spread <- beta_spread %>%
  top_n(10, log_ratio) %>%
  arrange(-log_ratio)
down_beta_spread <- beta_spread %>%
  top_n(-10, log_ratio) %>%
  arrange(-log_ratio)
top_beta_spread = rbind(top_beta_spread,down_beta_spread)

top_beta_spread %>%
  mutate(term = reorder_within(term, log_ratio, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_reordered()
