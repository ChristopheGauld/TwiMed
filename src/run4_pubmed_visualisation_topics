#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : processing pipeline to create matrix
# date            : 2020-05-30
# version         : 3
# ==============================================================================

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
