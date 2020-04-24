#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : Most frequent tweets= Timelines 
# date            : 2020-04-24
# version         : 1
# ==============================================================================





##### Most frequent
cleaned_tweet_words %>%
  ts_plot("hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #autism Twitter statuses from past 9 days",
    subtitle = "Aggregated using three-hour intervals")

library(plotly)
ts_plot(autism, slider = TRUE)
ts_info(autism)

#of the year
tmls <- search_tweets("#autism", n = 10000, include_rts = FALSE,retryonratelimit = TRUE )
cleaned_tweet_words %>%
  dplyr::filter(created_at > "2012-01-01") %>%
  ts_plot("weeks") +
  ggplot2::geom_point() +
  ggplot2::theme_light() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(x = NULL, y = NULL,
                title = "Frequency of #autism tweets",         subtitle = "Aggregated by weeks from October 2019" )
