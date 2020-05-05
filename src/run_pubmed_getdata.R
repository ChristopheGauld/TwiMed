#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : extract Pubmed data used in the study
# date            : 2020-04-24
# version         : 1
# ==============================================================================

library(pubmedR)
library(bibliometrix)

output_file <- "../data/pubmed.Rdata"

# https://www.rdocumentation.org/packages/bibliometrix/versions/2.3.2
# https://www.ncbi.nlm.nih.gov/pubmed/advanced

search_query <- ("autis*")
res <- pmQueryTotalCount(search_topic)
search_output <- pmApiRequest(query = search_query, limit = res$total_count, api_key = NULL)

# Toutes caractéristiques transformées en df
results_pubmed <- convert2df(search_output, dbsource = "pubmed", format="pubmed")

save(results_pubmed, file = output_file)