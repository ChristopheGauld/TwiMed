#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : processing pipeline to create matrix
# date            : 2020-09-11
# version         : 3 (GD)
# ==============================================================================

library(pubmedR)
library(bibliometrix)

output_file <- "data/pubmed.Rdata"

# pubmed extraction
search_query <- ("autis*")
res <- pmQueryTotalCount(search_query)
search_output <- pmApiRequest(query = search_query, res$total_count, #limit = 10000
                              , api_key = NULL)

# convert to a dataframe
results_pubmed <- convert2df(search_output, dbsource = "pubmed", format="api")
results <- biblioAnalysis(results_pubmed)
summary(results)

save(results_pubmed, file = output_file)
