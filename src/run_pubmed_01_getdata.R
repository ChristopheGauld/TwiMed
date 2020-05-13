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

# query pubmed
search_query <- ("autis*")
res <- pmQueryTotalCount(search_query)
# collect all the results with the pubmed API
search_output <- pmApiRequest(query = search_query, limit = res$total_count, api_key = NULL)
# transform pubmed API results into a dataframe
results_pubmed <- convert2df(search_output, dbsource = "pubmed", format="api")
# preview with bibliometrix
results <- biblioAnalysis(results_pubmed)
summary(results)
# save dataframe
save(results_pubmed, file = output_file)



# Same with RISmed
library(RISmed)
res <- EUtilsSummary("autis*", type="esearch", db="pubmed", datetype='pdat', retmax=500) # "autism[Title/Abstract] AND 2015:2020[DP]"
QueryCount(res)
search_rismed <- EUtilsGet(res,type="efetch",db="pubmed")
