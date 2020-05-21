
setwd("~/Desktop/R/TwiMed/DATAS")

library(pubmedR)
library(bibliometrix)

output_file <- "data/pubmed.Rdata"


# pubmed extraction
search_query <- ("autis*")
res <- pmQueryTotalCount(search_query)
search_output <- pmApiRequest(query = search_query, # limit = 1000 #res$total_count
                              , api_key = NULL)

# convert to a dataframe
results_pubmed <- convert2df(search_output, dbsource = "pubmed", format="api")
results <- biblioAnalysis(results_pubmed)
summary(results)

save(results_pubmed, file = output_file)
