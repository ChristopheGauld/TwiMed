
setwd("~/Desktop/R/TwiMed/DATAS")

library(pubmedR)
library(bibliometrix)

output_file <- "data/pubmed.Rdata"


# pubmed extraction
search_query <- ("autis*")
res <- pmQueryTotalCount(search_query)
search_output <- pmApiRequest(query = search_query, limit = 100 #res$total_count
                              , api_key = NULL)

# convert to a dataframe
results_pubmed <- convert2df(search_output, dbsource = "pubmed", format="api")
results <- biblioAnalysis(results_pubmed)
summary(results)

save(results_pubmed, file = output_file)

##############################################################################################################
# Same with RISmed
library(RISmed)
res <- EUtilsSummary("autis*", type="esearch", db="pubmed", datetype='pdat', retmax=100) # "autism[Title/Abstract] AND 2015:2020[DP]"
D <- EUtilsGet(res)
results_pubmed <- pubmed2df(D)
save(results_pubmed, file = output_file)

results <- biblioAnalysis(co)