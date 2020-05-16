#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : extract Pubmed data used in the study
# date            : 2020-05-16
# version         : 1
# ==============================================================================


# If pubmedR does not work


# Same with RISmed
library(RISmed)
res <- EUtilsSummary("autis*", type="esearch", db="pubmed", datetype='pdat', retmax=500) # "autism[Title/Abstract] AND 2015:2020[DP]"
D <- EUtilsGet(res)
results_pubmed <- pubmed2df(D)
results <- biblioAnalysis(co)
