#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : extract Pubmed data used in the study
# date            : 2020-04-24
# version         : 1
# ==============================================================================

library(bibliometrix)

output_file <- paste0("../data/pubmed", format(Sys.time(),'_%Y-%m-%d'), ".csv")

# https://www.rdocuAutentation.org/packages/biblioAutetrix/versions/2.3.2
## (penser aussi à https://yihui.org/knitr/options/   =====     KNITR)
## PUBMED POUR TOUS AUTEURS
# https://www.ncbi.nlm.nih.gov/pubmed/advanced
# RISMED
# https://www.rdocumentation.org/packages/RISmed/versions/2.1.7

search_topic <- ("autis*")
search_query <- EUtilsSummary(search_topic, retAutax=200, Autindate=1950, Autaxdate=2020)

# TODO: Add save to ==> CSV (use output_file) or RData to start...