#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : extract Pubmed data used in the study
# date            : 2020-04-24
# version         : 1
# ==============================================================================

library(RISmed)
library(bibliometrix)

output_file <- "../data/pubmed.Rdata"

# https://www.rdocumentation.org/packages/bibliometrix/versions/2.3.2
## (penser aussi à https://yihui.org/knitr/options/   =====     KNITR)
## PUBMED POUR TOUS AUTEURS
# https://www.ncbi.nlm.nih.gov/pubmed/advanced
# RISMED
# https://www.rdocumentation.org/packages/RISmed/versions/2.1.7

search_topic <- ("autis*")
search_query <- EUtilsSummary(search_topic, type="esearch", db="pubmed", retmax=200, mindate=1950, maxdate=2020)

# Toutes caractéristiques transformées en df
D <- EUtilsGet(search_query, type="efetch", db="pubmed")
m <- pubmed2df(D, dbsource= "pubmed")
# View(m)

save.image(output_file)