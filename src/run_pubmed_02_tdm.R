#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : preprocess Pubmed data into a Term Document Matrix
# date            : 2020-05-08
# version         : 1
# ==============================================================================

library(tm)

input_file <- "../data/pubmed.Rdata"
output_file <- "../data/pubmed_tdm.Rdata"

# load the pubmed data
load(input_file)
# convert to a corpus
datatxt.corpus <- Corpus(DataframeSource(data.frame(doc_id=results_pubmed$PMID, text=results_pubmed$AB)))
# remove punctuations
datatxt.corpus <- tm_map(datatxt.corpus, removePunctuation)
# remove white space
datatxt.corpus <- tm_map(datatxt.corpus, stripWhitespace)
# lowercase
datatxt.corpus <- tm_map(datatxt.corpus, tolower)
# remove common english word, terms used and human brain mapping
datatxt.corpus <- tm_map(datatxt.corpus,function(x) removeWords(x, c(stopwords("english"))))
# create corpus table
tdm_pubmed = TermDocumentMatrix(datatxt.corpus)
# save to data folder
save(tdm_pubmed, file = output_file)