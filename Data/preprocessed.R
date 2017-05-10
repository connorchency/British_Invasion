rm(list = ls())
library('tidytext')
library('topicmodels')
library('ldatuning')
library('stringi')
library('quanteda')
libs <- c("ldatuning","topicmodels","ggplot2","dplyr","rjson","quanteda","parallel","doParallel","tidytext")
lapply(libs, library, character.only = T)
rm(libs)

# Seed
set.seed(826)

# Load
BA = read.csv('./Desktop/British_Invasion/Data/BI_1181.csv', stringsAsFactors = F)

# Stopword
stopword1 = read.table('~/Documents/text_project/stopwords.txt', header = F, stringsAsFactors = FALSE)
stopword1 = as.vector(as.character(stopword1$V1))
load('~/Dropbox/text/HW3/custom_stopwords.RData')
stopw = as.vector(rbind(stopword1,custom_stopwords))
stopw = c(stopw, c('yeah','na','la','ba','da','sha', 
                   'du','oo','rah','huh','ya','whoa',
                   'oh', 'ooh','mmm','hmm','hm',
                   'nah')) # Add More

# DFM
tokensAll <- tokens(char_tolower(BA$Lyrics), remove_punct = TRUE, 
                    remove_numbers = TRUE, remove_symbols = TRUE,
                    remove_hyphens = TRUE, stem=TRUE)
tokensNoStopwords <- removeFeatures(tokensAll, stopwords("english"))
tokensNoStopwords <- removeFeatures(tokensNoStopwords , stopwords("french"))
tokensNoStopwords = removeFeatures(tokensNoStopwords, stopw)
tokensNgramsNoStopwords <- tokens_ngrams(tokensNoStopwords, 1)
# featnames(dfm(tokensNgramsNoStopwords, verbose = FALSE))
ly_BA = dfm(tokensNgramsNoStopwords, stem=TRUE)
