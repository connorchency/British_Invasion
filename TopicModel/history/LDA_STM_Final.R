rm(list = ls())
library('tidytext')
library('topicmodels')
library('ldatuning')
library('stringi')
library('quanteda')
libs <- c("ldatuning","topicmodels","ggplot2","dplyr","rjson","quanteda","parallel","doParallel","tidytext")
lapply(libs, library, character.only = T)
rm(libs)

# Setting seed
set.seed(826)
# Load Data
BI = read.csv('~/Desktop/British_Invasion/Preprocessing/BI_Final.csv',
              stringsAsFactors = FALSE)
BI = BI[,-1]
# Only BI1
BI = BI[BI$Label == 1,]
AM = read.csv('./Desktop/British_Invasion/Data/peer_lyrics.csv', stringsAsFactors = FALSE)
AM = AM[,-1]
BI_AM = rbind.data.frame(BI, AM, stringsAsFactors = F)

# Stopword
stopword1 = read.table('~/Documents/text_project/stopwords.txt', header = F, stringsAsFactors = FALSE)
stopword1 = as.vector(as.character(stopword1$V1))
load('~/Dropbox/text/HW3/custom_stopwords.RData')
stopw = as.vector(rbind(stopword1,custom_stopwords))
stopw = c(stopw, c('yeah')) # Add More

# DFM
tokensAll <- tokens(char_tolower(BI_AM$Lyrics), remove_punct = TRUE, 
                    remove_numbers = TRUE, remove_symbols = TRUE,
                    remove_hyphens = TRUE)
tokensNoStopwords <- removeFeatures(tokensAll, stopwords("english"))
tokensNoStopwords <- removeFeatures(tokensNoStopwords , stopwords("french"))
tokensNoStopwords = removeFeatures(tokensNoStopwords, stopw)
tokensNgramsNoStopwords <- tokens_ngrams(tokensNoStopwords, 1)
featnames(dfm(tokensNgramsNoStopwords, verbose = FALSE))
ly = dfm(tokensNgramsNoStopwords, stem=TRUE)
ly_new = dfm_trim(ly, min_docfreq = 30, verbose = T)
rowTotals <- apply(ly_new , 1, sum)
#colTotals <- apply(ly , 2, sum)
ly_new  = ly_new[rowTotals> 0, ]



# Find K
start.time <- Sys.time()
############################
result <- FindTopicsNumber(
  ly_new,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 826),
  mc.cores = 3L,
  verbose = TRUE)
############################
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
FindTopicsNumber_plot(result)

#K=?
mod<-LDA(ly_new, k = 10, method = "Gibbs",  control = list(seed = 826))

# Quickly extracts the word weights and transforms them into a data frame
topics <- tidy(mod, matrix = "beta")
# Generates a df of top terms
top_terms <- topics %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# Store the results of the distribution of topics over documents
doc_topics<-mod@gamma
# Store the results of words over topics
words_topics<-mod@beta
BI_AM_2 = BI_AM[rowTotals> 0,]
BI_AM_2 = BI_AM_2[apply(doc_topics, 2, which.max),]


# 
BI_AM_3 = BI_AM[rowTotals> 0,]
#BI_AM_3 = cbind.data.frame(apply(doc_topics, 1, which.max), BI_AM_3)
BI_topic_distr = table(apply(doc_topics, 1, which.max)[BI_AM_3$Label==1])/sum(BI_AM_3$Label==1)
AM_topic_distr = table(apply(doc_topics, 1, which.max)[BI_AM_3$Label==3])/sum(BI_AM_3$Label==3)
plot(AM_topic_distr, col=2, pch=3, type = 'o',lwd = 0.5)
points(BI_topic_distr, col=3, pch=3, type = 'o',lwd =0.5)

#
dev.new()
most_related_topic = apply(doc_topics, 1, which.max)
BI_AM_3 = cbind.data.frame(BI_AM_3, most_related_topic)
zs <- function(x) {names(which.max(table(x)))}
artist_topic = aggregate(BI_AM_3$most_related_topic, by=list(BI_AM_3$Artist), zs)
artist_nation = aggregate(BI_AM_3$Label, by=list(BI_AM_3$Artist), mean)
#  British
table(artist_topic[artist_nation$x==1,]$x)/sum(artist_nation$x==1)
plot(table(artist_topic[artist_nation$x==1,]$x)/sum(artist_nation$x==1), col=3, pch=6, type = 'p', ylim = c(0,1))
#  American
table(artist_topic[artist_nation$x==3,]$x)/sum(artist_nation$x==3)
points(table(artist_topic[artist_nation$x==3,]$x)/sum(artist_nation$x==3), col=2, pch=6, type = 'p')




#######
AM_BB = read.csv("./Desktop/British_Invasion/Data/1964_1969_Billboard_Charts.csv", 
                 stringsAsFactors = F)
chart_unique = unique(AM_BB[,c(2,4)])
full_AM = paste(AM$Artist,AM$Song)
full_BI = paste(BI$Artist,BI$Song)
full_AM_BB = paste(chart_unique$artist,chart_unique$song)
AM_New = AM[which(full_AM %in% full_AM_BB),]
BI_New = BI[which(full_BI %in% full_AM_BB),]

