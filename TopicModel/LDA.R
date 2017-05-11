rm(list = ls())
library('tidytext')
library('topicmodels')
library('ldatuning')
library('stringi')
library('quanteda')
libs <- c("ldatuning","topicmodels","ggplot2","dplyr","rjson","quanteda","parallel","doParallel","tidytext")
lapply(libs, library, character.only = T)
rm(libs)

load('~/Desktop/British_Invasion/Data/preprocessed.RData')

# Setting seed
set.seed(826)

# Delete Duplications
###################################################################################

# Find K
# start.time <- Sys.time()
# ############################
# result <- FindTopicsNumber(
#   ly_new,
#   topics = seq(from = 2, to = 20, by = 1),
#   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#   method = "Gibbs",
#   control = list(seed = 826),
#   mc.cores = 3L,
#   verbose = TRUE)
# ############################
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# time.taken
# FindTopicsNumber_plot(result)

ly_BA_more_2_tr = dfm_smooth(ly_BA_more_2, smoothing = 1)
ly_BA_more_2_tr = quanteda::dfm_trim(ly_BA_more_2_tr, min_count=1122, verbose=T) # 1102+20
mod<-LDA(ly_BA_more_2_tr, k = 10, method = "Gibbs",  control = list(seed = 826))


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

# The most Representative Song for Each Topic
BI_AM_2 = BA
BI_AM_2 = BI_AM_2[apply(doc_topics, 2, which.max),]

# More
k=nrow(doc_topics)
which.max2<-function(x){
  which(x == sort(x,partial=(k-1))[k-1])
}
which.max3<-function(x){
  which(x == sort(x,partial=(k-2))[k-2])
}
which.max4<-function(x){
  which(x == sort(x,partial=(k-3))[k-3])
}
which.max5<-function(x){
  which(x == sort(x,partial=(k-4))[k-4])
}
max1 = apply(doc_topics, 2, which.max)
max2 = apply(doc_topics, 2, which.max2)
max2 = sapply(max2, max)
max3 = apply(doc_topics, 2, which.max3)
max3 = sapply(max3, max)
max4 = apply(doc_topics, 2, which.max4)
max4 = sapply(max4, max)
max5 = apply(doc_topics, 2, which.max5)
max5 = sapply(max5, max)

BI_AM_2_top5 = BA
BI_AM_2_top5 = rbind.data.frame(BI_AM_2_top5[max1,],
                                BI_AM_2_top5[max2,],
                                BI_AM_2_top5[max3,],
                                BI_AM_2_top5[max4,],
                                BI_AM_2_top5[max5,])
BI_AM_2_top5$Top_Topic=seq(1:10)


# Song-Topic Distribution Between BI1 & AM
dev.new()
BI_AM_3 = BA
#BI_AM_3 = cbind.data.frame(apply(doc_topics, 1, which.max), BI_AM_3)
BI_topic_distr = table(apply(doc_topics, 1, which.max)[BI_AM_3$Label==1])/sum(BI_AM_3$Label==1)
AM_topic_distr = table(apply(doc_topics, 1, which.max)[BI_AM_3$Label==3])/sum(BI_AM_3$Label==3)
plot(AM_topic_distr, col=2, pch=3, type = 'o',lwd = 0.5)
points(BI_topic_distr, col=3, pch=3, type = 'o',lwd =0.5)


# Artist-Topic Distribution Between BI1 & AM
dev.new()
most_related_topic = apply(doc_topics, 1, which.max)
BI_AM_3 = cbind.data.frame(BI_AM_3, most_related_topic)
zs <- function(x) {names(which.max(table(x)))}
artist_topic = aggregate(BI_AM_3$most_related_topic, by=list(BI_AM_3$Artist), zs)
artist_nation = aggregate(BI_AM_3$Label, by=list(BI_AM_3$Artist), mean)
#  British
table(artist_topic[artist_nation$x==1,]$x)/sum(artist_nation$x==1)
plot(table(artist_topic[artist_nation$x==1,]$x)/sum(artist_nation$x==1), col=3, pch=6, type = 'p', ylim = c(0,0.4))
#  American
table(artist_topic[artist_nation$x==3,]$x)/sum(artist_nation$x==3)
points(table(artist_topic[artist_nation$x==3,]$x)/sum(artist_nation$x==3), col=2, pch=6, type = 'p')

