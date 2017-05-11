rm(list = ls())
library(stm)
libs <- c('stringi',"ldatuning","topicmodels","ggplot2","dplyr","rjson","quanteda","parallel","doParallel","tidytext")
lapply(libs, library, character.only = T)
rm(libs)

load('~/Desktop/British_Invasion/Data/preprocessed.RData')


# Setting seed
set.seed(826)

# 
BA_stm = BA
BA_stm$Artist = as.factor(BA_stm$Artist)
BA_stm$Label = as.factor(BA_stm$Label)

# Modelling
corpus_gt<-textProcessor(documents=BA_stm$Lyrics,
                         metadata = BA_stm[,c("Artist","Label","Lyrics","Song")],
                         language='english',
                         customstopwords = as.vector(as.character(stopw)))
corpus_out <- prepDocuments(corpus_gt$documents,
                            corpus_gt$vocab,
                            corpus_gt$meta)
library(Rtsne)
library(geometry)

stm_mod_10 <- stm(corpus_out$documents, 
              corpus_out$vocab, 
              prevalence = ~Label+Artist, 
              data = corpus_out$meta, 
              init.type ='Spectral', 
              K = 10)

#1 Top Words
stm::labelTopics(stm_mod_10)

#2 Word Cloud
for(i in 1:10){
  # dev.new()
  stm::cloud(stm_mod_10, topic = 1)
}

#3
# A plot that summarizes the topics by what words occur most commonly in them
dev.new()
plot(stm_mod_10,type="labels")
plot(stm_mod_10, type = 'summary', text.cex = 1, n = 5)


#4 prevalence Between Label: 1 & 3
dev.new()
prep<-estimateEffect(1:10 ~ Label , stm_mod_10, meta=corpus_out$meta)
plot(prep, "Label", model=stm_mod_10,
     method="difference",cov.value1="1",cov.value2="3", xlim = c(-0.1, 0.1))

# Song
dev.control(1)
thoughts1 = findThoughts(stm_mod_10, texts=corpus_out$meta$Song, topics=1:10, n=5)
thoughts2 = findThoughts(stm_mod_10, texts=corpus_out$meta$Artist, topics=1:10, n=5)
thoughts3 = findThoughts(stm_mod_10, texts=corpus_out$meta$Lyrics, topics=1:10, n=5)
thoughts4 = findThoughts(stm_mod_10, texts=corpus_out$meta$Label, topics=1:10, n=5)
thought = cbind.data.frame(thoughts1$docs, thoughts2$docs, thoughts3$docs, thoughts4$docs)
View(thought)
thoughts3


# 
stm::topicCorr(stm_mod_10)
