library(quanteda)
library(tidytext)
library(topicmodels)
library(ldatuning)
library(stringi)
library(stm)

BI = read.csv("~/Documents/text_project/British_Invasion/Preprocessing/BI.csv", stringsAsFactors = FALSE)
sum(table(BI$Artist)>5)
dim(table(BI$Artist))

artist_list = as.data.frame(table(BI$Artist))
colnames(artist_list) = c('Artist', 'Number')
final_list = artist_list[artist_list$Number>5,]$Artist

stopword1 = read.table('~/Documents/text_project/stopwords.txt', header = F, stringsAsFactors = FALSE)
stopword1 = as.vector(as.character(stopword1$V1))
load('~/Dropbox/text/HW3/custom_stopwords.RData')
stopw = as.vector(rbind(stopword1,custom_stopwords))


BI_new = BI[BI$Artist %in% final_list, ]
BI_new$Label = as.factor(BI_new$Label)
BI_new$Artist = as.factor(BI_new$Artist)
corpus_gt<-textProcessor(documents=BI_new$Lyrics,
                         metadata = BI_new[,c("Artist","Label","Lyrics")],
                         language='english',
                         stem = FALSE,
                         customstopwords = as.vector(as.character(stopw)))
corpus_out <- prepDocuments(corpus_gt$documents,
                            corpus_gt$vocab,
                            corpus_gt$meta,
                            lower.thresh=5)

library(Rtsne)
library(geometry)
stm_mod <- stm(corpus_out$documents, corpus_out$vocab, 
               prevalence = ~Label+Artist, data = corpus_out$meta, 
               init.type ='Spectral', K = 10)
stm::labelTopics(stm_mod)
stm::findThoughts(stm_mod, texts = corpus_out$meta$Lyrics, topics = c(5),n=3)
for(i in 1:10){
  stm::cloud(stm_mod, topic = 1)
}


plot(stm_mod,type="labels")
plot(stm_mod, type = 'summary', text.cex = 0.5)
prep<-estimateEffect(1:10 ~ Label , stm_mod, meta=corpus_out$meta)
plot(prep, "Label", model=stm_mod,
     method="difference",cov.value1="1",cov.value2="2", xlim = c(-0.1, 0.1))
```