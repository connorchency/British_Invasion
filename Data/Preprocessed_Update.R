load('~/Documents/NYU/Text-as-Data/British_Invasion/Data/preprocessed.RData')


BA = BA[,-1]
BA = BA[!duplicated(BA[,1:2]),]
BA = BA[which(BA$Lyrics != ""),]

stopw = c('yeah','na','la','ba','da','sha', 
          'du','oo','rah','huh','ya','whoa',
          'oh', 'ooh','mmm','hmm','hm',
          'nah')

lyrics_corpus = corpus(BA, docnames = "Song",text_field = "Lyrics")


 # Add More

ly_BA = dfm(lyrics_corpus, stem=TRUE,
            remove = c(stopwords("French"), stopw),
            removePunct = T, removeNumbers = T,
            removeSymbols = T, removeHyphens = T
            )

ly_BA_more = dfm(lyrics_corpus, stem=TRUE,
            remove = c(stopwords("French"), stopwords("English"), stopw),
            removePunct = T, removeNumbers = T,
            removeSymbols = T, removeHyphens = T
            )

ly_BA_a = dfm(lyrics_corpus, stem=TRUE, groups = "Artist",
              remove = c(stopwords("French"), stopw),
              removePunct = T, removeNumbers = T,
              removeSymbols = T, removeHyphens = T
              )

ly_BA_a_more = dfm(lyrics_corpus, stem=TRUE, groups = "Artist",
                 remove = c(stopwords("French"), stopwords("English"), stopw),
                 removePunct = T, removeNumbers = T,
                 removeSymbols = T, removeHyphens = T
)


#lyrics_trim = dfm_trim(lyrics_dfm, min_count = 20, verbose = T)
#lyrics_dist = dist(lyrics_trim, method = "euclidean")
#hc = hclust(lyrics_dist)

#plot(hc, hang = -1)

