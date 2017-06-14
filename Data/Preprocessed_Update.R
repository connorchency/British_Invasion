library(textcat)
load('~/Documents/NYU/Text-as-Data/British_Invasion/Data/preprocessed.RData')

lan = textcat(BA$Lyrics)


BA = BA[!duplicated(BA[,1:2]),]
BA = BA[which(BA$Lyrics != ""),]
BA = BA[which(lan != "portuguese"),]
BA = BA[which(lan != "french"),]

stopw = c('yeah','na','la','ba','da','sha', 
          'du','oo','rah','huh','ya','whoa',
          'oh', 'ooh','mmm','hmm','hm', 'mm',
          'nah', 'ah', 'aha', 'ohhh')

lyrics_corpus = corpus(BA, docnames = "Song", text_field = "Lyrics")

cus_stopwords = stopwords("english")[-(1:21)]

# Add More

ly_BA = dfm(lyrics_corpus, stem=TRUE,
            remove = c(stopwords("french"), stopw),
            removePunct = T, removeNumbers = T,
            removeSymbols = T, removeHyphens = T
            )

ly_BA_more = dfm(lyrics_corpus, stem=TRUE,
            remove = c(stopwords("french"), stopwords("english"), stopw),
            removePunct = T, removeNumbers = T,
            removeSymbols = T, removeHyphens = T
            )

ly_BA_more_2 = dfm(lyrics_corpus, stem=TRUE,
                 remove = 
                 c(stopwords("french"), cus_stopwords, stopw),
                 removePunct = T, removeNumbers = T,
                 removeSymbols = T, removeHyphens = T
)

ly_BA_a = dfm(lyrics_corpus, stem=TRUE, groups = "Artist",
              remove = c(stopwords("french"), stopw),
              removePunct = T, removeNumbers = T,
              removeSymbols = T, removeHyphens = T
              )

ly_BA_a_more = dfm(lyrics_corpus, stem=TRUE, groups = "Artist",
                 remove = c(stopwords("french"), stopwords("english"), stopw),
                 removePunct = T, removeNumbers = T,
                 removeSymbols = T, removeHyphens = T
)

ly_BA_a_more_2 = dfm(lyrics_corpus, stem=TRUE, groups = "Artist",
                   remove = c(stopwords("french"), cus_stopwords, stopw),
                   removePunct = T, removeNumbers = T,
                   removeSymbols = T, removeHyphens = T
)



