library(readr)
sup_lyrics <- read.csv("~/Documents/NYU/Text-as-Data/British_Invasion/Preprocessing/supp.csv")
con_lyrics <- read.csv("~/Documents/NYU/Text-as-Data/British_Invasion/Data/Contemporary_Artists_Song_Lyrics.csv")


library(stringr)
rmSqBracket <- function(x) {
  tmp = gsub("\\[.*?\\]", "", x)
  tmp = str_replace_all(tmp, "[\r\n]" , " ")
  return(tmp)
  }

sup_lyrics = sup_lyrics[,-1]

sup_lyrics$lyrics = lapply(sup_lyrics$lyrics,rmSqBracket)

con_lyrics$Lyrics = lapply(con_lyrics$Lyrics,rmSqBracket)

names(sup_lyrics)[1:3] = c("Artist", "Song", "Lyrics")

sup_lyrics['Label'] = 1
#con_lyrics['Label'] = 3

all_lyrics <- read.csv("~/Documents/NYU/Text-as-Data/British_Invasion/Data/BA_1128.csv")


all_lyrics = all_lyrics[,-1]

names(all_lyrics)

BI_OUT = rbind(sup_lyrics, all_lyrics)

BI_OUT[,3] = as.character(BI_OUT[,3])

write.csv(BI_OUT,
          '~/Documents/NYU/Text-as-Data/British_Invasion/Data/BI_1181.csv',
          fileEncoding = 'utf-8')

con_lyrics$Lyrics = as.character(con_lyrics$Lyrics)

write.csv(con_lyrics, '~/Documents/NYU/Text-as-Data/British_Invasion/Data/peer_lyrics.csv')
