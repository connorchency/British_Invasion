library(readr)
sup_lyrics <- read.csv("~/Documents/NYU/Text-as-Data/British_Invasion/Preprocessing/supplemental_BI_lyrics.csv")
con_lyrics <- read.csv("~/Documents/NYU/Text-as-Data/British_Invasion/Data/Contemporary_Artists_Song_Lyrics.csv")


sup_lyrics = sup_lyrics[,-1]
con_lyrics = con_lyrics[,-1]


library(stringr)
rmSqBracket <- function(x) {
  tmp = gsub("\\[.*?\\]", "", x)
  tmp = str_replace_all(tmp, "[\r\n]" , " ")
  return(tmp)
  }

sup_lyrics$lyrics = lapply(sup_lyrics$lyrics,rmSqBracket)

con_lyrics$Lyrics = lapply(con_lyrics$Lyrics,rmSqBracket)

names(sup_lyrics)[1:3] = c("Artist", "Song", "Lyrics")

sup_lyrics['Label'] = 1
con_lyrics['Label'] = 3

BI1_lyrics <- read.csv("~/Documents/NYU/Text-as-Data/British_Invasion/Preprocessing/BI.csv")

BI_OUT = rbind(sup_lyrics, BI1_lyrics)

write.csv(as.data.frame(BI_OUT),
          '~/Documents/NYU/Text-as-Data/British_Invasion/Preprocessing/BI_Final.csv',
          fileEncoding = 'utf-8')

con_lyrics$Lyrics = as.character(con_lyrics$Lyrics)

write.csv(con_lyrics, '~/Documents/NYU/Text-as-Data/British_Invasion/Data/peer_lyrics.csv')
