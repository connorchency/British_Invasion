library(readr)
sup_lyrics <- read.csv("~/Documents/NYU/Text-as-Data/British_Invasion/Preprocessing/supplemental_BI_lyrics.csv")

sup_lyrics = sup_lyrics[,-1]

library(stringr)
rmSqBracket <- function(x) {
  tmp = gsub("\\[.*?\\]", "", x)
  tmp = str_replace_all(tmp, "[\r\n]" , " ")
  return(tmp)}

sup_lyrics$lyrics = lapply(sup_lyrics$lyrics,rmSqBracket)
names(sup_lyrics)[1:3] = c("Artist", "Song", "Lyrics")

sup_lyrics['Label'] = 1

BI1_lyrics <- read.csv("~/Documents/NYU/Text-as-Data/British_Invasion/Preprocessing/BI.csv")

BI_OUT = rbind(sup_lyrics, BI1_lyrics)

write.csv(as.data.frame(BI_OUT),
          '~/Documents/NYU/Text-as-Data/British_Invasion/Preprocessing/BI_Final.csv',
          fileEncoding = 'utf-8')
