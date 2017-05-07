library(readr)
BI2_lyrics <- read_csv("~/Documents/text_project/British_Invasion/Data/BI2_lyrics.csv",
                       col_types = cols(X1 = col_skip()))
BI1_lyrics <- read_csv("~/Documents/text_project/British_Invasion/Data/BI1_lyrics.csv",
                       col_types = cols(X1 = col_skip()))

# Delete 'NA' in Lyrics and Names
nrow(BI1_lyrics)
nrow(BI2_lyrics)
#nrow(BI1_lyrics[complete.cases(BI1_lyrics$Lyrics),])
nrow(BI1_lyrics[complete.cases(BI1_lyrics),])
#nrow(BI2_lyrics[complete.cases(BI2_lyrics$Lyrics),])
nrow(BI2_lyrics[complete.cases(BI2_lyrics),])
#
BI1 = BI1_lyrics[complete.cases(BI1_lyrics),]
BI2 = BI2_lyrics[complete.cases(BI2_lyrics),]



# Remove [*](square bracket) and \r \n in Lyrics
library(stringr)
rmSqBracket <- function(x) {
  tmp = gsub("\\[.*?\\]", "", x)
  tmp = str_replace_all(tmp, "[\r\n]" , " ")
  return(tmp)}
BI1$Lyrics = lapply(BI1$Lyrics,rmSqBracket)
BI2$Lyrics = lapply(BI2$Lyrics,rmSqBracket)


# Add label (BI1 or 2)
BI1['Label'] = rep(1, nrow(BI1))
BI2['Label'] = rep(2, nrow(BI2))
BI = rbind.data.frame(BI1, BI2)
nrow(BI1)
nrow(BI2)
nrow(BI)
is.data.frame(BI)

# Num of Songs for Each Artist 1&2

# Artists in BI1
nrow(table(BI1$Artist))
# Artists in BI2
nrow(table(BI2$Artist))
# All
nrow(table(BI$Artist))

# Aggregate -> Number
g = group_by(BI, Artist, Label)
agg = summarise(g, count = distinct(Song))
colnames(agg) = c('Artist', 'Label','Number')
Artist_dist = agg[with(agg, order(-Number)), ]

# Check Duplications for Each Artist
BI %>% 
  group_by(Artist, Label, Song) %>%
  summarise() %>%
  select(distinct(Song))
# Plot Hist
hist(Artist_dist$Number, breaks =40, probability = TRUE)

#Output
BI_OUT <- apply(BI,2,as.character)
write_csv(as.data.frame(BI_OUT), 'BI.csv')
# x = read.csv('./BI.csv')
write_csv(Artist_dist, 'Artist_Agg_Song.csv')
