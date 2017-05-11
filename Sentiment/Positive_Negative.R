neg = read.table("./Desktop/dict/negative-words.txt",header = F, stringsAsFactors = F)
pos = read.table("./Desktop/dict/positive-words.txt",header = F, stringsAsFactors = F)
n = neg[,1]
p = pos[,1]

PN = dictionary(list(positive=p, negative=n))
load("./Documents/Spring2017/DSGA_3001_Text/British_Invasion/Data/preprocessed.RData")

ly = BA$Lyrics
res = dfm(ly, dictionary=PN, tolower = TRUE)
BA['p-n'] = res@x[1:1102]-res@x[1103:2204]
BA['PN'] = 1*(BA$`p-n` > 0) 
View(BA)

#
mean(BA$`p-n`)
fivenum(BA$`p-n`)
median(BA$`p-n`)
hist(BA$`p-n`)

# British
table(BA[BA$Label==1,]$PN)
table(BA[BA$Label==1,]$PN)/sum(BA$Label==1)
sort(table(BA[BA$Label == 1, ]$Artist))
table(BA[BA$Artist=='The Rolling Stones',]$PN)/sum(BA$Artist=='The Rolling Stones')
table(BA[BA$Artist=='The Beatles',]$PN)/sum(BA$Artist=='The Beatles')

# AM
table(BA[BA$Label==3,]$PN)
table(BA[BA$Label==3,]$PN)/sum(BA$Label==3)
sort(table(BA[BA$Label == 3, ]$Artist))
table(BA[BA$Artist=='Al Martino',]$PN)/sum(BA$Artist=='Al Martino')
table(BA[BA$Artist=='Dionne Warwick',]$PN)/sum(BA$Artist=='Dionne Warwick')
table(BA[BA$Artist=='The Beach Boys',]$PN)/sum(BA$Artist=='The Beach Boys')


# BR
hist(table(BA[BA$Label==1,]$`p-n`))
mean(BA[BA$Label==1,]$`p-n`)
sd(BA[BA$Label==1,]$`p-n`)
fivenum(BA[BA$Label==1,]$`p-n`)
# AM
hist(table(BA[BA$Label==3,]$`p-n`))
mean(BA[BA$Label==3,]$`p-n`)
sd(BA[BA$Label==3,]$`p-n`)
fivenum(BA[BA$Label==3,]$`p-n`)

library(ggplot2)
ggplot(data=BA, aes(BA$`p-n`)) + 
  geom_histogram(breaks=seq(-30, 70, by = 4), 
                 col="purple", 
                 fill="purple", 
                 alpha = .2) + 
  labs(title="Histogram for Sentiment Score") +
  labs(x="sentiment score", y="counts") + 
  xlim(c(-30,70)) + 
  ylim(c(0,100))

BA$Label = factor(BA$Label, labels = c("British", "American"))

fill <- "#4271AE"
line <- "#1F3552"
p <- ggplot(BA, aes(y=BA$`p-n`, x =BA$Label))
p + geom_boxplot(fill = fill, colour = line, alpha = 0.7) + scale_x_discrete(name = "British Invasion or American Peer") +
  scale_y_continuous(name = "Sentiment \nScore") + 
  ggtitle("Lyrics: Sentiment Analysis") + theme_bw() +
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(size = 17, family = "Tahoma", face = "bold"),
        text=element_text(family="Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(colour="black", size = 14),
        axis.text.y=element_text(colour="black", size = 14),
        axis.line = element_line(size=0.5, colour = "black"))
