library(NLP)
library(RTextTools)
library(tm)
load("./Documents/Spring2017/DSGA_3001_Text/British_Invasion/Data/preprocessed.RData")

dtm = create_matrix(BA$Lyrics, language = 'english',
                    stemWords = TRUE, 
                    weighting = tm::weightTfIdf,
                    removePunctuation = T, 
                    removeStopwords = T,
                    removeNumbers = T) 
OS_acc =  function(x) {
  train_break = as.integer(x*nrow(rw)) # or sampling again!
  container = create_container(dtm, t(rw$`actual score`),
                               trainSize = 1:train_break,
                               testSize = (train_break+1):nrow(rw),
                               virgin = FALSE) 
  cv.svm = cross_validate(container, nfold = 5, 
                          algorithm = 'SVM', kernel = 'linear')
  print(cv.svm$meanAccuracy)}

train_break = as.integer(1*nrow(BA))
container = create_container(dtm, t(BA$Label),
                             trainSize = 1:train_break,
                             virgin = FALSE) 


test = read.csv("./Documents/Spring2017/DSGA_3001_Text/British_Invasion/American_Respond/AM_uncertain_with_ly.csv", stringsAsFactors = F)


set.seed(826)
svm_radial = train_model(container = container, algorithm = "SVM", kernel = 'radial')
radial_pred = classify_model(container = container, model = svm_radial)
radial_tf = radial_pred$SVM_LABEL == factor(rw$`actual score`)[(train_break+1):nrow(rw)]
length(radial_tf[radial_tf==TRUE])/ length((train_break+1):nrow(rw)) # accuracy for test data