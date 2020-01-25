#load packages
library(nnet)
library(ROCR)
library(devtools)
library(InformationValue)
#Read the data set
setwd("C:/education/4 Sem/iim banglore conf")
data<-read.csv("creditcard.csv",header = T)
#create data set 
samplesize = 0.70 * nrow(data)
set.seed(80)
index = sample( seq_len ( nrow ( data ) ), size = samplesize )
train = data[ index, ]
test = data[ -index, ]
##neural network model
set.seed(777)
modelNN<-nnet(Class~Time+V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24+V25+V26+V27+V28+Amount ,train,size=21,rang=0.07,Hess=FALSE,decay=15e-4,maxit=2000)
predictionNN<-predict(modelNN,test)
table(predictionNN)
##ROC curve
optCutOff<-0.8
misClassError(test$detect, predictionNN, threshold = optCutOff)
plotROC(test$detect, predictionCart)
##Confusion Matrix
Concordance(test$detect, predictionNN)
sensitivity(test$detect, predictionNN, threshold =optCutOff)
specificity(test$Class, predictionNN, threshold = optCutOff)
confusionMatrix(test$Class, predictionNN, threshold = optCutOff)

