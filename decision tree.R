##package installed
library(pROC)
library(Rcpp)
library(ROCR)
library(rpart)
library(rpart.plot)
library(caret)
library(InformationValue)
## read data
data<-read.csv("creditcard.csv",header = T)
##split data
samplesize = 0.70 * nrow(data)
set.seed(80)
index = sample( seq_len ( nrow ( data ) ), size = samplesize )
train = data[ index, ]
test = data[ -index, ]
## decision tree
modelCart = rpart(Class~Time+V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24+V25+V26+V27+V28+Amount , data=train)
prp(modelCart)
predictionCart <- predict(modelCart, newdata=test)
t1 <- table(test$Class, predictionCart)
(t1[1]+t1[4])/(nrow(test))
## roc
optCutOff<-0.8
misClassError(test$Class, predictionCart, threshold = optCutOff)
plotROC(test$Class, predictionCart)
## confusion matrix
Concordance(test$Class, predictionCart)
sensitivity(test$Class, predictionCart, threshold =optCutOff)
specificity(test$Class, predictionCart, threshold = optCutOff)
confusionMatrix(test$Class, predictionCart, threshold = optCutOff)

