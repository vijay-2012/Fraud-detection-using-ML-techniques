# required packages.
library(party)
library(randomForest)
library(caTools)
#read data
setwd("C:/education/4 Sem/iim banglore conf")
data<-read.csv("sample.csv",header = T)
#split data
samplesize = 0.70 * nrow(data)
set.seed(80)
index = sample( seq_len ( nrow ( data ) ), size = samplesize )
datatrain = data[ index, ]
datatest = data[ -index, ]
# Create the forest.
output.forest <- randomForest(detect ~Time+V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24+V25+V26+V27+V28+Amount,importance=T, data = datatrain)
# View the forest results.
plot(output.forest)
print(output.forest) 
predictionCart <- predict(output.forest, newdata=datatest)
t1 <- table(datatest$detect, predictionCart)
(t1[1]+t1[4])/(nrow(test))
## roc
optCutOff<-0.8
misClassError(datatest$detect, predictionCart, threshold = optCutOff)
plotROC(datatest$detect, predictionCart)
## confusion matrix
Concordance(datatest$detect, predictionCart)
sensitivity(datatest$detect, predictionCart, threshold =optCutOff)
specificity(datatest$detect, predictionCart, threshold = optCutOff)
confusionMatrix(datatest$detect, predictionCart, threshold = optCutOff)
