#libraray needed
library(smbinning)
library(InformationValue)
#read data
data<-read.csv("creditcard.csv",header = T)
table(data$Class)
##split data
samplesize = 0.70 * nrow(data)
set.seed(80)
index = sample( seq_len ( nrow ( data ) ), size = samplesize )
datatrain = data[ index, ]
datatest = data[ -index, ]
# Making data continuous
continuous_vars <- c("V6", "V1","V2", "V3", "V4", "V5","V7","V8","V9","V10","V11","V12","V13","V14","V15","V16","V17","V18","V19","V20","V24","V25","V26","V27","V28","Amount","Time")
continuous_vars
iv_df <- data.frame(VARS=continuous_vars, IV=numeric(27))  # init for IV results
for(continuous_var in continuous_vars){
  smb <- smbinning(datatrain, y="Class", x=continuous_var)  # WOE table
  if(class(smb) != "character"){  # any error while calculating scores.
    iv_df[iv_df$VARS == continuous_var, "IV"] <- smb$iv
  }
}
iv_df <- iv_df[order(-iv_df$IV), ]  # sort
iv_df
#logistic regression
logitMod <- glm(Class~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24+V25+V26+V27+V28+Amount+Time, data=datatrain, family=binomial(link="logit"))
predicted <- plogis(predict(logitMod, datatest))
summary(logitMod)
##ROC
optCutOff <-optimalCutoff(datatest$Class, predicted)
optCutOff<-0.7
misClassError(datatest$Class, predicted, threshold = optCutOff)
plotROC(datatest$Class, predicted)
## Confusion matrix
Concordance(datatest$Class, predicted)
sensitivity(datatest$Class, predicted, threshold =0.5)
specificity(datatest$Class, predicted, threshold = optCutOff)
confusionMatrix(datatest$Class, predicted, threshold = optCutOff)

