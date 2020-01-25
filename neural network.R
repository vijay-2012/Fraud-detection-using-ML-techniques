#load packages
library(neuralnet)
library(ROCR)
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
#build model.
nn.sag<- neuralnet(detect ~ Time+V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24+V25+V26+V27+V28+Amount, data = train, hidden = 5, algorithm = "sag", err.fct = "sse", linear.output = FALSE)
plot(nn.sag)


