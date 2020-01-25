##load package
library(corrplot)
#Read the data set
data<-read.csv("creditcard.csv",header = T)
#correlation matrix
correlation <-cor(data,method = "pearson")
corrplot(correlation, number.cex = .9,method = "circle",type = "full",tl.cex=0.8,tl.col = "black")


