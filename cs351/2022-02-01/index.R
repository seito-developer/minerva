setwd("~/Documents/develop/minerva/cs351/2022-02-01/")
mydata <- read.csv("dividendinfo.csv")

attach(mydata)

scaleddata<-scale(mydata)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

maxmindf <- as.data.frame(lapply(mydata, normalize))

# Training and Test Data
trainset <- maxmindf[1:160, ]
testset <- maxmindf[161:200, ]

#Neural Network
library(neuralnet)
nn <- neuralnet(dividend ~ fcfps + earnings_growth + de + mcap + current_ratio, data=trainset, hidden=c(2,1), linear.output=FALSE, threshold=0.01)
nn$result.matrix
plot(nn)