#Load libraries
library(rpart)
library(rpart.plot)
library(partykit)

#Import dataset
file_path = "./assets/engineer-career-data-arranged3.csv"
dataset <- read.csv(file_path, header = T)
head(dataset)
attach(dataset)

#Generate tree
tree <- rpart(job.offer ~ entry + age + study.load + year, data=dataset2, method = "class")
summary(tree)
rpart.plot(tree2)
printcp(tree2)
plotcp(tree2)