#job offer ... over 2 ... 1 & under 2 ... 0
#final education ... "大学中退以上" ... 1

library(rpart)
library(rpart.plot)
library(partykit)

file_path = "./assets/engineer-career-data-arranged3.csv"
dataset <- read.csv(file_path, header = T)
head(dataset)
attach(dataset)

summary(dataset)

#entry + final.education + age + study.load + year
# removed age
tree <- rpart(job.offer ~ ., data=dataset, method = "class")
summary(tree)

rpart.plot(tree)
printcp(tree)
plotcp(tree)

###
#2nd
###

dataset2 <- read.csv("./assets/engineer-career-data-arranged4.csv", header = T)
tree2 <- rpart(job.offer ~ entry + age + study.load + year, data=dataset2, method = "class")
summary(tree2)

rpart.plot(tree2)
printcp(tree2)
plotcp(tree2)
#cp=0.048 so let's prune
tree.cp <- prune(tree,cp=0.048)
rpart.plot(tree.cp)
plotcp(tree)

#Result ... the success rate depends on year


#RMSEは？