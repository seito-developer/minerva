#job offer ... over 2 ... 1 & under 2 ... 0
#final education ... "大学中退以上" ... 1

library(rpart)
library(rpart.plot)
library(partykit)

file_path = "./assets/engineer-career-data-arranged4.csv"
dataset <- read.csv(file_path, header = T)
head(dataset)
attach(dataset)

summary(dataset)

#entry + final.education + age + study.load + year
# removed age
tree <- rpart(job.offer ~ final.education + study.load + year, data=dataset, method = "class")
summary(tree)

rpart.plot(tree)
printcp(tree)
plotcp(tree)

#cp=0.048 so let's prune
tree.cp <- prune(tree,cp=0.048)
rpart.plot(tree.cp)
plotcp(tree)

#Result ... the success rate depends on year


#RMSEは？