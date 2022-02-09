install.packages("MASS")
install.packages("Matching")
install.packages("tree")
library(MASS)
library(Matching)
library(tree)

library(ISLR)

rm(Carseats)
attach(Carseats)

# No/Yes = 0/1
##High=ifelse(Sales <=8,"No","Yes ")
High=ifelse(Sales <= 8,0,1)
Carseats = data.frame(Carseats, High)
Carseats
tree.carseats = tree(High~.-Sales, Carseats)

summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats,pretty =0)
tree.carseats

set.seed(2)
train=sample(1: nrow(Carseats), 200)
Carseats.test=Carseats[-train,]
High.test=High[-train]
tree.carseats = tree(High~.-Sales , Carseats ,subset=train)
tree.pred=predict(tree.carseats, Carseats.test, type="class")
table(tree.pred, High.test)
