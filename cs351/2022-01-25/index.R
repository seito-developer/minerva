install.packages("MASS")
install.packages("Matching")
install.packages("tree")
library(MASS)
library(Matching)
library(tree)

#妊娠の数。
npreg <- Pima.tr[,1]

#経口ブドウ糖負荷試験における血漿ブドウ糖濃度。
glu <- Pima.tr[,2]

#拡張期血圧（mmHg）。
bp <- Pima.tr[,3]

#上腕三頭筋の皮膚のひだの厚さ
skin <- Pima.tr[,4]

#ボディマス指数
bmi <- Pima.tr[,5]

#糖尿病の血統機能。
ped <- Pima.tr[,6]

#age
age <- Pima.tr[,7]

#YesまたはNo、WHO基準に従った糖尿病患者の場合。
type <- Pima.tr[,8]

summary(Pima.tr)

#true_positive
true_positive <- Pima.tr[Pima.tr$glu>=200 & Pima.tr$type=="Yes",]
nrow(true_positive)

#false_positive
false_positive <- Pima.tr[Pima.tr$glu<140 & Pima.tr$type=="Yes",]
nrow(false_positive)

#true_negative
true_negative <- Pima.tr[Pima.tr$glu>=200 & Pima.tr$type=="No",]
nrow(true_negative)
  
#false_negative
false_negative <- Pima.tr[Pima.tr$glu<140 & Pima.tr$type=="No",]
nrow(false_negative)

#calculate the misclassification rate
(nrow(false_positive) + nrow(true_negative)) / (nrow(true_positive) + nrow(false_positive) + nrow(true_negative) + nrow(false_negative))


#######################
install.packages("MASS")
install.packages("Matching")
install.packages("tree")
library(MASS)
library(Matching)
library(tree)
library(ISLR)
Pima.tr

rm(Pima_data)
Pima_data <- Pima.tr

High=ifelse(Pima_data$glu <= 140,0,1)
Pima_data = data.frame(Pima_data, High)
Pima_data

tree.pima_data = tree(High~.-type, Pima_data)

summary(tree.pima_data)
plot(tree.pima_data)
text(tree.pima_data,pretty =0)
