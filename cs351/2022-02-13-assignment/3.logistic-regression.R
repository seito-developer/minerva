#slop-categorical-multiple-regression
#年齢によって内定数はかわるか？
library(ggplot2)
library(broom)
library(dplyr)

file_path = "./assets/engineer-career-data-arranged3.csv"
dataset <- read.csv(file_path, header = T)
head(dataset)
attach(dataset)

result <- glm(formula = job.offer ~ ., data = dataset, family="binomial")
summary(result)
cor(dataset$job.offer, dataset$entry)
cor(dataset$job.offer, dataset$study.load)
cor(dataset$job.offer, dataset$year)
cor(dataset$job.offer, dataset$age)
cor(dataset$job.offer, dataset$final.education)
#Coefficients:
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      4.165e+03  9.633e+02   4.323 1.54e-05 ***
#age             -2.283e-02  5.658e-02  -0.404    0.687    
#entry            2.346e-02  1.898e-02   1.236    0.216    
#final.education  9.010e-02  5.765e-01   0.156    0.876    
#year            -2.060e+00  4.766e-01  -4.322 1.55e-05 ***
#study.load       1.504e-03  7.091e-04   2.120    0.034 *   
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 139.529  on 203  degrees of freedom
#Residual deviance:  97.983  on 199  degrees of freedom
#AIC: 109.96

#
#Number of Fisher Scoring iterations: 7

#> cor(dataset$job.offer, dataset$entry)
#[1] 0.1129729
#> cor(dataset$job.offer, dataset$study.load)
#[1] 0.1401916
# cor(dataset$job.offer, dataset$year)
#[1] -0.2658596
#> cor(dataset$job.offer, dataset$age)
#1] -0.06231733
#> cor(dataset$job.offer, dataset$final.education)
#[1] 0.07013675

exp(result$coefficients)

#(Intercept)       entry  study.load        year         age 
#Inf   1.0236671   1.0015070   0.1259857   0.9745030 

dataset_strict <- read.csv("./assets/engineer-career-data-arranged4.csv", header = T)
result_strict_all <- glm(formula = job.offer ~ ., data = dataset_strict, family="binomial")
summary(result_strict_all)
#result_strict <- glm(formula = job.offer ~ entry, data = dataset_strict, family="binomial")
summary(result_strict)

#plot
#job.offer ~ entry
ggplot(dataset_strict,aes(x=entry, y=job.offer)) +
  geom_point() + 
  geom_smooth(method = "glm", method.args= list(family="binomial"))

exp(result_strict$coefficients)
