#slop-categorical-multiple-regression
#年齢によって内定数はかわるか？
library(ggplot2)
library(broom)
library(dplyr)

file_path = "./assets/engineer-career-data-arranged3.csv"
dataset <- read.csv(file_path, header = T)
head(dataset)
attach(dataset)

result_all <- glm(formula = job.offer ~ ., data = dataset, family="binomial")
summary(result_all)

#Coefficients:
#Estimate Std. Error z value Pr(>|z|)  
#(Intercept)      1.892e+03  8.650e+02   2.188   0.0287 *
#  age             -4.544e-02  5.233e-02  -0.868   0.3852  
#entry            2.302e-02  1.618e-02   1.423   0.1547  
#final.education  4.189e-01  4.969e-01   0.843   0.3992  
#year            -9.354e-01  4.281e-01  -2.185   0.0289 *
#  study.load       1.436e-03  6.891e-04   2.085   0.0371 *

#> study.load
result <- glm(formula = job.offer ~ study.load, data = dataset, family="binomial")
summary(result)

cor(dataset$job.offer, dataset$study.load)

plot_view <- ggplot(dataset,aes(x=study.load, y=job.offer)) +
  geom_point() + 
  geom_smooth(method = "glm", method.args= list(family="binomial"))
plot_view

exp(result$coefficients)
exp(confint(result, level = 0.95))

#Odds
#(Intercept)  study.load 
#4.602419    1.001327 

#Confint
#               2.5 %   97.5 %
#(Intercept) 2.429224 9.359579
#study.load  1.000064 1.002749
