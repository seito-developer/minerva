#slop-categorical-multiple-regression
#年齢によって内定数はかわるか？
library(ggplot2)
library(broom)
library(dplyr)

file_path = "./assets/engineer-career-data-arranged.csv"
dataset <- read.csv(file_path, header = T)
head(dataset)
attach(dataset)

result <- lm(formula = job.offer ~ entry + age, data=dataset)
summary(result)

#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  2.280887   0.480742   4.745 3.96e-06 ***
#  entry        0.013690   0.004235   3.232  0.00144 ** 
#  age         -0.037840   0.020472  -1.848  0.06601 .  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 1.282 on 201 degrees of freedom
#Multiple R-squared:  0.06615,	Adjusted R-squared:  0.05686 
#F-statistic:  7.12 on 2 and 201 DF,  p-value: 0.001029

