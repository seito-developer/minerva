#Load libraries
library(ggplot2)
library(broom)
library(dplyr)

#Import dataset
file_path = "./assets/engineer-career-data-arranged.csv"
dataset <- read.csv(file_path, header = T)
head(dataset)
attach(dataset)

#multiple linear regression
result <- lm(formula = job.offer ~ ., data=dataset)
summary(result)


#single linear regression
result <- lm(formula = job.offer ~ entry, data=dataset)
summary(result)

#plot
plot(result)
abline(result, lwd=3)

#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)      2.407e+02  1.302e+02   1.849 0.065966 .  
#age             -2.541e-02  2.199e-02  -1.156 0.249152    
#entry            1.498e-02  4.358e-03   3.436 0.000719 ***
#  final.education  1.353e-01  1.910e-01   0.709 0.479399    
#year            -1.181e-01  6.446e-02  -1.832 0.068380 .  
#study.load      -2.843e-05  2.469e-04  -0.115 0.908446    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 1.283 on 198 degrees of freedom
#Multiple R-squared:  0.0787,	Adjusted R-squared:  0.05543 
#F-statistic: 3.383 on 5 and 198 DF,  p-value: 0.005909