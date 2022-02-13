#slop-categorical-multiple-regression
library(ggplot2)
library(broom)
library(dplyr)

file_path = "./assets/engineer-career-data-arranged.csv"
dataset <- read.csv(file_path, header = T)
head(dataset)
attach(dataset)

#plot
plot_dataset <- data.frame(job.offer = dataset$job.offer, entry = dataset$entry)
result <- lm(formula = job.offer ~ entry, data=plot_dataset)
summary(result)
plot(result)
abline(result, lwd=3)
#cor(dataset$job.offer, dataset$entry)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 1.421168   0.122332   11.62  < 2e-16 ***
#  entry       0.013927   0.004259    3.27  0.00126 ** 
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 1.289 on 202 degrees of freedom
#Multiple R-squared:  0.05028,	Adjusted R-squared:  0.04558 
#F-statistic: 10.69 on 1 and 202 DF,  p-value: 0.001263
#相関係数 0.224234

dataset_20 <- dataset[dataset$age<=29,]
plot_dataset_20 <- data.frame(job.offer = dataset_20$job.offer, entry = dataset_20$entry)
result <- lm(formula = job.offer ~ entry, data=plot_dataset_20)
summary(result)
cor(dataset_20$job.offer, dataset_20$entry)
plot(result)
abline(result, lwd=3)

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 1.482056   0.150951   9.818  < 2e-16 ***
#  entry       0.015576   0.005189   3.002  0.00314 ** 
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 1.367 on 150 degrees of freedom
#Multiple R-squared:  0.05667,	Adjusted R-squared:  0.05038 
#F-statistic: 9.012 on 1 and 150 DF,  p-value: 0.003144
#相関係数 0.2380589


# predict
new_data <- data.frame(
  job.offer = 1,
  entry = 30
)
new_data
predict(result, new_data, level = 0.95, interval="confidence")

