#Load libraries
library(ggplot2)
library(broom)
library(dplyr)

#Import dataset
file_path = "./assets/engineer-career-data-arranged2.csv"
dataset <- read.csv(file_path, header = T)
head(dataset)
attach(dataset)

#Logistic regression with all variables
result_all <- glm(formula = job.offer ~ ., data = dataset, family="binomial")
summary(result_all)

year <- year - 2000

result_all <- glm(formula = job.offer ~ entry + study.load + final.education + year, data = dataset, family="binomial")
summary(result_all)

result_a <- glm(formula = job.offer ~ study.load, data = dataset, family="binomial")
summary(result_a)

result_b <- glm(formula = job.offer ~ year, data = dataset, family="binomial")
summary(result_b)

result_sy <- glm(formula = job.offer ~ study.load + year, data = dataset, family="binomial")
summary(result_sy)

#plot
plot_view <- ggplot(dataset,aes(x=year, y=job.offer)) +
  geom_point() + 
  geom_smooth(method = "glm", method.args= list(family="binomial"))
plot_view

#Odds and Confidence interval
exp(result$coefficients)
exp(confint(result, level = 0.95))

test_data <- data.frame(study.load = 10)
predict(result_a, test_data, type="response") 

