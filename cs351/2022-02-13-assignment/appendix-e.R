#
# 意味なかった
#

#Load libraries
library(ggplot2)
library(broom)
library(dplyr)

#Import dataset
file_path = "./assets/engineer-career-data-arranged.csv"
dataset <- read.csv(file_path, header = T)
head(dataset)
attach(dataset)

#Regression with all variables
arranged_data <- dataset[job.offer < 4,]
result <- lm(formula = job.offer ~ ., data=arranged_data)
summary(result) # no effects

#Logistic regression
arranged_data$job.offer <- ifelse(arranged_data$job.offer > 0, 1, 0)
arranged_data

result_all <- glm(formula = job.offer ~ ., data = arranged_data, family="binomial")
summary(result_all)

result_a <- glm(formula = job.offer ~ study.load, data = arranged_data, family="binomial")
summary(result_a)


#plot
plot_view <- ggplot(arranged_data,aes(x=study.load, y=job.offer)) +
  geom_point() + 
  geom_smooth(method = "glm", method.args= list(family="binomial"))
plot_view

test_data <- data.frame(study.load = 500)
predict(result_a, test_data, type="response") 

