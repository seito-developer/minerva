#Load libraries
library(ggplot2)
library(broom)
library(dplyr)

#Import dataset
file_path = "./assets/engineer-career-data-arranged.csv"
dataset <- read.csv(file_path, header = T)
head(dataset)
attach(dataset)
dataset

new_data <- dataset[dataset$job.offer > 0,]

hist(new_data$age, main="Age distribution of job offers", xlab="Age", ylab="Number of people")
mean(new_data$age)
# 平均年齢：27.34歳

mean(new_data$entry)
# 平均エントリー数：20.21

sum(new_data$final.education)/nrow(new_data)
# 4年生大卒率：0.6538