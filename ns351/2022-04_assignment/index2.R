library("dplyr")
library("tidyr")
library("ggplot2")
library("stringr")

file_path = "./dataset.csv"
dataset <- read.csv(file_path, header = T)
head(dataset)

eval_data <- c("strong_disagree", "disagree", "neigther", "agree", "strong_agree")


#> ggplot(ChickWeight, aes(x = Time, y = weight, fill = Diet)) + geom_bar(stat = "identity")
