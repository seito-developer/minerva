library("dplyr")
library("tidyr")
library("ggplot2")
library("stringr")

file_path = "./dataset.csv"
dataset <- read.csv(file_path, header = T)
head(dataset)
