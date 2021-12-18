# Initial setup
library(tidyverse) # the pakcage library for ggplot2, dplyr, ... etc
source('/Users/seito/Documents/develop/minerva/cs350/2021-12-19-assignment/function.R')
#library("stringr") #
#file_path = "./data/responses.csv"
file_path = "/Users/seito/Documents/develop/minerva/cs350/2021-12-19-assignment/assets/degrees-that-pay-back.csv"
df1 <- read.csv(file_path, header = T)
dataset <- df1[,3]
sararies <- c()
len <- length(dataset)
for (i in 1:len) {
  #sararies <- as.integer(str_sub(val, start = 2, end = -1)) * 10
  sararies <- c(sararies, dollerToInt(dataset[i]))
  #print(dataset[i])
}

print(sararies)

