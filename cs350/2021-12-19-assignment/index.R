# Initial setup
library(tidyverse) # the pakcage library for ggplot2, dplyr, ... etc

#library("stringr") #
#file_path = "./data/responses.csv"
file_path = "/Users/seito/Documents/develop/minerva/cs350/2021-12-19-assignment/assets/degrees-that-pay-back.csv"
df1 <- read.csv(file_path, header = T)
sararies <- c()
len <- length(df1[,3])
for (val in df1) {
  #sararies <- as.integer(str_sub(val, start = 2, end = -1)) * 10
  target <- str_sub(val, start = 2, end = -1)
  num <- str_replace(target, ',', '')
  sararies <- as.integer(num)
}

sararies

