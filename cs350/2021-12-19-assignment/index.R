# My function
source('/Users/seito/Documents/develop/minerva/cs350/2021-12-19-assignment/function.R')
# The pakcage library for ggplot2, dplyr, ... etc
library(tidyverse)

# Import data
file_path = "/Users/seito/Documents/develop/minerva/cs350/2021-12-19-assignment/assets/degrees-that-pay-back.csv"
df1 <- read.csv(file_path, header = T)

dataset <- df1[,3]
sararies <- c()
len <- length(dataset)
for (i in 1:len) {
  sararies <- c(sararies, dollerToInt(dataset[i]))
}

print(sararies)

