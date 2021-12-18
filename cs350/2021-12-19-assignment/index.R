# My function
source('/Users/seito/Documents/develop/minerva/cs350/2021-12-19-assignment/function.R')
# The pakcage library for ggplot2, dplyr, ... etc
library(tidyverse)

# Import data
file_path = "/Users/seito/Documents/develop/minerva/cs350/2021-12-19-assignment/assets/degrees-that-pay-back.csv"
df1 <- read.csv(file_path, header = T)

# Set data lists
um <- df1[,1]
sms <- dollerToInt(df1[,2], sms)
mcms <- dollerToInt(df1[,3], sms)

#Calculate total income for 30 years
total_income <- (sms * 10 + mcms * 20)

#Integrate samplings from the population with Central Limit Theorem
nsims = 10000
sample_size <- 10
samplings <- c()
for(i in 1:nsims){
  samplings <- c(samplings, mean(sample(total_income, sample_size, replace=TRUE)))
}

#Draw the chart
hist(samplings, col = "lightblue", main = "Income for 30 years", xlab = "$")