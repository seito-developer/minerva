# My function
source('/Users/seito/Documents/develop/minerva/cs350/2021-12-19-assignment/function.R')
# The pakcage library for ggplot2, dplyr, ... etc
library(tidyverse)

# Import data
file_path = "/Users/seito/Documents/develop/minerva/cs350/2021-12-19-assignment/assets/salaries-by-college-type.csv"
df <- read.csv(file_path, header = T)

# Set data lists
um <- df[,1]
st <- df[,2]
sms <- dollerToInt(df[,3], sms)
mcms <- dollerToInt(df[,4], mcms)

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
hist(samplings, col = "lightblue", main = "Income for 30 years", xlab = "Dollar")

values.mean <- mean(total_income)
values.range <- range(total_income)
values.sd <- sd(samplings)
cat("mean:", values.mean)
cat("sd", values.sd)
cat("range", values.range)

#Total income in each of schools
schools_total_income_dataset <- data.frame(st, total_income)
row_len <- nrow(schools_total_income_dataset)

#Make the dataset of the total income in Ivy League
il_dataset <- c()
for (i in 1:row_len) {
  if(schools_total_income_dataset[i, 1] == "Ivy League"){
    il_dataset <- c(il_dataset, schools_total_income_dataset[i, 2])
  }
  # print(la_dataset[c(i), ])
}
print(il_dataset)

sample(il_dataset, 1)