# My function
source('/Users/seito/Documents/develop/minerva/cs350/2021-12-19-assignment/function.R')
# The pakcage library for ggplot2, dplyr, ... etc
library(tidyverse)

# Import data
file_path = "/Users/seito/Documents/develop/minerva/cs350/2021-12-19-assignment/assets/degrees-that-pay-back.csv"
df1 <- read.csv(file_path, header = T)

# Starting Median Salary * 10 years
um <- df1[,1]
sms <- dollerToInt(df1[,2], sms)
mcms <- dollerToInt(df1[,3], sms)

#Lifetime income
total_income <- (sms * 10 + mcms * 20)

#table <- data.frame(um, li)
#print(table)
hist(total_income, col = "lightblue", main = "Income for 30 years", xlab = "$")

samplings <- c()
for(i in 1:100){
  samplings <- c(samplings, mean(sample(total_income, 10)))
}
print(samplings)

#sd(li)
#mean(li)
#print()

#dnorm( x, mean=m, sd=n )
