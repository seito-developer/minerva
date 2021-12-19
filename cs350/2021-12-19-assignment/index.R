# The pakcage library for ggplot2, dplyr, ... etc
library(tidyverse)

# My function
dollerToInt <- function(dataset, data_list){
  data_list <- c()
  len <- length(dataset)
  for (i in 1:len) {
    complete.cases(dataset[i])
    target <- str_sub(dataset[i], start = 2, end = -1)
    num <- str_replace(target, ',', '')
    data_list <- c(data_list, as.integer(num))
  }
  return(data_list)
}

# Import data
file_path = "/Users/seito/Documents/develop/minerva/cs350/2021-12-19-assignment/assets/salaries-by-college-type.csv"
df <- read.csv(file_path, header = T)

# Set data lists
um <- df[,1]
st <- df[,2]
sms <- dollerToInt(df[,3], sms)
mcms <- dollerToInt(df[,4], mcms)

#Calculate total income for 30 years(Newly granted × 10 years + Midway granted × 20 years)
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

values.mean <- mean(samplings)
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
  if(schools_total_income_dataset[i, 1] == "Engineering"){
    il_dataset <- c(il_dataset, schools_total_income_dataset[i, 2])
  }
  # print(la_dataset[c(i), ])
}
print(il_dataset)

# variables for students in engineering
sumilation_size <- 100
z_value <- 2.33 ## Z value of top 1%

# the simulation if a student in engineering can be in top 1%
simulate_rate_of_total_income <- function(){
  result_storage <- c()
  for (i in 1:sumilation_size){
    extracted_il_sample <- sample(il_dataset, 1)
    #Identify standardized Z-value
    #(x - μ)/σ/√n >= 2.33
    print( (extracted_il_sample - values.mean)/(values.sd/sqrt(nsims)) )
    if((extracted_il_sample - values.mean)/(values.sd/sqrt(nsims)) >= z_value){
      result_storage <- c(result_storage, 1)
    } else {
      result_storage <- c(result_storage, 0)
    }
  }
  return(result_storage)
}

#Get the probability with 95% of confidence interval
total_result_storage <- c()
confidence_interval <- 1 - 0.05
for (i in 1:sumilation_size){
  if(confidence_interval < sum(simulate_rate_of_total_income())/sumilation_size){
    total_result_storage <- c(total_result_storage, 1)
  } else {
    total_result_storage <- c(total_result_storage, 0)
  }
}
print(total_result_storage)
print(sum(total_result_storage)/sumilation_size)
