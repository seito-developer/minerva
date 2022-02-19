#Load libraries
library(ggplot2)
library(broom)
library(dplyr)

#Import dataset
file_path = "./assets/ns-arranged-data.csv"
dataset <- read.csv(file_path, header = T)
head(dataset)

courses <-c("In-person Courses", "Online Courses")

###
## Convert values "In-person Courses/Online Courses" in Preferred course to numbers 1/0
## And add the new row "Expected.online"
###
row_num <- nrow(dataset)
i <- 1
expected_online_vals <- c()
while (i < row_num + 1) {
  if(dataset$Preferred.course[i] == courses[1]) {
    expected_online_vals <- c(expected_online_vals, 1)
  } else {
    expected_online_vals <- c(expected_online_vals, 0)
  }
  i <- i+1
}

new_col <- data.frame(expected_online_vals)
names(new_col) <- c("Expected.online")
dataset <- cbind(dataset, new_col)
head(dataset)

# remake data set from the original without unnecessary rows
new_dataset <-c(dataset[3],dataset[4],dataset[5],dataset[6],dataset[7],dataset[8],dataset[9],dataset[10],dataset[11],dataset[12],dataset[13],dataset[14],dataset[15],dataset[16], dataset[18])
results<- glm(formula = new_dataset$Expected.online ~ new_dataset$Enjoy.online + new_dataset$Motivated.online + new_dataset$Enjoy.offline, data = new_dataset, family="binomial")
summary(results)

#Coefficients:
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                    2.7799     1.1384   2.442   0.0146 *  
#  new_dataset$Enjoy.online      -1.5446     0.2488  -6.208 5.36e-10 ***
#  new_dataset$Motivated.online  -0.3581     0.1803  -1.987   0.0469 *  
#  new_dataset$Enjoy.offline      1.2613     0.2251   5.605 2.09e-08 ***
#Null deviance: 429.33  on 462  degrees of freedom
#Residual deviance: 260.85  on 459  degrees of freedom
#AIC: 268.85