#Load libraries
library(ggplot2)
library(broom)
library(dplyr)

#Import dataset
file_path = "./assets/ns-arranged-data.csv"
dataset <- read.csv(file_path, header = T)
head(dataset)

###
## Convert values "In-person Courses/Online Courses" in Preferred course to numbers 1/0
## And add the new row "Expected.online"
###
courses <-c("In-person Courses", "Online Courses")
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


###
## And add row "synchronous"
###
mode <- c("Live classes (ie: Zoom, Google Meet etc.)", "Recorded Lectures", "Uploaded or emailed Materials", "Discussion forums/chats")
i <- 1
synchronous_vals <- c()
while (i < row_num + 1) {
  if(dataset$Current.online.courses[i] == mode[1]) {
    synchronous_vals <- c(synchronous_vals, 1)
  } else if(dataset$Current.online.courses[i] == mode[2]) {
    synchronous_vals <- c(synchronous_vals, 0)
  } else if(dataset$Current.online.courses[i] == mode[3]) {
    synchronous_vals <- c(synchronous_vals, 0)
  } else if(dataset$Current.online.courses[i] == mode[4]) {
    synchronous_vals <- c(synchronous_vals, 0)
  }
  i <- i+1
}

new_col2 <- data.frame(synchronous_vals)
names(new_col2) <- c("Synchronous")
dataset <- cbind(dataset, new_col2)

dataset$Synchronous

# remake data set from the original without unnecessary rows
new_dataset <-c(dataset[3],dataset[4],dataset[5],dataset[6],dataset[7],dataset[8],dataset[9],dataset[10],dataset[11],dataset[12],dataset[13],dataset[14],dataset[15],dataset[16], dataset[18],dataset[19])
results<- lm(formula = new_dataset$Synchronous ~ ., data = new_dataset)
summary(results)

results2 <- lm(formula = new_dataset$Motivated.online ~ ., data = new_dataset)
summary(results2)

results3 <- lm(formula = new_dataset$Expected.online ~ new_dataset$Motivated.online + new_dataset$Synchronous, data = new_dataset)
summary(results3)
