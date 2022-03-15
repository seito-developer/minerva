#Load libraries
library(ggplot2)
library(broom)
library(dplyr)

#Import dataset
file_path = "./assets/ns-arranged-data.csv"
dataset <- read.csv(file_path, header = T)
head(dataset)

# items from the dataset
courses <-c("In-person Courses", "Online Courses")
mode <- c("Live classes (ie: Zoom, Google Meet etc.)", "Recorded Lectures/Videos", "Recorded Lectures", "Uploaded or emailed Materials", "Discussion forums/chats")

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
result_all <- glm(formula = new_dataset$Expected.online ~ . , data = new_dataset, family="binomial")
summary(result_all)
