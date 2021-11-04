# Initial setup
library(ggplot2) # the library for plotting various types of charts
file_path = "./dataset.csv"
data <- read.csv(file_path, header = T)

#Extract the column of the study workload
study_workload <- data[,8]

#mean
mean(study_workload)
#median
median(study_workload)
#mode
names(which.max(table(study_workload)))
#range
range(study_workload)
#standard deviation
sd(study_workload)

#Plot the histogram
hist(study_workload, col = "lightblue", main = "The studiy workload of Japanese business persons per week", xlab = "Time (mins)")

