# Initial setup
library(ggplot2) # the library for plotting various types of charts
#file_path = "./data/responses.csv"
file_path = "/Users/seito/Documents/develop/minerva/cs350/2021-12-09/data/responses.csv"
data <- read.csv(file_path, header = T)

#Extract the column of the study workload
music_col <- data[,1]

#all
music_data <- na.omit(music_col)

#mean
mean(music_data)
#median
median(music_data)
#mode
names(which.max(table(music_data)))
#range
range(music_data)
#standard deviation
sd(music_data)

#Plot the histogram
hist(music_data, col = "lightblue", main = "music", xlab = "Score")

