#Load libraries
library(ggplot2)
library(broom)
library(dplyr)

#Import dataset
file_path = "./assets/ns-arranged-data.csv"
dataset <- read.csv(file_path, header = T)
head(dataset)

mode <- c("Live classes (ie: Zoom, Google Meet etc.)", "Recorded Lectures", "Uploaded or emailed Materials", "Discussion forums/chats")
specie <- c(rep(mode[1],3), rep(mode[2],3), rep(mode[3],3), rep(mode[4],3))

questions <- rep(c("Motivated.online", "Distracted.online", "Actions.online"), 4)

#Motivated online
q1 <- c(mean(dataset[dataset$Current.online.courses == mode[1], 8]),
mean(dataset[dataset$Current.online.courses == mode[2], 8]),
mean(dataset[dataset$Current.online.courses == mode[3], 8]),
mean(dataset[dataset$Current.online.courses == mode[4], 8]))

#Distracted onlin
q2 <- c(mean(dataset[dataset$Current.online.courses == mode[1], 8]),
             mean(dataset[dataset$Current.online.courses == mode[2], 8]),
                  mean(dataset[dataset$Current.online.courses == mode[3], 8]),
                       mean(dataset[dataset$Current.online.courses == mode[4], 8]))

#Actions online
q3 <- c(mean(dataset[dataset$Current.online.courses == mode[1], 9]),
             mean(dataset[dataset$Current.online.courses == mode[2], 9]),
                  mean(dataset[dataset$Current.online.courses == mode[3], 9]),
                       mean(dataset[dataset$Current.online.courses == mode[4], 9]))

value <- c(q1, q2, q3)

data <- data.frame(specie,questions,value)
ggplot(data, aes(fill=questions, y=value, x=specie)) + 
  geom_bar(position="dodge", stat="identity")



