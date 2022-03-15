#Load libraries
library(ggplot2)
library(broom)
library(dplyr)

#Import dataset
file_path = "./assets/ns-arranged-data.csv"
dataset <- read.csv(file_path, header = T)
head(dataset)

mode <- c("Live classes (ie: Zoom, Google Meet etc.)", "Recorded Lectures", "Uploaded or emailed Materials", "Discussion forums/chats")
specie <- c(rep(mode[1],4), rep(mode[2],4), rep(mode[3],4), rep(mode[4],4))

questions <- rep(c("Motivated.online", "Distracted.online", "Actions.online", "Engaging online"), 4)

#Motivated online
q1 <- c(mean(dataset[dataset$Current.online.courses == mode[1], 6]),
        mean(dataset[dataset$Current.online.courses == mode[2], 6]),
        mean(dataset[dataset$Current.online.courses == mode[3], 6]),
        mean(dataset[dataset$Current.online.courses == mode[4], 6]))

#Engaging online
q2 <- c(mean(dataset[dataset$Current.online.courses == mode[1], 8]),
        mean(dataset[dataset$Current.online.courses == mode[2], 8]),
        mean(dataset[dataset$Current.online.courses == mode[3], 8]),
        mean(dataset[dataset$Current.online.courses == mode[4], 8]))

#Distracted online
q3 <- c(mean(dataset[dataset$Current.online.courses == mode[1], 9]),
             mean(dataset[dataset$Current.online.courses == mode[2], 9]),
                  mean(dataset[dataset$Current.online.courses == mode[3], 9]),
                       mean(dataset[dataset$Current.online.courses == mode[4], 9]))
dataset[dataset$Current.online.courses == mode[1], 10]
#Actions online
q4 <- c(mean(dataset[dataset$Current.online.courses == mode[1], 10]),
             mean(dataset[dataset$Current.online.courses == mode[2], 10]),
                  mean(dataset[dataset$Current.online.courses == mode[3], 10]),
                       mean(dataset[dataset$Current.online.courses == mode[4], 10]))

value <- c(q1, q2, q3, q4)

data <- data.frame(specie,questions,value)
ggplot(data, aes(fill=questions, y=value, x=specie)) + 
  geom_bar(position="dodge", stat="identity")




