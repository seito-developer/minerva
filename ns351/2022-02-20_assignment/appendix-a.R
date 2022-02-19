#Load libraries
library(ggplot2)
library(broom)
library(dplyr)

#Import dataset
file_path = "./assets/ns-arranged-data1.csv"
dataset <- read.csv(file_path, header = T)
head(dataset)

questions <- c('What.is.the.primary.mode.of.teaching.in.your.online.courses.', 'What.is.your.preferred.mode.of.teaching.for.online.courses.')
questions_len <- length(questions)

teaching_mode <- c(rep('Discussion forums/chats',4), rep('Live classes (ie: Zoom, Google Meet etc.)',4), rep('Recorded Lectures/Videos',4), rep('Uploaded or emailed Materials',4))

condition <- rep(c(questions[1] , questions[2]))

q1 <- c(dataset[1,questions[1]], dataset[2,questions[1]], dataset[3,questions[1]], dataset[4,questions[1]])
q2 <- c(dataset[1,questions[2]], dataset[2,questions[2]], dataset[3,questions[2]], dataset[4,questions[2]])
value <- c(q1, q2)

data <- data.frame(teaching_mode,condition,value)

ggplot(data, aes(fill=condition, y=value, x=teaching_mode)) + 
  geom_bar(position="dodge", stat="identity")


specie <- c()
i <- 1
for (teaching_mode_item in teaching_mode) {
  specie[i] <- c(rep(teaching_mode_item , 2))
  i <- i+1;
}



value <- abs(rnorm(12 , 0 , 15))

data <- data.frame(specie,condition,value)

index_q1 <- 1
foo <- c()
for (teaching_mode_item in teaching_mode) {
  foo <- c(foo, length(dataset[dataset$items[1] == teaching_mode[index_q1],]))
  #print(teaching_mode[index_q1])
  index_q1 <- index_q1+1;
}
foo

length(dataset[items[1] == teaching_mode[2],])

length(dataset[dataset[3,] == "Discussion forums/chats",])
teaching_mode[4]
