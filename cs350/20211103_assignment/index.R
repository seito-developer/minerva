# Load initial files
library(ggplot2)
file_path = "./data/dist/dataset.csv"
data <- read.csv(file_path, na.strings = c("", "NA"))

# Variables
data$enough_time = data[, 18] #You think you do enough with your time.
data$plan = data[, 14] #You will plan your activities from day to day.
data$hw = data[, 21] #You know how much time you spend on each of the homework I do.

data_x <- "you think you do enough with your time."
#data_y <- "You will plan your activities from day to day."
data_y <- "You know how much time you spend on each of the homework I do."

plot_base <- ggplot(data, aes(x=enough_time, y=hw))
plot_with_label <- plot_base + labs(x = data_x, y= data_y)

# draw
plot_with_label + geom_point()

