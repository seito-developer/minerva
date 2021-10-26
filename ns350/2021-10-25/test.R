# Load initial files
library(ggplot2)
file_path = "./assets/s1-student-version--ea-assignment--climate-change--land-ice-mass-data---sheet1.csv"
data <- read.csv(file_path)

# Variables
data$time = data[, 1] #time column can be accessed with "time"
data$greenland = data[, 2] #greenland column
data$antarctica = data[, 3] #antarctica column

plot_base <- ggplot(data, aes(x=time, y=antarctica)) + labs(x = "Year", y= "Antarctica ice mass anomaly", caption = "Figure 1.  Annual land ice mass anomalies in Antarctica from 2002 until 2014")

# draw
plot_base + geom_line(aes(x=time, y=antarctica), color="blue") + geom_line(aes(x=time, y=greenland), color="green")

