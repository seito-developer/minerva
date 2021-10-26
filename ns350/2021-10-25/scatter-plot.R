# Load initial files
library(ggplot2)
file_path = "./assets/s1-student-version--ea-assignment--climate-change--land-ice-mass-data---sheet1.csv"
data <- read.csv(file_path)

# Variables
#data$time = data[, 1]
data$greenland = data[, 2]
data$antarctica = data[, 3]

plot_base <- ggplot(data, aes(x=greenland, y=antarctica)) + labs(x = "Ice mass anomaly in Greenland", y= "AntarcticaIce mass anomaly in Antarctica")

# draw
plot_base + geom_point()

