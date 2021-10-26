#LAND ICE MASS DATA ,,
#Data from JPL RL05p1M (SS48z-CRI),,
#Greenland GIA correction: A et al.,,
#Antarctica GIA correction: Ivins et al.,,
#Values are anomalies relative to timeseries mean,,
#"Provided by David Wiese, JPL",,http://climate.nasa.gov/vital-signs/land-ice/
#  ,,
#Greenland Trend (2002 - 2014): -287 Gt/yr,,
#Antarctica Trend (2002 - 2014): -134 Gt/yr,,
#,,

library(ggplot2)
file_path = "./assets/s1-student-version--ea-assignment--climate-change--land-ice-mass-data---sheet1.csv"
data <- read.csv(file_path)

data$time = data[, 1] #time column can be accessed with "time"
data$greenland = data[, 2] #greenland column
data$antarctica = data[, 3] #antarctica column

plot_base <- ggplot(data, aes(x=time, y=antarctica)) + labs(x = "Year", y= "Antarctica ice mass anomaly", caption = "Figure 1.  Annual land ice mass anomalies in Antarctica from 2002 until 2014")

# draw
plot_base + geom_line(aes(x=time, y=antarctica), color="blue") + geom_line(aes(x=time, y=greenland), color="green")

