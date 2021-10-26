library(lubridate)

#Read data
csv_path <- "../s1-student-version--ea-assignment--climate-change--land-ice-mass-data---sheet1.csv"
data <- read.csv(csv_path, header = TRUE)

#Define data
data_time <- data[11:150, 1]
data_gm <- data[10:150, 2]
data_am <- data[10:150, 3]

#Plot graph
plot(data_time, data_gm, type="l", xaxt="s", yaxt="s", xlab="Time",ylab="Tempature", main="LAND ICE MASS DATA")
par(xaxt="s")
axis.Date(1,at=seq(min(data[,1]),max(data[,1]),"Time"),format="%y.%d")

#Add color & another line
points(data_time, data_gm, type="l", lwd=1, col="red")
points(data_time, data_am, type="l", lwd=1, col="blue")

#Put labels
legend("bottomleft",c("Greenland mass","Antarctica mass"),
       lwd=c(1,1),
       col=c("red","blue"),
       lty=c("solid","solid")
      )

