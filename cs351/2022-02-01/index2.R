library(ggplot2)
conrowdat <- read.csv("ClassifyDirtyData_Conrow.csv", sep = ",")
conrowdat
#To further investigate how this data may look, I apply a color to the biary data and create a random line on the plane as the target function
X1 <- conrowdat[1]
X2 <- conrowdat[2]
Y0 <- conrowdat[3]