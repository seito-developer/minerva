#load library
install.packages("makedummies")
install.packages("ggplot2")
library(makedummies)
library("ggplot2")
library(tidyverse)
library(effsize)

#function for generating dummy data
generateDummyData <- function(asynchronous, synchronous, top_score){
  size <- 372
  mean_scores <- c(top_score * 0.75, top_score * 0.9, top_score)
  sd <- 7
  
  students_low <- data.frame(
    student_achive = 1,
    asynchronous = rep(asynchronous, size = size),
    synchronous = rep(synchronous, size = size),
    grade = rep(1:4, times = size/4, size = size),
    class_score_a = round(abs(rnorm(size, mean = mean_scores[1], sd = sd))),
    class_score_b = round(abs(rnorm(size, mean = mean_scores[1], sd = sd))),
    class_score_c = round(abs(rnorm(size, mean = mean_scores[1], sd = sd)))
  )
  
  students_mid <- data.frame(
    student_achive = 2,
    asynchronous = rep(asynchronous, size = size),
    synchronous = rep(synchronous, size = size),
    grade = rep(1:4, times = size/4, size = size),
    class_score_a = round(abs(rnorm(size, mean = mean_scores[2], sd = sd))),
    class_score_b = round(abs(rnorm(size, mean = mean_scores[2], sd = sd))),
    class_score_c = round(abs(rnorm(size, mean = mean_scores[2], sd = sd)))
  )
  
  students_high <- data.frame(
    student_achive = 3,
    asynchronous = rep(asynchronous, size = size),
    synchronous = rep(synchronous, size = size),
    grade = rep(1:4, times = size/4, size = size),
    class_score_a = round(abs(rnorm(size, mean = mean_scores[3], sd = sd))),
    class_score_b = round(abs(rnorm(size, mean = mean_scores[3], sd = sd))),
    class_score_c = round(abs(rnorm(size, mean = mean_scores[3], sd = sd)))
  )
  generated_data <- rbind(students_low, students_mid, students_high)
  
  return(generated_data)
}

# generate dummy data for each groups
test_sync <- generateDummyData(0, 1, 75)
test_async <- generateDummyData(1, 0, 70)
test_combined <- generateDummyData(1, 1, 85)

# function for showing t-test & cohen's D result
t.test.es <- function(x, y, t.paired = FALSE, es.ci = 0.95, es.paired = FALSE, rm = FALSE)
{
  t <- t.test(x, y, paired = t.paired)
  es <- effsize::cohen.d(x, y, conf.level = es.ci, na.rm = rm, paired = es.paired)
  return(list(t,es))
}


t.test.es(x = test_sync$class_score_a, y = test_combined$class_score_a)

# function for plot
plot_test <- function(title){
  h1 <- hist(test_sync$class_score_a)
  h2 <- hist(test_async$class_score_a)
  h3 <- hist(test_combined$class_score_a)
  
  plot(0, 0, type="n", xlim=c(0,100), ylim=c(0, 200), xlab="", ylab="", )
  lines(h1$mids, h1$counts, col="red")
  lines(h2$mids, h2$counts, col="blue")
  lines(h3$mids, h3$counts, col="green")
  title(main = title)
  legend("topleft", c("sync","async","combined"), fill=c("red", "blue", "green"))
}

plot_test("Class A")

