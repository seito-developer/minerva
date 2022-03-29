#load library
install.packages("makedummies")
install.packages("ggplot2")
library(makedummies)
library("ggplot2")
library(tidyverse)
library(effsize)

#function for generating dummy data
size <- 372

generateDummyData <- function(asynchronous, synchronous, scores, sd){
  mean_scores <- c(scores[1], scores[2], scores[3])
  
  students_low <- data.frame(
    student_achive = 1,
    asynchronous = rep(asynchronous, size = size/3),
    synchronous = rep(synchronous, size = size/3),
    grade = rep(1:4, times = size/4, size = size),
    class_score_a = round(abs(rnorm(size, mean = mean_scores[1], sd = sd))),
    class_score_b = round(abs(rnorm(size, mean = mean_scores[1], sd = sd))),
    class_score_c = round(abs(rnorm(size, mean = mean_scores[1], sd = sd)))
  )
  
  students_mid <- data.frame(
    student_achive = 2,
    asynchronous = rep(asynchronous, size = size/3),
    synchronous = rep(synchronous, size = size/3),
    grade = rep(1:4, times = size/4, size = size),
    class_score_a = round(abs(rnorm(size, mean = mean_scores[2], sd = sd))),
    class_score_b = round(abs(rnorm(size, mean = mean_scores[2], sd = sd))),
    class_score_c = round(abs(rnorm(size, mean = mean_scores[2], sd = sd)))
  )
  
  students_high <- data.frame(
    student_achive = 3,
    asynchronous = rep(asynchronous, size = size/3),
    synchronous = rep(synchronous, size = size/3),
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

# function for plot
plot_test <- function(title, target1, target2, target3){
  h1 <- hist(target1)
  h2 <- hist(target2)
  h3 <- hist(target3)
  
  plot(0, 0, type="n", xlim=c(0,100), ylim=c(0, 200), xlab="Scores", ylab="Numbers of Students", )
  lines(h1$mids, h1$counts, col="red")
  lines(h2$mids, h2$counts, col="blue")
  lines(h3$mids, h3$counts, col="green")
  title(main = title)
  legend("topleft", c("sync","async","combined"), fill=c("red", "blue", "green"))
}

# function for showing t-test & cohen's D result
t.test.es <- function(x, y, t.paired = FALSE, es.ci = 0.95, es.paired = FALSE, rm = FALSE)
{
  t <- t.test(x, y, paired = t.paired)
  es <- effsize::cohen.d(x, y, conf.level = es.ci, na.rm = rm, paired = es.paired)
  return(list(t,es))
}

plot_test("Class A", test_sync$class_score_a, test_async$class_score_a, test_combined$class_score_a)
plot_test("Class B", test_sync$class_score_b, test_async$class_score_b, test_combined$class_score_b)
plot_test("Class C", test_sync$class_score_c, test_async$class_score_c, test_combined$class_score_c)

# Class A
t.test.es(x = test_combined$class_score_a[test_combined$grade == 1], y = test_sync$class_score_a[test_sync$grade == 1])
t.test.es(x = test_combined$class_score_a[test_combined$grade == 2], y = test_sync$class_score_a[test_sync$grade == 2])
t.test.es(x = test_combined$class_score_a[test_combined$grade == 3], y = test_sync$class_score_a[test_sync$grade == 3])

t.test.es(x = test_combined$class_score_a, y = test_async$class_score_a)
mean(test_sync$class_score_a)
mean(test_async$class_score_a)
mean(test_combined$class_score_a)

# Class B
t.test.es(x = test_combined$class_score_b, y = test_sync$class_score_b)
t.test.es(x = test_combined$class_score_b, y = test_async$class_score_b)
mean(test_sync$class_score_b)
mean(test_async$class_score_b)
mean(test_combined$class_score_b)

# Class C
t.test.es(x = test_combined$class_score_c, y = test_sync$class_score_c)
t.test.es(x = test_combined$class_score_c, y = test_async$class_score_c)
mean(test_sync$class_score_c)
mean(test_async$class_score_c)
mean(test_combined$class_score_c)
