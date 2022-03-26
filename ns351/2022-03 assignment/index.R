# can combine 1 row (asynchronous + synchronous)
# 

# SNUCMの医学部1年生、2年生、3年生、4年生の学生を、以下のように分類しました。
# 学習方法は、100% Asynchronous (非同期式のみ), 100%(非同期式のみ), 100%(非同期式のみ)の3グループ。
# 2年以内にすべての必修科目で適用
# 学業成績（過去2学期の平均GPA）のカテゴリー（低、中、高）に基づく。大学側が「不合格」と判断した学生は除外する。

#load library
install.packages("makedummies")
install.packages("ggplot2")
library(makedummies)
library("ggplot2")
library(tidyverse)
library(effsize)

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

# dummy date
test_sync <- generateDummyData(0, 1, 75)
test_async <- generateDummyData(1, 0, 70)
test_combined <- generateDummyData(1, 1, 85)

# function for plot with t-test
plot_test <- function(title, target1, target2, base){
  diff <- target1 - target2
  ggplot(base, aes(x = diff)) +
    geom_density() + 
    geom_vline(xintercept = mean(diff)) +
    geom_vline(xintercept = mean(diff) +
                 2 * c(-1,1) * sd(diff)/sqrt(nrow(base)), linetype = 2) +
    labs(title = title)
}

# function for showing cohen's D
t.test.es <- function(x, y, t.paired = FALSE, es.ci = 0.95, es.paired = FALSE, rm = FALSE)
{
  t <- t.test(x, y, paired = t.paired)
  es <- effsize::cohen.d(x, y, conf.level = es.ci, na.rm = rm, paired = es.paired)
  return(list(t,es))
}

plot_test("Class A - Synchronous vs Combined", test_sync$class_score_a, test_combined$class_score_a, test_sync)
t.test.es(x = test_sync$class_score_a, y = test_combined$class_score_a)




