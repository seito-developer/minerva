# can combine 1 row (asynchronous + synchronous)
# 

# SNUCMの医学部1年生、2年生、3年生、4年生の学生を、以下のように分類しました。
# 学習方法は、100% Asynchronous (非同期式のみ), 100%(非同期式のみ), 100%(非同期式のみ)の3グループ。
# 2年以内にすべての必修科目で適用
# 学業成績（過去2学期の平均GPA）のカテゴリー（低、中、高）に基づく。大学側が「不合格」と判断した学生は除外する。

#load library
install.packages("makedummies")
install.packages("ggplot2")
library("makedummies")
library("ggplot2")
library(tidyverse)

generateDummyData <- function(asynchronous, synchronous, top_score){
  
  mean_scores <- c(top_score * 0.75, top_score * 0.9, top_score)
  sd <- 5
  
  students_low <- data.frame(
    student_achive = 1,
    asynchronous = rep(asynchronous, size = size),
    synchronous = rep(synchronous, size = size),
    grade = rep(1:4, times = size/4, size = size),
    gpa_class_a = round(abs(rnorm(size, mean = mean_scores[1], sd = sd))),
    gpa_class_b = round(abs(rnorm(size, mean = mean_scores[1], sd = sd))),
    gpa_class_c = round(abs(rnorm(size, mean = mean_scores[1], sd = sd)))
  )
  
  students_mid <- data.frame(
    student_achive = 2,
    asynchronous = rep(asynchronous, size = size),
    synchronous = rep(synchronous, size = size),
    grade = rep(1:4, times = size/4, size = size),
    gpa_class_a = round(abs(rnorm(size, mean = mean_scores[2], sd = sd))),
    gpa_class_b = round(abs(rnorm(size, mean = mean_scores[2], sd = sd))),
    gpa_class_c = round(abs(rnorm(size, mean = mean_scores[2], sd = sd)))
  )
  
  students_high <- data.frame(
    student_achive = 3,
    asynchronous = rep(asynchronous, size = size),
    synchronous = rep(synchronous, size = size),
    grade = rep(1:4, times = size/4, size = size),
    gpa_class_a = round(abs(rnorm(size, mean = mean_scores[3], sd = sd))),
    gpa_class_b = round(abs(rnorm(size, mean = mean_scores[3], sd = sd))),
    gpa_class_c = round(abs(rnorm(size, mean = mean_scores[3], sd = sd)))
  )
  generated_data <- rbind(students_low, students_mid, students_high)
  
  return(generated_data)
}

# dummy date
testData_synchronous <- generateDummyData(0, 1, 80)
testData_asynchronous <- generateDummyData(1, 0, 70)
testData_blended <- generateDummyData(1, 1, 85)



testData_all
#regression
testData_all <- rbind(testData_a, testData_b, testData_c)
result <- lm(formula = testData_all$gpa_class_a ~ testData_all$asynchronous + testData_all$synchronous, data = testData_all)
result
#cohen's D
#testData_a_arranged <- data.frame(testData_a$gpa_class_a
ES <- 0.8
#ES*1 + 1
ggplot(testData_c$gpa_class_a, aes(x,y, color="Asynchronous")) +
  # add line for treatment group
  geom_line(size=1) + 
  # add line for control group
  geom_line(testData_c$gpa_class_c, aes(color="Combination"),size=1) +
  # shade overlap
  #geom_polygon(aes(color=NULL), data=testData_c$gpa_class_a, fill="red", alpha=I(4/10),
  #             show_guide=F) +
  # add vlines for group means
  #geom_vline(xintercept = 1, linetype="dotted") + 
  #geom_vline(xintercept = mean1, linetype="dotted") + 
  # add plot title
  #opts(title=paste("Visualizing Effect Sizes 
  #    (Cohen's d = ",ES,"; U3 = ",u3,")", sep="")) +
  # change colors and legend annotation
  scale_color_manual("Group", 
                     values= c("Asynchronous" = "black","Combination" = "red")) +
  # remove axis labels
  ylab(NULL) + xlab(NULL)

ggplot(data = testData_c) + aes(x = displ, y = gpa_class_a)
