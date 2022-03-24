# SNUCMの医学部1年生、2年生、3年生、4年生の学生を、以下のように分類しました。
# 学習方法は、100% Asynchronous (非同期式のみ), 100%(非同期式のみ), 100%(非同期式のみ)の3グループ。
# 2年以内にすべての必修科目で適用
# 学業成績（過去2学期の平均GPA）のカテゴリー（低、中、高）に基づく。大学側が「不合格」と判断した学生は除外する。

#load library
install.packages("makedummies")
library("makedummies")

# dummy date
testData_a <- data.frame(
  asynchronous = rep(0, size = size),
  synchronous = rep(1, size = size),
  grade = rep(1:4, times = size/4, size = size),
  gpa_class_a = round(abs(rnorm(size, mean = 2, sd = 1))),
  gpa_class_b = round(abs(rnorm(size, mean = 2, sd = 1))),
  gpa_class_c = round(abs(rnorm(size, mean = 2, sd = 1)))
)
testData_b <- data.frame(
  asynchronous = rep(1, size = size),
  synchronous = rep(0, size = size),
  grade = rep(1:4, times = size/4, size = size),
  gpa_class_a = round(abs(rnorm(size, mean = 2, sd = 1))),
  gpa_class_b = round(abs(rnorm(size, mean = 2, sd = 1))),
  gpa_class_c = round(abs(rnorm(size, mean = 2, sd = 1)))
)
testData_c <- data.frame(
  asynchronous = rep(1, size = size),
  synchronous = rep(1, size = size),
  grade = rep(1:4, times = size/4, size = size),
  gpa_class_a = round(abs(rnorm(size, mean = 2.5, sd = 1))),
  gpa_class_b = round(abs(rnorm(size, mean = 2.5, sd = 1))),
  gpa_class_c = round(abs(rnorm(size, mean = 2.5, sd = 1)))
)

testData_all <- rbind(testData_a, testData_b, testData_c)
testData_all
