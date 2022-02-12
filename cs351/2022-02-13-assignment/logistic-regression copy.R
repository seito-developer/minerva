#年齢によって合格率はかわるか？

library(ggplot2)
library(broom)
library(dplyr)

file_path = "./assets/engineer-career-data-arranged.csv"
dataset <- read.csv(file_path)
head(dataset)
#attach(dataset)

trans_age <- function(val){
  output <- NULL
  if(val < 30){
    output <- 0
  } else {
    output <- 1
  }
  return(output)
}

mutate(dataset, age2 = trans_age(age))
       
  # 勉強時間によって合格率はかわるか？
# study load p-val = 0.0512
# 有意差ギリなし
result <- glm(formula = job.offer ~ study.load, data = dataset, family="binomial")
result
summary(result)

#-------------

# 2 type of data
# 大卒 or 高卒
u_graduated_person <-  dataset[final.education == "GU", ]
hs_graduated_person <- dataset[final.education == "GH", ]

# 平均合格率に0.05663251に差がある
u_jo_mean <- mean(u_graduated_person$job.offer)
hs_jo_mean <- mean(hs_graduated_person$job.offer)
cat("the gap between u_jo_mean and hs_jo_mean:", u_jo_mean - hs_jo_mean)

# 大卒とそれ以外で関連性があるか判定
# edu p-val = 0.281
# 有意差なし
result <- glm(formula = job.offer ~ study.load + final.education, data = dataset, family="binomial")
result
summary(result)

# 大卒の方が1.69倍合格しやすい...が有意差なしとなっているためこの数字は信ぴょう性なし
# exp(result$coefficients)

#https://tamanobi.hatenablog.com/entry/2018/06/09/192810

##############
# plot 書く #
##############