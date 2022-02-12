#t検定したい。高卒と大卒
#検定力
#Cohen Dでeffect size

#説明変数：内定数
#従属変数：年齢、勉強時間、年...相関性はあるか？

library(ggplot2)
library(broom)
library(dplyr)

file_path = "./assets/engineer-career-data.csv"
dataset <- read.csv(file_path, header = T)
head(dataset)
attach(dataset)



# transform "year" column
# 2022年今年に近いほど内定が取りやすい or 取りにくくなるかを算出したい
dataset <- transform(dataset, year=year-2022)
dataset 

result <- lm(formula = job.offer ~ age + entry + year + study.load, data = dataset)
result
summary(result)

# entry 1.0145085 1件増やすごとに内定0.01件増える
exp(result$coefficients)

#draw
coeff <- result$coefficients
ggplot(data = dataset, aes(x = entry, y = job.offer)) +
  geom_point() + geom_abline(intercept = coeff[1], slope = coeff[3])

result2 <- lm(formula = job.offer ~ study.load + entry, data = dataset)
summary(result2)


##############
# plot 書く #
##############