library(ggplot2)

file_path = "./assets/engineer-career-data-arranged2.csv"
dataset <- read.csv(file_path, header = T)
head(dataset)
attach(dataset)

#mean
u_graduated_person <-  dataset[final.education == 1, ]
hs_graduated_person <- dataset[final.education == 0, ]

u_jo_mean <- mean(u_graduated_person$job.offer)
hs_jo_mean <- mean(hs_graduated_person$job.offer)

#大卒とそれ以外で関連性があるか判定
result <- glm(formula = job.offer ~ study.load + final.education, data=dataset)
result

#t検定は出来ない
#ただ、前提時要件があって、2群が正規分布していることが必要です。サンプルを選んだときに、無作為抽出していたり、サンプル数が1000ほどあれば、正規分布を想定できます。
