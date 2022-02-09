library(tidyverse)
library(nnet)

file_path = "./assets/engineer-career-data2.csv"
dataset <- read.csv(file_path, header = T)
head(dataset)
dataset


ans <- glm(内定数 ~ ご年齢, family = binomial(), data = dataset)
summary(ans)

dataset %>% 
  ggplot(aes(x = ご年齢, y = ,内定数)) + 
  geom_jitter() + 
  geom_smooth(method = "glm", method.args = list(family = "binomial"))

ans_sum <- summary(ans)
coe <- ans_sum$coefficient
coe

#predict(model2, type = "response")

