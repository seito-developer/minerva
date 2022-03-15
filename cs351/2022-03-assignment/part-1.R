install.packages("ISLR")
library(ISLR)
library(ggplot2)
data(Default) #10000 observations
set.seed(123)
# sample 80% of the observations for the training set
training.set.index <- sample(1:10000, 8000, replace = FALSE)
training.set <- Default[training.set.index,]
# the remaining 20% are for the test set
test.set <- Default[-training.set.index,]
# confirm that your results match mine
mean(training.set$income) # 33514.55
mean(test.set$income)     # 33526.7
### Alternatively, just download the data set:
#write.csv(training.set, file = "training.set_Default.csv", row.names = FALSE)
###training set download link,  in .csv format: https://drive.google.com/file/d/1Ps_3w-jUfJs0hA4rCpbyZuPJuOA6LItw/view?usp=sharing
#write.csv(test.set, file = "test.set_Default.csv", row.names = FALSE)
###test set download link, in .csv format: https://drive.google.com/file/d/1WmiCfb1fhSmWBfxGYL5A99FQ_Tmz2EaK/view?usp=sharing


# Convert Y/N to 1/0
prob <- c()
for (item in training.set$student) {
  if(item == 'Yes'){
    prob <- c(prob, 1)
  } else {
    prob <- c(prob, 0)
  }
}

result <- glm(formula = prob ~ training.set$income + training.set$balance, data = training.set, family="binomial")
summary(result)

plot_view <- ggplot(training.set,aes(x=training.set$income, y=prob)) +
  geom_point() + 
  geom_smooth(method = "glm", method.args= list(family="binomial"))
plot_view

