library(data.table)
install.packages("ISLR")
library(ISLR)
library(ggplot2)
install.packages("InformationValue")
library(InformationValue)
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

# Convert Y/N to 1/0
binary_converter <- function(data){
  prob <- c()
  for (item in data) {
    if(item == 'Yes'){
      prob <- c(prob, 1)
    } else {
      prob <- c(prob, 0)
    }
  }
  return(prob)
}

training_def <- binary_converter(training.set$default)
new_training_data = data.table(default = training_def, income = training.set$income, balance = training.set$balance)
new_training_data
my_model <- glm(formula = training_def ~ training.set$income + training.set$balance, data = new_training_data, family="binomial")
summary(my_model)

plot_view <- ggplot(training.set,aes(x=training.set$balance, y=training_def)) +
  geom_point() + 
  geom_smooth(method = "glm", method.args= list(family="binomial"))
plot_view


#---
test_def <- binary_converter(test.set$default)
new_test_data = data.table(default = test_def, income = test.set$income, balance = test.set$balance)
new_test_data
prediction <- predict(my_model, newData = new_test_data, type="response")

optimal <- optimalCutoff(new_test_data$default, prediction)[1]

confusionMatrix(new_test_data$default, prediction)

