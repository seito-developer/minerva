###### Preparation START ######
install.packages("ISLR")
install.packages("InformationValue")
install.packages("dplyr")

library(data.table)
library(ISLR)
library(ggplot2)
library(InformationValue)
library(dplyr)

data(Default) #10000 observations
set.seed(123)

training.set.index <- sample(1:10000, 8000, replace = FALSE) # sample 80% of the observations for the training set
training.set <- Default[training.set.index,] # the remaining 20% are for the test set
test.set <- Default[-training.set.index,] # confirm that your results match mine

mean(training.set$income) # 33514.55
mean(test.set$income)     # 33526.7
###### Preparation END ######

###### (1) Extra START ######
#Arranging the training data
sampling_size = nrow(test.set)

sampled_training = sample_n(tbl = training.set, size = sampling_size)
sampled_training$default <- ifelse(sampled_training$default=="Yes", 1, 0)
sampled_training$student <- ifelse(sampled_training$student=="Yes", 1, 0)
my_model_2 <- glm(formula = sampled_training$default ~ sampled_training$student + sampled_training$balance, 
                  data = sampled_training, family="binomial")
my_model_3 <- glm(formula = sampled_training$default ~ sampled_training$student, 
                  data = sampled_training, family="binomial")
my_model_4 <- glm(formula = sampled_training$default ~ sampled_training$balance, 
                  data = sampled_training, family="binomial")

my_model_2$aic
my_model_3$aic
my_model_4$aic

#Estimate the prediction and arranging the test data
sampled_test = sample_n(tbl = test.set, size = sampling_size)
sampled_test$default <- ifelse(sampled_test$default=="Yes", 1, 0)
sampled_test$student <- ifelse(sampled_test$student=="Yes", 1, 0)

#Generate the confusion matrix
prediction <- predict(my_model_4, newData = sampled_test, type="response")
matrix <- confusionMatrix(sampled_test$default, prediction)
matrix
(matrix$`0`[2] + matrix$`1`[1]) / (matrix$`0`[1] + matrix$`1`[2])

#Output the rate of default
prediction_arranged <- ifelse(prediction >= 0.5, 1, 0)
sum(prediction_arranged)/length(prediction_arranged)

###### (1) Extra END ######

