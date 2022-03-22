###### Preparation START ######
install.packages("ISLR")
install.packages("dplyr")

library(data.table)
library(ISLR)
library(dplyr)

data(Default) #10000 observations
set.seed(123)
training.set.index <- sample(1:10000, 8000, replace = FALSE) # sample 80% of the observations for the training set
training.set <- Default[training.set.index,] # the remaining 20% are for the test set
test.set <- Default[-training.set.index,] # confirm that your results match mine
###### Preparation END ######

###### (1) START ######
#Arranging the training data
sampling_size = nrow(test.set)
sampled_training = sample_n(tbl = training.set, size = sampling_size)
sampled_training$default <- ifelse(sampled_training$default=="Yes", 1, 0)
sampled_training$student <- ifelse(sampled_training$student=="Yes", 1, 0)

#Run the logistic regression
my_model <- glm(formula = sampled_training$default ~ ., data = sampled_training, family="binomial")
result <- summary(my_model)
result

#Estimate the prediction and arranging the test data
sampled_test = sample_n(tbl = test.set, size = sampling_size)
sampled_test$default <- ifelse(sampled_test$default=="Yes", 1, 0)
sampled_test$student <- ifelse(sampled_test$student=="Yes", 1, 0)

#Generate the confusion matrix
prediction <- predict(my_model, newData = sampled_test, type="response")
matrix <- as.matrix(table(factor(prediction>0.5, levels=c(F, T)), sampled_test$default))
matrix

# Show error rate
(matrix[1,2] + matrix[2,1]) / sum(matrix)

#Output the rate of default
prediction_arranged <- ifelse(prediction >= 0.5, 1, 0)
sum(prediction_arranged)/length(prediction_arranged)
###### (1) END ######

###### (2) START ######
#load the library
install.packages("rgenoud")
library(rgenoud)

#Call the better 
my_model_2 <- glm(formula = sampled_training$default ~ sampled_training$student + sampled_training$balance, 
                  data = sampled_training, family="binomial")
prediction_2 <- predict(my_model_2, newData = sampled_test, type="response")

#function to find the optimal threshold
find_threshold <- function(x){
  conf_matrix <- as.matrix(table(factor(prediction_2>x, levels=c(F, T)), sampled_test$default))
  error_rate = (conf_matrix[1,2] + conf_matrix[2,1]) / sum(conf_matrix)
  return(error_rate)
}

#Optimize the matrix
thresholds <- c()
for (index in 1:100) {
  genoud_result <- genoud(find_threshold, nvars = 1, max = FALSE) 
  thresholds <- c(thresholds, genoud_result$par)
}
cat('mean:', mean(thresholds), 'min:', min(thresholds), 'max:', max(thresholds))

improved_matrix <- as.matrix(table(factor(prediction_2>mean(thresholds), levels=c(F, T)), sampled_test$default))
improved_matrix

# Show error rate
(improved_matrix[1,2] + improved_matrix[2,1]) / sum(improved_matrix)
###### (2) END ######

###### (3) START ######
result <- optim(0.5, find_threshold, method = "Brent", lower = -100, upper = 100)

improved_matrix2 <- as.matrix(table(factor(prediction_2>result$par, levels=c(F, T)), sampled_test$default))
improved_matrix2

(improved_matrix2[1,2] + improved_matrix2[2,1]) / sum(improved_matrix2)
###### (3) END ######


