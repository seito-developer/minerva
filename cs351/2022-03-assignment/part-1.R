###### Preparation START ######
install.packages("ISLR")
install.packages("InformationValue")

library(data.table)
library(ISLR)
library(ggplot2)
library(InformationValue)

data(Default) #10000 observations
set.seed(123)

training.set.index <- sample(1:10000, 8000, replace = FALSE) # sample 80% of the observations for the training set
training.set <- Default[training.set.index,] # the remaining 20% are for the test set
test.set <- Default[-training.set.index,] # confirm that your results match mine
mean(training.set$income) # 33514.55
mean(test.set$income)     # 33526.7
###### Preparation END ######

###### (1) START ######
#Arranging the training data
training.set$default <- ifelse(training.set$default=="Yes", 1, 0)
my_model <- glm(formula = training.set$default ~ training.set$income + training.set$balance, 
                data = training.set, family="binomial")
summary(my_model)

#Estimate the prediction and arranging the test data
prediction <- predict(my_model, newData = test.set, type="response")
test.set$default <- ifelse(test.set$default=="Yes", 1, 0)
optimal <- optimalCutoff(test.set$default, prediction)[1]
test_len <- length(test.set$default)

#Generate the confusion matrix
confusionMatrix(test.set$default, head(prediction, n=test_len))
###### (1) END ######