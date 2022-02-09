library(ggplot2)

file_path = "./assets/engineer-career-data-arranged.csv"
dataset <- read.csv(file_path, header = T)
head(dataset)

person_offered <- dataset[dataset$job.offer >= 1, ]
person_not_offered <- dataset[dataset$job.offer == 0, ]
result <- glm(dataset$job.offer ~ ., data = dataset)
summary(result)

person_offered
person_not_offered

sample_size = nrow(person_not_offered)

person_offered_extracted <- sample(person_offered, sample_size, replace = TRUE, prob = NULL)


mean(person_not_offered$study.load)
mean(person_offered_extracted$study.load)
