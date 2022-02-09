library(ggplot2)

file_path2 = "./assets/engineer-career-data-arranged2.csv"
dataset2 <- read.csv(file_path2, header = T)
head(dataset2)

result <- glm(dataset2$job.offer ~ ., data = dataset2, family=binomial(link="logit"))
sum <- summary(result)
coe <- sum$coefficients
sum

w0 <- coe[1,1]
w1 <- coe[1,1]
w0

plot(result)

person_offered
person_not_offered

sample_size = nrow(person_not_offered)

person_offered_extracted <- sample(person_offered, sample_size, replace = TRUE, prob = NULL)


mean(person_not_offered$study.load)
mean(person_offered_extracted$study.load)
