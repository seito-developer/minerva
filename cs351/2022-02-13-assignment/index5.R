library(ggplot2)

file_path2 = "./assets/engineer-career-data-arranged2.csv"
dataset2 <- read.csv(file_path2, header = T)
head(dataset2)

result <- glm(dataset2$job.offer ~ ., data = dataset2, family=binomial(link="logit"))
sum <- summary(result)
coe <- sum$coefficients
sum

# 無理やり回帰分析につっこんだ？
# Fitting a line to a binary response
# job.offer ~ study_load in logistic regression plot
data_space <- ggplot(data = dataset2, aes(x = study.load, y = job.offer)) + 
  geom_jitter(width = 0, height = 0.05, alpha = 0.5)

# linear regression line
data_space + geom_smooth(formula = dataset2$job.offer ~ dataset2$study.load, method = "glm", method.args = list(family = "binomial"), se = FALSE, color="red")



w0 <- coe[1,1]
w1 <- coe[1,1]
w0

plot(result)
single <- glm(ddataset2$study.load ~ ataset2$job.offer, data = dataset2, family=binomial(link="logit"))
plot(dataset2$job.offer, dataset2$study.load, ,xlab="job offer", ylab="study load")
abline(single, lwd=1, col="red")

person_offered
person_not_offered

sample_size = nrow(person_not_offered)

person_offered_extracted <- sample(person_offered, sample_size, replace = TRUE, prob = NULL)


mean(person_not_offered$study.load)
mean(person_offered_extracted$study.load)
