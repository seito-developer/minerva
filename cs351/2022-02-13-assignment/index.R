library(ggplot2)

file_path = "./assets/engineer-career-data2.csv"
dataset <- read.csv(file_path, header = T)
head(dataset)

arranged_dataset <- dataset[dataset$内定数>=1, ]

age <- arranged_dataset[,1]
entry <- arranged_dataset[,2]
year <- arranged_dataset[,3]
study_load <- arranged_dataset[,4]

#hist(c(age, offer))

#age & entry. r2=0.004504
#cannot look the relationship
ggplot(arranged_dataset, aes(x=age, y=entry)) + geom_point() + geom_smooth(method = "lm") + labs(x="Age", y="Entry")
summary(lm(age ~ entry, data = arranged_dataset))

#year & entry. r2=0.003
#cannot look the relationship
ggplot(arranged_dataset, aes(x=year, y=entry)) + geom_point() + geom_smooth(method = "lm") + labs(x="Year", y="Entry")
summary(lm(year ~ entry, data = arranged_dataset))

#year & study_load r2=0.02857
#cannot look the relationship
ggplot(arranged_dataset, aes(x=year, y=study_load)) + geom_point() + geom_smooth(method = "lm") + labs(x="Year", y="Study load")
summary(lm(year ~ study_load, data = arranged_dataset))
dataset
dataset$内定数
ans <- glm(dataset$内定数~., data = dataset, family = 'binomial')
ans_sum <- summary(ans)
coe <- ans_sum$coefficient
coe

RR <- exp(coe[,1])
RRlow <- exp(coe[,1]-1.96*coe[,2])
RRup <- exp(coe[,1]+1.96*coe[,2])
N <- nrow(data)
aic <- AIC(ans)
result <- cbind(coe,RR,RRlow,RRup,aic,N)
result
