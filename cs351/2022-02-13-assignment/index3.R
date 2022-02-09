library(ggplot2)

file_path = "./assets/engineer-career-data2.csv"
dataset <- read.csv(file_path, header = T)
head(dataset)

age <- dataset[,1]
entry <- dataset[,2]
year <- dataset[,3]
study_load <- dataset[,4]
offer <- dataset[,5]

dat <- dataset[,c(5, 1,2,3,4)]
dat
#ans <- glm(dat$内定数~.,data=dat,family=binomial) #解析結果をansに代入
ans <- glm(dat$内定数~dat$ご年齢+dat$エントリーした会社数+dat$就活した時期+dat$勉強時間,data=dat,family=binomial) #解析結果をansに代入
ans_sum <- summary(ans)
ans_sum
coe <- ans_sum$coefficient
coe # いずれも有意差なし (5%未満)

#plot(dat$エントリーした会社数, dat$内定数, xlim = range(x), xlab = 'x', ylab = 'y')

#RR <- exp(coe[,1])
#RRlow <- exp(coe[,1]-1.96*coe[,2])
#RRup <- exp(coe[,1]+1.96*coe[,2])
#N <- nrow(data)
#aic <- AIC(ans)
#result <- cbind(coe,RR,RRlow,RRup,aic,N)
#result
#colnames(result)[6:7] <- c("RR95%CI.low","RR95%CI.up")
#result[2:nrow(result),8:9] <- ""
#result

