# Get the data
df <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQG3Q0A9Q4JsjWvaUK60W0xI7sJ3HZZVwdWrawoZZ2QRbMQgVynFjeIW0pSK2eEdyRphUanzvRyn9Up/pub?gid=754442322&single=true&output=csv")
# Eliminate a useless column (the first column)
df <- df[,-1]

head(df)

# Notice that the "turnout" variable is binary
# turnout = 1 if the person voted
# turnout = 0 if they did not.

# let's run a regression to predict "turnout"
# based on the predictors in this data set

# those predictors are 
# white, age, educate, income, and agesqrd
# where "educate" is years of education and
# agesqrd" is actually (age * age) / 100

# run the logistic regression 
lm2 <- glm(turnout ~ 
             white + age + educate + income + agesqrd, 
           data = df, family = binomial)
result <- summary(lm2)
coefficients <- result$coefficient

result
coefficients
