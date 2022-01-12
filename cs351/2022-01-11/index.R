n <- 20
a <- rnorm(n,0,1)
b <- rnorm(n,0,1)
c <- c(a, b)
c

covariance <- sum(c/n*2)
covariance / (sd(a) * sd(b))

plot(a, b)

