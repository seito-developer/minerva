a <- rnorm(n=100, mean=10, sd=2)
b <- rnorm(n=100, mean=10, sd=2)

plot(a,b)

abline(lm(a ~ b))

c <- c(a, b)
r <- (sum(c)/200) / (100 * 100)
r