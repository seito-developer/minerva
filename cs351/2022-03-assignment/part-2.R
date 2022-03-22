#Loading libraries
library(ggplot2)

f <- expression(x^3)
D(f,"x")

f <- deriv(~x^3, "x", func=TRUE)
f(2)


dt <- 0.01
T = 1
nstep = T/dt + 1
x = rep(1, nstep)

for(i in 1:nstep){
  x[i+1] = x[i] - dt * (9 * x[i])
}
t=(0:nstep) * dt
plot(t, x)
