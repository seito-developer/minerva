install.packages("deSolve")
library(deSolve)

sir_fomula <- function(time, variables, parameters) {
  with(as.list(c(variables, parameters)), {
    differential_S <- -beta * I * S
    differential_I <-  beta * I * S - gamma * I
    differential_R <-  gamma * I
    return(list(c(differential_S, differential_I, differential_R)))
  })
}

# beta: weekly infectious contact rate
# ganmma: weekly recovery rate
param_vals <- c(
  beta  = 0.03, gamma = 0.3
)

initial_vals <- c(S = 999, I = 1, R = 0)

#duration (week)
time_vals <- seq(0, 10)

sir_values_1 <- ode(
  y = initial_vals,
  times = time_vals,
  func = sir_fomula,
  parms = param_vals 
)
sir_values_1 <- as.data.frame(sir_values_1)

colors <- c("blue", "red", "green")

with(sir_values_1, {
  # plotting the time series of susceptibles:
  plot(time, S, type = "l", col = colors[1], xlab = "time (days)", ylab = "number of people")
  lines(time, I, col = colors[2])
  lines(time, R, col = colors[3])
})

# adding a legend:
legend("right", c("S", "I", "R"),
       col = c(colors[1], colors[2], colors[3]), lty = 1, bty = "n")

