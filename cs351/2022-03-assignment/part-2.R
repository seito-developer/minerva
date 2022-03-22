install.packages("deSolve")
library(deSolve)

# SIR's base model with differential
sir_formula <- function(time, variables, parameters) {
  with(as.list(c(variables, parameters)), {
    differential_S <- -beta * I * S
    differential_I <-  beta * I * S - gamma * I
    differential_R <-  gamma * I
    return(list(c(differential_S, differential_I, differential_R)))
  })
}

# settings
# beta: weekly infectious contact rate
# gamma: weekly recovery rate
param_vals <- c(
  beta  = 0.03, gamma = 0.3
)

#other settings
colors <- c("blue", "red", "green")
initial_vals <- c(S = 999, I = 1, R = 0)
time_vals <- seq(0, 10)
sir_model <- ode(
  y = initial_vals,
  times = time_vals,
  func = sir_formula,
  parms = param_vals 
)
sir_model <- as.data.frame(sir_model)

#plot
with(sir_model, {
  plot(time, S, type = "l", col = colors[1], xlab = "time (week)", ylab = "number of people")
  lines(time, I, col = colors[2])
  lines(time, R, col = colors[3])
})
legend("right", c("S", "I", "R"), col = c(colors[1], colors[2], colors[3]), lty = 1, bty = "n")

