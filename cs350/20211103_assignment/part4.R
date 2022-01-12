# Strage to save T/F
parent_strage <- c()
child_strage <- c()

# Define the simulate times
challenges <- 10000

#Simulate by enough times
for (parent_i in 1:challenges) {
  
  # Generate the coffee prices
  x <- sample(1:10, 10, replace = FALSE)
  # Generate my post-it
  y <- sample(1:10, 10, replace = FALSE)
  
  # Compare all coffee prices and my post-its
  for (child_i in 1:10) {
    child_strage[child_i] <- x[child_i] == y[child_i]
  }
  
  # Tally the results of "NONE of the post-its are matched to the correct coffee shop"
  if(sum(child_strage) == 0){
    parent_strage[parent_i] <- TRUE
  } else {
    parent_strage[parent_i] <- FALSE
  }
}

# Estimate the probability
prob <- sum(parent_strage)/challenges

# Output
print(paste("The probability is", prob * 100, "%"))

