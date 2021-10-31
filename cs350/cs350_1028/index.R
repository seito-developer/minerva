my_EV <- function(X,provX){
  
  #mean function
  my_mean <- function(dataset) 
  {
    # this function returns the mean of a dataset
    N = length(dataset)
    return(sum(dataset) / N)
  }
  
  #calculate X
  N <- 20000
  list_data <- c()
  
  for (i in 1:N) {
    list_data <- sample(X, 1)
  }
  ev1 = my_mean(list_data)
  
  #calculate provX
  N2 <- length(X)
  list_data2 <- c()
  for (i in 1:N2) {
    list_data2 <- c(list_data2, i * (1/N2))
  }
  ev2 = sum(list_data2)
  
  val <- c(ev1, ev2)
  return(val)
}

# Sample
#my_EV(c(1,2,3,4,5,6))
X = c(1,2,3,4,5,6) # outcomes from rolling a die
probX <- c(1/6,1/6,1/6,1/6,1/6,1/6) # a fair die (equal probabilites)
cat("\n", my_EV(X,probX), "\n") 




