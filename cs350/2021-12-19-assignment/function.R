dollerToInt <- function(dataset, data_list){
  data_list <- c()
  len <- length(dataset)
  for (i in 1:len) {
    complete.cases(dataset[i])
    target <- str_sub(dataset[i], start = 2, end = -1)
    num <- str_replace(target, ',', '')
    data_list <- c(data_list, as.integer(num))
  }
  return(data_list)
}


