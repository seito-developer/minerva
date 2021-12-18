dollerToInt <- function(val){
  complete.cases(val)
  target <- str_sub(val, start = 2, end = -1)
  num <- str_replace(target, ',', '')
  return(as.integer(num))
}

