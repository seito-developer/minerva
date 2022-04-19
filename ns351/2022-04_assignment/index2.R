library("dplyr")
library("tidyr")
library("ggplot2")
library("stringr")

file_path = "./dataset.csv"
dataset <- read.csv(file_path, header = T)
head(dataset)

#### Arrange dataset ####

reasons <- c("quality", "operability", "freshness", "etu", "ets", "ac", "ask", "others", "comb", "prc", "dic")
#reasons_col <- c(dataset$book_strength, dataset$text_strength, dataset$video_strength, dataset$console_strength, dataset$live_strength, dataset$book_weakness, dataset$text_weakness, dataset$video_weakness, dataset$console_weakness, dataset$live_weakness)
reasons_len <- length(reasons)
dataset$book_strength
reasons_col[1]

generate_reason_dataset <- function(col, title){
  list_values <- c()
  for (index in 1:reasons_len) {
    list_values <- c(list_values, sum(col == reasons[index]))
  }
  
  data_frame <- data.frame(reasons, list_values)
  print(data_frame)
  ggplot(data_frame, aes(x=reorder(reasons,-list_values) , y=list_values)) + geom_bar(stat="identity") + ggtitle(title)
}

generate_reason_dataset(dataset$book_strength, "The Reason of Book Strength")
