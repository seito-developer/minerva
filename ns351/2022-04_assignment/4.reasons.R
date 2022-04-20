library("dplyr")
library("tidyr")
library("ggplot2")
library("stringr")
library(RColorBrewer)

file_path = "./dataset.csv"
dataset <- read.csv(file_path, header = T)
head(dataset)

# Chart Color
coul <- rev(brewer.pal(5, "Blues"))

reasons <- c("quality", "operability", "freshness", "easy-to-understand", "easy-to-start", "activating", "asking", "others", "combination", "practicality", "dictionary")
reasons_label <- c("quality", "operability", "freshness", "e-t-u", "e-t-s", "activating", "asking", "others", "combination", "practicality", "dictionary")
reasons_len <- length(reasons)
dataset$book_strength
reasons_col[1]

generate_reason_dataset <- function(col, title){
  list_values <- c()
  for (index in 1:reasons_len) {
    list_values <- c(list_values, sum(col == reasons[index]))
  }
  
  sum_votes <- sum(list_values)
  
  data_frame <- data.frame(reasons_label, list_values)
  ggplot(data_frame, aes(x=reorder(reasons_label,-list_values) , y=list_values, fill=list_values)) + geom_bar(stat="identity") + scale_colour_brewer(palette = "BuPu", direction = - 1) + ggtitle(title) + scale_y_continuous(limits = c(0, 10)) + labs(x="Reasons", y="Number o Votes", fill="Votes", caption=paste("Total Votes:",sum_votes))
}

generate_reason_dataset(
  dataset$book_strength, 
  "The Reason of Book Strength"
)

generate_reason_dataset(
  dataset$text_strength, 
  "The Reason of Text Strength"
)

generate_reason_dataset(
  dataset$video_strength, 
  "The Reason of Video Strength"
)

generate_reason_dataset(
  dataset$console_strength, 
  "The Reason of Console Strength"
)

generate_reason_dataset(
  dataset$live_strength, 
  "The Reason of Live Strength"
)



