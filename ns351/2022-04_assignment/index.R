library("dplyr")
library("tidyr")
library("ggplot2")
library("stringr")
library(RColorBrewer)
library("plyr")

file_path = "./dataset.csv"
dataset <- read.csv(file_path, header = T)
head(dataset)

# Variables
eval_data <- c("strong_disagree", "disagree", "neigther", "agree", "strong_agree")
types <- c("book", "text", "video", "console", "live")

## types
convert_eval_to_score <- function(target_col){
  new_col <- ifelse(target_col=="強く同意する", eval_data[5],
                    ifelse(target_col=="やや同意する", eval_data[4],
                           ifelse(target_col=="どちらとも言えない", eval_data[3],
                                  ifelse(target_col=="あまり同意しない", eval_data[2],
                                         ifelse(target_col=="まったく同意しない", eval_data[1], FALSE
                                         )))))
  return(new_col)
}

new_book <- c(convert_eval_to_score(dataset[,6]))
new_text <- c(convert_eval_to_score(dataset[,7]))
new_video <- c(convert_eval_to_score(dataset[,8]))
new_console <- c(convert_eval_to_score(dataset[,9]))
new_live <- c(convert_eval_to_score(dataset[,10]))

# build new dataset
new_dataset <- data.frame(
  book = new_book,
  text = new_text,
  video = new_video,
  console = new_console,
  live = new_live
)

scoring <- function(item){
  score <- c()
  for (index in 1:5) {
    score = c(score, sum(str_count(item, eval_data[index])))
  }
  return(score)
}

start_col <- 1
type_cols_len <- ncol(new_dataset)
scores <- c()

for (i_types in start_col:type_cols_len) {
  print(i_types)
  for (i_eval in 1:length(eval_data)) {
    scores <- c(scores, sum(new_dataset[,i_types]==eval_data[i_eval]))
  }
}

#variables: eval_data, types, scores
custom_types <- c(rep(types, each = length(types)))
custom_eval  <- c(rep(eval_data, times = length(eval_data)))
new_table <- data.frame(custom_types, custom_eval, scores)
custom_types
scores
custom_eval

df <- data.frame(x = custom_types, y = scores, group = custom_eval)
ggplot(df, aes(x = x, y = y, fill = custom_eval)) + 
  geom_bar(stat = "identity")




ggplot(new_table, aes(x = custom_types, y = scores, fill = custom_eval, label = scores)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette="Blues") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) + 
  labs(title="The Rate of Evaluation in Each Types",
       x = "Learning Types", y = "Numbers of Votes", 
       caption = paste("Total Votes:", sum(scores)/5),
       fill = "Evaluation")



new_table
ggplot(data = new_table, aes(x = custom_types, y = scores, fill = custom_eval)) + 
  geom_bar(stat='identity')
  