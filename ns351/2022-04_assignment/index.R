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

# age
new_age <- c()
new_age <- ifelse(dataset[,2]=="15歳以下", 15, 
                          ifelse(dataset[,2]=="16 - 20歳", 18, 
                                 ifelse(dataset[,2]=="21 - 25歳", 23, 
                                        ifelse(dataset[,2]=="26 - 30歳", 28, 
                                               ifelse(dataset[,2]=="31 - 35歳", 33, 
                                                      ifelse(dataset[,2]=="36 - 40歳", 38, 
                                                             ifelse(dataset[,2]=="41 - 45歳", 43, 
                                                                    ifelse(dataset[,2]=="46 - 50歳", 48, 
                                                                           ifelse(dataset[,2]=="51 - 55歳", 53, 
                                                                                  ifelse(dataset[,2]=="56 - 60歳", 58, 
                                                                                         ifelse(dataset[,2]=="61 - 65歳", 63, 
                                                                                                ifelse(dataset[,2]=="66 - 70歳", 68, 
                                                                                                       ifelse(dataset[,2]=="71 - 75歳", 73, 
                                                                                                              ifelse(dataset[,2]=="76 - 80歳", 78, 
                                                                                                                     ifelse(dataset[,2]=="81歳以上", 81, FALSE 
                          )))))))))))))))
# experience
new_experience <- c()
new_experience <- ifelse(dataset[,4]=="ない", 1,
                         ifelse(dataset[,4]=="1年未満", 2,
                                ifelse(dataset[,4]=="1年以上3年未満", 3,
                                       ifelse(dataset[,4]=="3年以上5年未満", 4,
                                              ifelse(dataset[,4]=="5年以上", 5, FALSE
                                              )))))

new_lang <- c()
new_lang <- ifelse(dataset[,5]=="強く同意する（絶対に日本語がいい）", eval_data[5],
                   ifelse(dataset[,5]=="やや同意する（できれば日本語がいい）", eval_data[4],
                          ifelse(dataset[,5]=="どちらとも言えない（日本語でも英語でも気にしない）", eval_data[3],
                                 ifelse(dataset[,5]=="あまり同意しない（できれば英語がいい）", eval_data[2],
                                        ifelse(dataset[,5]=="まったく同意しない（絶対に英語がいい）", eval_data[1], FALSE
                                        )))))

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
  age = new_age,
  experience = new_experience,
  lang = new_lang,
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

start_col <- 4
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

ggplot(new_table, aes(x = custom_types, y = scores, fill = custom_eval, label = scores)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette="Blues") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) + 
  labs(title="The Rate of Evaluation in Each Types",
       x = "Learning Types", y = "Numbers of Votes", 
       caption = paste("Total Votes:", sum(scores)/5),
       fill = "Evaluation")


  