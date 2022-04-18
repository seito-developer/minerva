library("dplyr")
library("tidyr")
library("ggplot2")
library("stringr")

file_path = "./dataset.csv"
dataset <- read.csv(file_path, header = T)
head(dataset)

#### Arrange dataset ####

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
# lang
eval_data <- c("strong_disagree", "disagree", "neigther", "agree", "strong_agree")

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

book_score = scoring(new_dataset$book)
text_score = scoring(new_dataset$text)
video_score = scoring(new_dataset$video)
live_score = scoring(new_dataset$live)
console_score = scoring(new_dataset$console)

items <- c("book", "text", "video", "live", "console")

new_table = data.frame(
  strong_disagree =  c(book_score[1], text_score[1], video_score[1], live_score[1], console_score[1]),
  disagree =  c(book_score[2], text_score[2], video_score[2], live_score[2], console_score[2]),
  neigther =  c(book_score[3], text_score[3], video_score[3], live_score[3], console_score[3]),
  agree =  c(book_score[4], text_score[4], video_score[4], live_score[4], console_score[4]),
  strong_agree =  c(book_score[5], text_score[5], video_score[5], live_score[5], console_score[5])
)
new_table

colnames(new_table) <- rev(eval)
rownames(new_table) <- items
par(xpd=TRUE) 
plot <- barplot(t(new_table), legend = FALSE, main="Histogram", xlab="Learning Types", ylab="Number of each scores")
#main="Histogram", xlab="range", ylim=c(0,30), col="#993435")

book_mean <- sum(new_table[1,])
text_mean <- sum(new_table[2,])
video_mean <- sum(new_table[3,])
live_mean <- sum(new_table[4,])
console_mean <- sum(new_table[5,])

text(0.7,95,paste("Score:",book_mean),pos=1)
text(1.9,95,paste("Score:",text_mean),pos=1)
text(3.1,95,paste("Score:",video_mean),pos=1)
text(4.3,95,paste("Score:",live_mean),pos=1)
text(5.5,95,paste("Score:",console_mean),pos=1)

legend("topright", legend=rev(eval), fill=c("#000000", "#333333", "#666666", "#999999", "#eeeeee"))

