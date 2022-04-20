library("dplyr")
library("tidyr")
library("ggplot2")
library("stringr")
library(RColorBrewer)

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
new_live <- c(convert_eval_to_score(dataset[,10]))
new_console <- c(convert_eval_to_score(dataset[,9]))

# build new dataset
new_dataset <- data.frame(
  age = new_age,
  experience = new_experience,
  lang = new_lang,
  book = new_book,
  text = new_text,
  video = new_video,
  live = new_live,
  console = new_console
)

scoring <- function(item){
  score <- c()
  for (index in 1:5) {
    score = c(score, sum(item==eval_data[index]))
  }
  return(score)
}

book_score = scoring(new_dataset$book)
text_score = scoring(new_dataset$text)
video_score = scoring(new_dataset$video)
console_score = scoring(new_dataset$console)
live_score = scoring(new_dataset$live)

items <- c("book", "text", "video", "console", "live")

new_table = data.frame(
  strong_disagree =  c(book_score[1], text_score[1], video_score[1], console_score[1], live_score[1]),
  disagree =  c(book_score[2], text_score[2], video_score[2], console_score[2], live_score[2]),
  neigther =  c(book_score[3], text_score[3], video_score[3], console_score[3], live_score[3]),
  agree =  c(book_score[4], text_score[4], video_score[4], console_score[4], live_score[4]),
  strong_agree =  c(book_score[5], text_score[5], video_score[5], console_score[5], live_score[5])
)
new_table

x2ratio <- function(x, byrow=FALSE) {
  if ( !is.matrix(x) ) return(NA)  #引数xが行列以外ならば実行しない
  if ( !is.logical(byrow) ) return(NA) #引数byrowが論理値以外ならば実行しない
  if (byrow == FALSE)  { return(     t(  t(x) / apply(x , 2, sum)  )  )
  }
  else return(    x / apply(x , 1, sum)  )
}

colnames(new_table) <- rev(eval)
rownames(new_table) <- items
par(xpd=TRUE) 
plot <- barplot(t(new_table), legend = FALSE, main="The Rate of Evaluation in Each Types", xlab="Learning Types", ylab="Number of each scores", col = rev(brewer.pal(5, "Blues")))
legend("topright", legend=eval, fill=brewer.pal(5, "Blues"))

dividing_eval <- function(target){
  disagree <- new_table[target,1] + new_table[target,2]
  agree <- new_table[target,4] + new_table[target,5]
  return(c(disagree, agree))
}

division_table = data.frame(
  book = dividing_eval(1),
  text = dividing_eval(2),
  video = dividing_eval(3),
  console = dividing_eval(4),
  live = dividing_eval(5)
)
division_table
rownames(division_table) <- c("dis_agree", "agree")
division_table
t(new_table)
plot <- barplot(t(t(division_table)), legend = FALSE, main="The Rate of Evaluation in Each Types", xlab="Learning Types", ylab="Number of each scores", col = rev(c("#BDD7E7","#3182BD")))
legend("topright", legend=c("agree", "dis_agree"), fill=c("#BDD7E7","#3182BD"))
brewer.pal(5, "Blues")
           