library("dplyr")
library("tidyr")
library("ggplot2")
library("stringr")
library("fmsb")

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

#book_score = scoring(new_dataset[new_dataset$experience >= 4,]$book)
#text_score = scoring(new_dataset[new_dataset$experience >= 4,]$text)
#video_score = scoring(new_dataset[new_dataset$experience >= 4,]$video)
#live_score = scoring(new_dataset[new_dataset$experience >= 4,]$live)
#console_score = scoring(new_dataset[new_dataset$experience >= 4,]$console)

items <- c("book", "text", "video", "live", "console")

x2ratio <- function(x, byrow=FALSE) {
  if ( !is.matrix(x) ) return(NA)  #引数xが行列以外ならば実行しない
  if ( !is.logical(byrow) ) return(NA) #引数byrowが論理値以外ならば実行しない
  if (byrow == FALSE)  { return(     t(  t(x) / apply(x , 2, sum)  )  )
  }
  else return(    x / apply(x , 1, sum)  )
}

book_score = scoring(new_dataset[new_dataset$experience == 1,]$book)
(book_score[1] * 1 + book_score[2] * 2 + book_score[3] * 3 + book_score[4] * 4 +book_score[5] * 5)/length(new_dataset[new_dataset$experience == 1,]$book)

generate_generation_list <- function(ex){
  
  book_score = scoring(new_dataset[new_dataset$experience == ex,]$book)
  book_point = (book_score[1] * 1 + book_score[2] * 2 + book_score[3] * 3 + book_score[4] * 4 +book_score[5] * 5)/length(new_dataset[new_dataset$experience == 1,]$book)
  
  text_score = scoring(new_dataset[new_dataset$experience == ex,]$text)
  text_point = (text_score[1] * 1 + text_score[2] * 2 + text_score[3] * 3 + text_score[4] * 4 +text_score[5] * 5)/length(new_dataset[new_dataset$experience == 1,]$text)
  
  video_score = scoring(new_dataset[new_dataset$experience == ex,]$video)
  video_point = (video_score[1] * 1 + video_score[2] * 2 + video_score[3] * 3 + video_score[4] * 4 +video_score[5] * 5)/length(new_dataset[new_dataset$experience == 1,]$video)
  
  live_score = scoring(new_dataset[new_dataset$experience == ex,]$live)
  live_point = (live_score[1] * 1 + live_score[2] * 2 + live_score[3] * 3 + live_score[4] * 4 +live_score[5] * 5)/length(new_dataset[new_dataset$experience == 1,]$live)
  
  console_score = scoring(new_dataset[new_dataset$experience == ex,]$console_score)
  console_point = (console_score[1] * 1 + console_score[2] * 2 + console_score[3] * 3 + console_score[4] * 4 +console_score[5] * 5)/length(new_dataset[new_dataset$experience == 1,]$console_score)
  
  return(c(book_point, text_point, video_point, live_point, book_point))
}

generation_1_list <- generate_generation_list(1)
generation_2_list <- generate_generation_list(2)
generation_3_list <- generate_generation_list(3)
generation_4_list <- generate_generation_list(4)
generation_5_list <- generate_generation_list(5)
generation_4_list
generation_score_table = data.frame(
  book = c(generation_1_list[1], text = generation_2_list[1], video = generation_3_list[1], live = generation_4_list[1], console = generation_5_list[1]),
  text = c(generation_1_list[2], text = generation_2_list[2], video = generation_3_list[2], live = generation_4_list[2], console = generation_5_list[2]),
  video = c(generation_1_list[3], text = generation_2_list[3], video = generation_3_list[3], live = generation_4_list[3], console = generation_5_list[3]),
  live = c(generation_1_list[4], text = generation_2_list[4], video = generation_3_list[4], live = generation_4_list[4], console = generation_5_list[4]),
  console = c(generation_1_list[5], text = generation_2_list[5], video = generation_3_list[5], live = generation_4_list[5], console = generation_5_list[5])
)
rownames(generation_score_table) <- c("no_ex", "under 1", "1-3", "3-5", "over 5")
#generation_score_table <- generation_score_table * 0.1
generation_score_table
generation_score_table <- rbind(rep(20,5) , rep(0,5) , generation_score_table)
generation_score_table

generation_score_tablecolors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9), rgb(0.3,0.7,0.4,0.9), rgb(0.8,0.6,0.3,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.2), rgb(0.8,0.2,0.5,0.2) , rgb(0.7,0.5,0.1,0.2), rgb(0.3,0.7,0.4,0.2), rgb(0.8,0.6,0.3,0.2) )

radarchart(generation_score_table[-c(1,2),], 
           axistype=0 , maxmin=F,
           #custom polygon
           pcol=colors_border , pfcol=colors_in , plwd=2 , plty=1,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
           #custom labels
           vlcex=0.8 
)
legend(x=0.7, y=1, legend = rownames(generation_score_table[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)

#legend(x=0.7, y=1, legend = c("no_ex", "under 1", "1-3", "3-5", "over 5"), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)

#text(0.7,95,paste("Score:",book_mean),pos=1)
#text(1.9,95,paste("Score:",text_mean),pos=1)
#text(3.1,95,paste("Score:",video_mean),pos=1)
#text(4.3,95,paste("Score:",live_mean),pos=1)
#text(5.5,95,paste("Score:",console_mean),pos=1)

new_dataset[new_dataset$experience == 4,]$book

colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9), rgb(0.3,0.7,0.4,0.9), rgb(0.8,0.6,0.3,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.2), rgb(0.8,0.2,0.5,0.2) , rgb(0.7,0.5,0.1,0.2), rgb(0.3,0.7,0.4,0.2), rgb(0.8,0.6,0.3,0.2) )

data <- as.data.frame(matrix( sample( 0:20 , 25 , replace=T) , ncol=5))
colnames(data) <- items
rownames(data) <- paste("mister" , letters[1:5] , sep="-")
data <- rbind(rep(20,5) , rep(0,5) , data)
data
radarchart(data, pcol=colors_border , pfcol=colors_in , plwd=2 , plty=1)

