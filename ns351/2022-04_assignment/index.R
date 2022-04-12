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
new_lang <- c()
new_lang <- ifelse(dataset[,5]=="強く同意する（絶対に日本語がいい）", 5,
                   ifelse(dataset[,5]=="やや同意する（できれば日本語がいい）", 4,
                          ifelse(dataset[,5]=="どちらとも言えない（日本語でも英語でも気にしない）", 3,
                                 ifelse(dataset[,5]=="あまり同意しない（できれば英語がいい）", 2,
                                        ifelse(dataset[,5]=="まったく同意しない（絶対に英語がいい）", 1, FALSE
                                        )))))
                         
## types
convert_eval_to_score <- function(target_col){
  new_col <- ifelse(target_col=="強く同意する", 5,
                ifelse(target_col=="やや同意する", 4,
                       ifelse(target_col=="どちらとも言えない", 3,
                              ifelse(target_col=="あまり同意しない", 2,
                                     ifelse(target_col=="まったく同意しない", 1, FALSE
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
new_dataset


