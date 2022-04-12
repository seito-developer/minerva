file_path = "./dataset.csv"
dataset <- read.csv(file_path, header = T)
head(dataset)

#Arrange dataset

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
new_age
