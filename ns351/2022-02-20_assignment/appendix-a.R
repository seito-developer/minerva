#Load libraries
library(ggplot2)
library(broom)
library(dplyr)

#Import dataset
file_path = "./assets/ns-arranged-data.csv"
dataset <- read.csv(file_path, header = T)
head(dataset)


courses <-c("In-person Courses", "Online Courses")

mode <- c("Live classes (ie: Zoom, Google Meet etc.)", "Recorded Lectures/Videos", "Recorded Lectures", "Uploaded or emailed Materials", "Discussion forums/chats")

###
## Convert values "In-person Courses/Online Courses" in Preferred course to numbers 1/0
## And add the new row "Expected.online"
###
row_num <- nrow(dataset)
i <- 1
expected_online_vals <- c()
while (i < row_num + 1) {
  if(dataset$Preferred.course[i] == courses[1]) {
    expected_online_vals <- c(expected_online_vals, 1)
  } else {
    expected_online_vals <- c(expected_online_vals, 0)
  }
  i <- i+1
}

new_col <- data.frame(expected_online_vals)
names(new_col) <- c("Expected.online")
dataset <- cbind(dataset, new_col)
head(dataset)

# remake data set from the original without unnecessary rows
new_dataset <-c(dataset[3],dataset[4],dataset[5],dataset[6],dataset[7],dataset[8],dataset[9],dataset[10],dataset[11],dataset[12],dataset[13],dataset[14],dataset[15])
result_all <- glm(formula = dataset$Expected.online ~ . , data = new_dataset, family="binomial")
summary(result_all)

#Coefficients:
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                                                        2.37332    1.96710   1.207 0.227622    
#Current.online.coursesLive classes (ie: Zoom, Google Meet etc.)    1.38269    1.02848   1.344 0.178821    
#Current.online.coursesRecorded Lectures                            1.34558    1.02460   1.313 0.189088    
#Current.online.coursesUploaded or emailed Materials                1.82831    1.12010   1.632 0.102620    
#Preffered.online.coursesLive classes (ie: Zoom, google meet etc.)  0.45753    0.99392   0.460 0.645281    
#Preffered.online.coursesRecorded Lectures/Videos                  -0.08534    0.98767  -0.086 0.931145    
#Preffered.online.coursesUploaded or emailed Materials              1.03534    1.29158   0.802 0.422781    
#Enjoy.online                                                      -1.54837    0.28434  -5.445 5.17e-08 ***
#Motivated.online                                                  -0.45399    0.21414  -2.120 0.033997 *  
#Satisfied.online                                                  -0.10594    0.20579  -0.515 0.606704    
#Engaging.online                                                    0.05417    0.23178   0.234 0.815220    
#Distracted.online                                                 -0.10375    0.18473  -0.562 0.574366    
#Actions.online                                                     0.16796    0.16258   1.033 0.301548    
#Enjoy.offline                                                      1.13119    0.30395   3.722 0.000198 ***
#Motivated.offline                                                 -0.47902    0.28903  -1.657 0.097451 .  
#Satisfied.offline                                                  0.57571    0.29719   1.937 0.052720 .  
#Engaging.offline                                                   0.11184    0.31739   0.352 0.724549    
#Distracted.offline                                                -0.33550    0.17041  -1.969 0.048981 *  



result <- lm(formula = totalPr ~ wheels + cond, data=new_dataset)