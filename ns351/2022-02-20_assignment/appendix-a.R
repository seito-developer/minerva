#Load libraries
library(ggplot2)
library(broom)
library(dplyr)

#Import dataset
file_path = "./assets/NS50 Scientific Proposal_ Science of Learning Data - Converted data .csv"
dataset <- read.csv(file_path, header = T)
head(dataset)
attach(dataset)
