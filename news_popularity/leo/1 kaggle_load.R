#set wd
setwd("E:/spain/BGSE/term 2/kaggle")

# load libraries
if (!require("caret")) install.packages("caret", dependencies = c("Depends", "Suggests")); library(caret)
if (!require("corrplot")) install.packages("corrplot"); library(corrplot)
if (!require("plyr")) install.packages("plyr"); library(plyr)
if (!require("fscaret")) install.packages("fscaret", dependencies = c("Depends", "Suggests")); library(fscaret)
if (!require("lubridate")) install.packages("lubridate"); library(lubridate)
if (!require("dplyr")) install.packages("dplyr"); library(dplyr)
if (!require("lattice")) install.packages("lattice"); library(lattice)
if (!require("randomForest")) install.packages("randomForest"); library(randomForest)
if (!require("FactoMineR")) install.packages("FactoMineR"); library(FactoMineR)
if (!require("tidyr")) install.packages("tidyr"); library(tidyr)
if (!require("rvest")) install.packages("rvest"); library(rvest)


#load scripts
source("6 kaggle_functions.R")


# load data sets
df <- read.csv("#data sets/news_popularity_training.csv")
df_test <- read.csv("#data sets/news_popularity_test.csv")
df_test$popularity <- rep(NA, nrow(df_test))
df2<-rbind(df,df_test)
