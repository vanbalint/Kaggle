source("2 kaggle_clean.R")


###################################
#FEATURE ENGINEERING
####################################3



#url length
df2$url<-as.character(df2$url)
df2$urlLength<-nchar(df2$url[1])


#dates
df2$time = substr(df2$url,21,30); df2$url<-NULL; df2$id<-NULL
df2$time <-ymd(df2$time)
df2$year <- factor(year(df2$time))
df2$month <-factor(month(df2$time, label=TRUE, abbr=TRUE), ordered=FALSE)

###convert data object to numeric
df2$time <-as.numeric(df2$time)

###dummify time
dfDummy <- dummyVars("~.",data=df2, fullRank=F)
df2 <- as.data.frame(predict(dfDummy,df2))
print(names(df2))






##################################################

#place outcome column last
outcomeName <- "popularity"
featureName <- names(df2)[names(df2) !=outcomeName]
df2<-df2[,c(featureName, outcomeName)]

#separate train/test set
df2_train<-df2[1:30000,]
df2_test<-df2[30001:nrow(df2),-ncol(df2)]

