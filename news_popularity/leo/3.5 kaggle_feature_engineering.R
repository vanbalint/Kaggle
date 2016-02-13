
source("2 kaggle_clean.R")


###################################
#FEATURE ENGINEERING
####################################3





#normalize
omit<-c("id", "url", "popularity")
df2.norm<-as.data.frame(lapply(df2[,setdiff(names(df2), omit)], normalize))
df2.norm<-cbind(df2.norm, df2[,omit])

#url length
df2.norm$url<-as.character(df2.norm$url)
df2.norm$urlLength<-nchar(df2.norm$url[1])


#dates
df2.norm$time = substr(df2.norm$url,21,30); df2.norm$url<-NULL; df2.norm$id<-NULL
df2.norm$time <-ymd(df2.norm$time)
df2.norm$year <- factor(year(df2.norm$time))
df2.norm$month <-factor(month(df2.norm$time, label=TRUE, abbr=TRUE), ordered=FALSE)

###convert data object to numeric
df2.norm$time <-as.numeric(df2.norm$time)

###dummify time
dfDummy.norm <- dummyVars("~.",data=df2.norm, fullRank=F)
df2.norm <- as.data.frame(predict(dfDummy.norm,df2.norm))
print(names(df2.norm))






##################################################

#place outcome column last
outcomeName <- "popularity"
featureName <- names(df2.norm)[names(df2.norm) !=outcomeName]
df2.norm<-df2.norm[,c(featureName, outcomeName)]

#separate train/test set
df2.norm_train<-df2.norm[1:30000,]
df2.norm_test<-df2.norm[30001:nrow(df2.norm),-ncol(df2)]

