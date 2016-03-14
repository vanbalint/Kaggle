setwd("E:/spain/BGSE/term 2/kaggle")

source("2 kaggle_clean.R")
df3<-read.csv("#data setes/df3_complete.csv")

########################
#create subject score feature
#######################

#aggregate sum
lifestyle.sum<-sum(df3["data_channel_is_lifestyle"]); lifestyle.sum
entertainment.sum<-sum(df3["data_channel_is_entertainment"]); entertainment.sum
bus.sum<-sum(df3["data_channel_is_bus"]); bus.sum
socmed.sum<-sum(df3["data_channel_is_socmed"]); socmed.sum
tech.sum<-sum(df3["data_channel_is_tech"]); tech.sum
world.sum<-sum(df3["data_channel_is_world"]); world.sum

sum(lifestyle.sum,entertainment.sum,bus.sum, socmed.sum, tech.sum, world.sum)

length(which(df3$data_channel_is_lifestyle == 0 
             & df3$data_channel_is_entertainment==0
             & df3$data_channel_is_bus ==0 
             & df3$data_channel_is_socmed ==0 
             & df3$data_channel_is_tech==0 
             & df3$data_channel_is_world==0)) #6134 obs without category

length(which(df3$data_channel_is_lifestyle[30001:nrow(df3)] == 0 
             & df3$data_channel_is_entertainment[30001:nrow(df3)]==0
             & df3$data_channel_is_bus[30001:nrow(df3)] ==0 
             & df3$data_channel_is_socmed[30001:nrow(df3)] ==0 
             & df3$data_channel_is_tech[30001:nrow(df3)]==0 
             & df3$data_channel_is_world[30001:nrow(df3)]==0)) #1476 obs without category in test set

####create subject called other
without.category.index<-which(df3$data_channel_is_lifestyle == 0 
                              & df3$data_channel_is_entertainment==0
                              & df3$data_channel_is_bus ==0 
                              & df3$data_channel_is_socmed ==0 
                              & df3$data_channel_is_tech==0 
                              & df3$data_channel_is_world==0)
df3$data_channel_is_other<-rep(0,nrow(df3))
df3$data_channel_is_other[without.category.index]<-1

other.sum<-sum(df3["data_channel_is_other"]); other.sum

sum(lifestyle.sum,entertainment.sum,bus.sum, socmed.sum, tech.sum, world.sum, other.sum)

#### compute subject score
df3$category_score<-rep(NA,nrow(df3))

lifestyle.index<-which(df3$data_channel_is_lifestyle==1)
df3$category_score[lifestyle.index] <- df3$shares.full[lifestyle.index] / lifestyle.sum

entertainment.index<-which(df3$data_channel_is_entertainment==1)
df3$category_score[entertainment.index] <- df3$shares.full[entertainment.index] / entertainment.sum

bus.index<-which(df3$data_channel_is_bus==1)
df3$category_score[bus.index] <- df3$shares.full[bus.index] / bus.sum

socmed.index<-which(df3$data_channel_is_socmed==1)
df3$category_score[socmed.index] <- df3$shares.full[socmed.index] / socmed.sum

tech.index<-which(df3$data_channel_is_tech==1)
df3$category_score[tech.index] <- df3$shares.full[tech.index] / tech.sum

world.index<-which(df3$data_channel_is_world==1)
df3$category_score[world.index] <- df3$shares.full[world.index] / world.sum

other.index<-which(df3$data_channel_is_other==1)
df3$category_score[other.index] <- df3$shares.full[other.index] / other.sum


#clean up a bit
df4<-df3

df4$X.1<-df4$X<-df4$id<-df4$timedelta<-df4$numb.char<-NULL
df4$author<-as.factor(df4$author)

write.csv(df4,"#data sets/df4.csv")


#############################################
#eliminate no.of shares and category score feature
##############################################
df4$shares.full<-df4$category_score<-NULL


########################
#create logged features
#######################

logged.index<-c(2,3,4,5,6,7,8,9,10,19,21,25,26,27,28,29,50)

logged.matrix<-matrix(NA,nrow(df4), length(logged.index))

for (i in 1:ncol(logged.matrix)){
  index<-logged.index[i]
  logged.matrix[,i]<-log(df4.rm.factor[,index]+1)
}

df4<-cbind(df4, as.data.frame(logged.matrix))



#write.csv(df4,"df4_before_standardizing_and_dummifying_withoutshares.csv")

#############
#standardize
###############
omit<-c("url", "popularity", "author")

df5<-as.data.frame(apply(df4[,setdiff(names(df4), omit)],2,scale))

df5<-cbind(df5, df4[,omit])

##############
#create dates features
#############
df5$time = substr(df5$url,21,30) 
df5$time <-ymd(df5$time)
df5$df5 <- factor(year(df5$time))
df5$month <-factor(month(df5$time, label=TRUE, abbr=TRUE), ordered=FALSE)

###clean a bit more
time<-df5$time
url<-df5$url
author<-df5$author
df5$time<-df5$url<-df5$author<-NULL

###dummify time and add back date and url
dfDummy.norm <- dummyVars("~.",data=df5, fullRank=F)
df5 <- as.data.frame(predict(dfDummy.norm,df5))

#place outcome column last
outcomeName <- "popularity"
featureName <- names(df5)[names(df5) !=outcomeName]
df5<-df5[,c(featureName, outcomeName)]

write.csv(df5,"#data sets/df5_without_author_withoutshares.csv")
##################################################
