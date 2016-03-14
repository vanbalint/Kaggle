source("1 kaggle_load.R")

###############
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

###################
# strip plot function of classes
#####################


png("#images/strip_is_weekend/stripplot_%03d.png")
for(i in 1:length(df2.norm_train)) {
  x=df2.norm_train[,i]
  y=df2.norm_train[,"popularity"]
  z=df2.norm_train[,"is_weekend"]
  
  myPlot <- stripplot(y ~ x,  
                      jitter.data = TRUE, alpha = 0.6,
                      groups= z, 
                      auto.key=list(title = "is_weekend", columns = 2),
                      xlab = names(df2.norm_train)[i], ylab = "Popularity")
  print(myPlot)
}
dev.off()



#################### 
#pca 
#######################
gbm<-read.csv("#data frames/VarImpTable_GBM.csv")
rf<-read.csv("#data frames/VarImpTable_rf.csv")

df3<-df2.norm_train
df3$popularity<-as.factor(df3$popularity)

head(gbm)
gbm_name<-as.vector(gbm[1:25,1]) #variables with weights higher than 10
e<-match(gbm_name, names(df3))
df3<-df3[,c(e,ncol(df3))]

#remove time and highly correlated variables
nullify <- c("time", "timedelta", "kw_max_avg", "kw_max_min", "kw_min_avg", "n_unique_tokens", "self_reference_min_shares")
df3<-df3[,setdiff(names(df3), nullify)]

#pca
pca<-PCA(df3, quali.sup=ncol(df3))
summary(pca, nbelements=Inf)
summary(pca, nbelements=Inf, file="#data frames/pca.txt") 

#individuals factor map
##raw
png("#images/PCA_Observations_raw.png",width=10, height=8, units="in", res=1200)
plot(pca, cex=0.5, habillage=ncol(df3))
dev.off()
##only popularity
plot(pca, habillage=ncol(df3), invisible="ind")


#variables factor map
plot(pca, choix="var", cex=0.8, select=" contrib 10") #top 10 (threshold chosen stepwise)


png("#images/PCA_combined.png", width=10, height=8, units="in", res=1200)
par(mfrow=c(1,2))
plot(pca, choix="var", cex=0.8, select=" contrib 10") #top 10 (threshold chosen stepwise)
plot(pca, habillage=ncol(df3), invisible="ind")
dev.off()


##################3
#check distribution to check whether log transformation is necessary
##################
df4<-read.csv("df4.csv")

df4.rm.factor<-df4[,-c(1,2,62,63,65)]
feat<-names(df4.rm.factor)

png("#images/density2/density_%03d.png")
for(i in 1:length(df4.rm.factor)) {
  
  myPlot<-densityplot(df4.rm.factor[,i], xlab=paste("Density for", feat[i], sep=" "), main=paste("Density for", feat[i], sep=" "))
  print(myPlot)
}
dev.off
