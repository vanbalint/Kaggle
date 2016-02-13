source("3 kaggle_feature_engineering.R")
source("3.5 kaggle_feature_engineering.R")



######################
#Exploratory Analysis
######################




#################### correlation #######################
#check for autocorrelation to check whether removing redundant variables is needed or clustering opps
####### conclusion: removal is not needed

cor_df <- cor(df[,-c(1,2)])

corrplot(cor_df, 
         method="square", 
         order = "hclust", 
         tl.cex=0.5, tl.col="black", tl.srt=45, 
         main="Correlation Plot")

#################### plots #######################


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



#################### pca #######################
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



#interpretation
#popularity values are significant
#positive vs. negative coordinates

#PC1:related with low vs. quantity of words, entertainment vs world, LDA 3&1 vs LDA 2 (respectively correlated)
#PC2:related with LDA 4 vs. LDA2, tech vs. entertainment
#PC3:related with socmed vs entertainment, LDA 00, LDA 01

#popularity 4,5,3 have positive coordinates on PC1, while 1,2 have negative coordinates. Out of this, the contribution of 4,5,3 is highest
#popularity 2,3 have positive coordinates on PC2, while 1,4,5 have negative coordinates. Out of this, the contribution of 2,1 is highest
#popularity 2,3,4,5 have positive coordinates on PC3, while 1 has negative coordinates. Out of this, the contribution of 3,5,1 is highest

#cumulative variance (14, 26, 36) respectively

#interpretation
#supplementary categories: Dist=distance from center of gravity, V.test=significant if greater than abs(2)

#arg: file="asdf.txt" writes file
#arg: habillage=number of column or "variable name" (the supplementary categorial variable)
#arg: invisible=vector of strings name of variables to omit from plot
#arg: select= from results, "cos2 0.7" greater than 0.7 in cos2 metric
#arg: unselected= from 0 to 1 to change transparency of unselected items (to be used with select argument)

df9<- data.frame(re= sample(c("white","afam","carib"),20,replace=TRUE), usborn= sample(c("yes","no"),20,replace=TRUE),stringsAsFactors=FALSE) 

df8<-within(df9,{native3<- 1*(re=="carib" & usborn=="no"); native2<- 1*(re=="carib" & usborn=="yes"); native1<- 1*(re=="afam" & usborn=="yes"); native0<- 1*(re=="white" & usborn=="yes")}) 

library(reshape2) 
df10<- melt(df8,id.vars=c("re","usborn")) 
df10New<-df10[df10$value==1,-4] 
df10New$variable<-as.numeric(gsub("[[:alpha:]]","",df10New$variable)) 
colnames(df10New)[3]<- "native" 
row.names(df10New)<- 1:nrow(df10New) 

df3<-melt(df1)
?

str(df2.norm_train)



df4<-df3

factorize_category<-c("data_channel_is_entertainment", 
               "data_channel_is_socmed",
               "data_channel_is_tech",
               "data_channel_is_world")  
nonfactorize<-setdiff(names(df4), factorize_category)

df4<-melt(df4, id.vars=nonfactorize, measure.vars=factorize_category)
