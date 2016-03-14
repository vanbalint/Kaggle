source("kaggle_exploratory_analysis.R")
source("kaggle_functions.R")

####### Balint's RF ##########
randomforest1 <- randomForest(as.factor(popularity) ~ .,
                              data=news_popularity_training[,-(1:2)], ntree=400)

popularity_prediction <- predict(randomforest1, news_popularity_test)
submit <- data.frame(id = news_popularity_test$id, popularity = as.numeric(popularity_prediction))

write.csv(submit, file = "#predictions/new_pop_prediction_randomforest1.csv", row.names = FALSE)

varImpPlot(randomforest1)
plot(randomforest1)

u<-varImp(randomforest1, scale=F)
results2<-data.frame(row.names(u), u$Overall)
colnames(results2) <- c('VariableName','Weight')
results2 <- results2[order(results2$Weight, decreasing=TRUE), ]

write.csv(results2, file = "#data frames/VarImpTable_rf.csv", row.names = FALSE)


#############################
# RF, randomized, balanced class (original features), same parameters as Balint's first rf
#############################
Balanced_5<-ClassBalance(news_popularity_training, targetClass="5", 0.5)
Balanced_4<-ClassBalance(Balanced_5, targetClass="4", 0.5)
df_balanced<-Balanced_4


randomforest1 <- randomForest(as.factor(popularity) ~ .,
                              data=df_balanced[,-(1:2)], ntree=400)

popularity_prediction <- predict(randomforest1, news_popularity_test)
submit <- data.frame(id = news_popularity_test$id, popularity = as.numeric(popularity_prediction))

write.csv(submit, file = "#predictions/new_pop_prediction_randomforest2.csv", row.names = FALSE)







####### GBM ##########

objControl <- trainControl(method='cv')
objModel <- train(x=df2_train[,-ncol(df2_train)], y=df2_train[,ncol(df2_train)], 
                  method="gbm", 
                  metric="Accuracy",
                  trControl=objControl  
)

# see relative importance of variables 
summary(objModel)

# see tuning parameters
print(objModel)


#The final values used for the model were n.trees = 150, interaction.depth = 2, shrinkage = 0.1 and n.minobsinnode = 10. 

# plot variable importance
varImp <- varImp(objModel, scale=F)
plot(varImp, main="Variable Importance (GBM)")


#varImp table
results1 <- data.frame(row.names(varImp$importance),varImp$importance$Overall)
colnames(results1) <- c('VariableName','Weight')
results1 <- results1[order(results1$Weight, decreasing=TRUE), ]
results1$VariableName <- as.character(results1$VariableName)

write.csv(results1, file = "#data frames/VarImpTable_GBM.csv", row.names = FALSE)

#predict
predictions <- predict(object=objModel, df2_test, type='raw')
submit <- data.frame(id = df_test$id, popularity = as.numeric(predictions))
write.csv(submit, file = "#predictions/new_pop_prediction_gbm.csv", row.names = FALSE)



####### GBM vs RF ##########

gbm<-read.csv("#data frames/VarImpTable_GBM.csv")
rf<-read.csv("#data frames/VarImpTable_rf.csv")

varimp_combined<-merge(gbm, rf, by.x="VariableName", by.y="VariableName")
names(varimp_combined)<-c("VariableName", "GBM", "RF")
varimp_combined<-varimp_combined[order(varimp_combined$RF, decreasing=TRUE),]

#plot
png("#images/VarImp_comparison.png",
    width     = 10,
    height    = 9.5,
    units     = "in",
    res       = 1200,
    pointsize = 4
)
barchart(reorder(VariableName, RF) ~ GBM+RF, data = varimp_combined, 
         stack = TRUE, layout = c(1, 1),
         auto.key = list(title = "Variable Importance Comparison", columns = 2))
dev.off()



#####
#more predictions
#######

#prepare training/prediction with data set with extra features

df5<-read.csv("#data sets/df5_without_author_withoutshares.csv")

#call back author name feature
author<-read.csv("#data sets/author_name.csv", colClasses = "character")
author[,1]<-NULL

#remove id
df5$X<-NULL

#separate train/test set
df5.train<-df5[1:30000,]; 
df5.train$popularity<-as.factor(df5.train$popularity)

df5.test<-df5[30001:nrow(df5),-ncol(df5)]

#remove outlier identified in distribution plots
df5.train<-df5.train[-22686,]

################
#gbm with author features only + best 1 feature
###############
df7<-df5

keep<-c("kw_max_max", "popularity")
df7<-df7[keep]
df7<-cbind(df7,author)
df7<-df7[,c("kw_max_max", "author", "popularity")]

dfDummy.norm2 <- dummyVars("~.",data=df7, fullRank=F)
df7 <- as.data.frame(predict(dfDummy.norm2,df7))
df7$popularity<-as.factor(df7$popularity)


df7.train<-df7[1:30000,]; 
df7.test<-df7[30001:nrow(df7),-ncol(df7)]

#remove X and outlier
objControl7 <- trainControl(method='cv')
objModel7 <- train(x=df7.train[,-ncol(df7.train)], y=df7.train[,ncol(df7.train)], 
                   method="gbm", 
                   metric="Accuracy",
                   trControl=objControl  
)

The final values used for the model were n.trees = 50, interaction.depth = 2, shrinkage = 0.1 and n.minobsinnode = 10. 

predictions.is7 <- predict(object=objModel7, df7.train[,-ncol(df7.train)], type='raw')
actual7<-as.numeric(df7.train[,ncol(df7.train)])

table(actual7==predictions.is7)[2] / (table(actual7==predictions.is7)[2] + table(actual7==predictions.is7)[1])
TRUE 
0.4613487

#extract important variable
varimp3 <- varImp(objModel7, scale=F)
plot(varimp3, main="Variable Importance (GBM)")

results <- data.frame(row.names(varimp3$importance),varimp3$importance$Overall)
colnames(results) <- c('VariableName','Weight')
results <- results[order(results$Weight, decreasing=TRUE),]
results$VariableName<-as.character(results$VariableName)

keep2<-results$VariableName[results$Weight>0]
keep2<-keep2[-1]
write.csv(keep2, "#data sets/important_variable_authors.csv")





################
# random forest full feature set+stratified sampling
################
df8<-df5[,c(keep, "popularity")]
bestauthor.df<-df7[,keep2]

df8<-cbind(bestauthor.df,df8)
write.csv(df8,"#data sets/df_bestvariables.csv")
df8<-read.csv("#data sets/df_bestvariables.csv")

df8.train<-df8[1:30000,]; 
df8.test<-df8[30001:nrow(df8),-ncol(df8)]

#remove X and outlier
df8.train$X<-NULL
df8.train$popularity<-as.factor(df8.train$popularity)

nmin<-min(table(df8.train$popularity))

ctrl <- trainControl(method = "cv")

df8.down<-train(popularity ~ ., data = df8.train,
                method = "rf",
                metric = "Accuracy",
                ntree = 1500,
                tuneLength = 5,
                trControl = ctrl,
                strata = df8.train$popularity,
                sampsize = rep(nmin, 5))


df8.fit<-predict(df8.down, df8.train[,-ncol(df8.train)], type="raw")
actual8<-as.numeric(df8.train[,ncol(df8.train)])

table(actual8==df8.fit)[2] / (table(actual8==df8.fit)[2] + table(actual8==df8.fit)[1])

TRUE 
0.3834128 

df8.fit<-predict(df8.down, df8.test)
submit<-data.frame(id = df_test$id, popularity = as.numeric(df8.fit))

write.csv(submit, file = "#predictions/df8_downsampled_rf_bestvariables.csv", row.names = FALSE)



#############################################
df8<-read.csv("#data sets/df_bestvariables.csv")
df8$X<-NULL

df8.train<-df8[1:10000,]; 
df8.test<-df8[30001:nrow(df8),-ncol(df8)]

#remove X and outlier
df8.train$popularity<-as.factor(df8.train$popularity)

objControl8 <- trainControl(method = "cv")
gbmGrid<-expand.grid(interaction.depth = (1:5) * 2,
                     n.trees = (1:10)*25, shrinkage = .1, n.minobsinnode=5:15)


objModel8 <- train(popularity~.,data=df8.train,
                   method="gbm", 
                   metric="Accuracy",
                   trControl=objControl8,
                   tuneGrid=gbmGrid
)


