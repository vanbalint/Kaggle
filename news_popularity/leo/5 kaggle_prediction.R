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

