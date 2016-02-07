# install.packages('randomForest')
library(randomForest)

news_popularity_training <- read.csv("news_popularity_training.csv")
news_popularity_test<- read.csv("news_popularity_test.csv")

randomforest1 <- randomForest(as.factor(popularity) ~ .,
                              data=news_popularity_training[,-(1:2)], ntree=400)

popularity_prediction <- predict(randomforest1, news_popularity_test)
submit <- data.frame(id = news_popularity_test$id, popularity = as.numeric(popularity_prediction))
write.csv(submit, file = "new_pop_prediction_randomforest1.csv", row.names = FALSE)

# popularity_prediction <- predict(randomforest1, news_popularity_training[20001:30000,-(1:2)])
# 
# table(popularity_prediction, news_popularity_training$popularity[20001:30000])
# sum(popularity_prediction == news_popularity_training$popularity[20001:30000])/10000


