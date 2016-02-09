# install.packages('ranger')
library(ranger)

news_popularity_training_and_wordcount<- read.csv("news_popularity_training_and_wordcount.csv")
news_popularity_test_and_wordcount <- read.csv("news_popularity_test_and_wordcount.csv")

news_popularity_training_and_wordcount$popularity <- as.factor(news_popularity_training_and_wordcount$popularity)
randomforestwords1 <- ranger(popularity ~ ., write.forest = TRUE,
                             data=news_popularity_training_and_wordcount[,-(1:2)], num.trees=500)
rm(news_popularity_training_and_wordcount)
popularity_prediction <- predict(randomforestwords1, news_popularity_test_and_wordcount)
submit <- data.frame(id = news_popularity_test$id, popularity = as.numeric(popularity_prediction$predictions))
write.csv(submit, file = "new_pop_prediction_randomforestwords1.csv", row.names = FALSE)


# #trying how well the additional variables work
# news_popularity_training <- read.csv("news_popularity_training.csv")
# news_popularity_test<- read.csv("news_popularity_test.csv")
# news_popularity_training$popularity <- as.factor(news_popularity_training$popularity)
# randomforest1 <- ranger(popularity ~ ., write.forest = TRUE,
#                              data=news_popularity_training[10000:20000,-(1:2)], num.trees=100)
# popularity_prediction <- predict(randomforest1, news_popularity_training[20001:30000,-(1:2)])
# table(popularity_prediction$predictions, news_popularity_training$popularity[20001:30000])
# sum(popularity_prediction$predictions == news_popularity_training$popularity[20001:30000])/10000
# 
# news_popularity_training_and_wordcount$popularity <- as.factor(news_popularity_training_and_wordcount$popularity)
# randomforestwords1 <- ranger(popularity ~ ., write.forest = TRUE,
#                         data=news_popularity_training_and_wordcount[10000:20000,-(1:2)], num.trees=300)
# popularity_prediction <- predict(randomforestwords1, news_popularity_training_and_wordcount[20001:30000,-(1:2)])
# table(popularity_prediction$predictions, news_popularity_training$popularity[20001:30000])
# sum(popularity_prediction$predictions == news_popularity_training$popularity[20001:30000])/10000
