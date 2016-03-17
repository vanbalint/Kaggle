# install.packages('ranger')
library(ranger)

news_popularity_training_and_wordcount<- read.csv("news_popularity_training_and_wordcount.csv")
news_popularity_test_and_wordcount <- read.csv("news_popularity_test_and_wordcount.csv")

word_frequency_test <- colSums(news_popularity_test_and_wordcount[,63:3309])
useful_words <- word_frequency_test>5
training_wordcount <- news_popularity_training_and_wordcount[,64:3310]
test_wordcount <- news_popularity_test_and_wordcount[,63:3309]
training_wordcount <- training_wordcount[,useful_words]
test_wordcount <- test_wordcount[,useful_words]

#removing not relevant words for the prediction
wordmatrix <- rbind(news_popularity_training_and_wordcount[,64:3310],
                    news_popularity_test_and_wordcount[,63:3309])
wordmatrix <- wordmatrix[useful_words]
library(cluster)
distwordmatrix <- dist(t(wordmatrix))
write.csv(as.matrix(distwordmatrix), "distwordmatrix")

distwordmatrix <- read.csv("distwordmatrix")
distwordmatrix <- distwordmatrix[,-1]
distwordmatrix <- as.dist(distwordmatrix)
clustered <- hclust(distwordmatrix)
groups <- cutree(clustered, k=100)
wordclustercount_training <- matrix(0, nrow(news_popularity_training_and_wordcount), 100)
for(row in 1:nrow(training_wordcount)){
    for(col in 1:length(training_wordcount)){
        wordclustercount_training[row, groups[col]]<- training_wordcount[row, col]>0
    }
    cat(row, "\n")
}
wordclustercount_training <- as.data.frame(wordclustercount_training)
wordclustercount_test <- matrix(0, nrow(news_popularity_test_and_wordcount), 100)
for(row in 1:nrow(test_wordcount)){
    for(col in 1:length(test_wordcount)){
        wordclustercount_test[row, groups[col]]<- test_wordcount[row, col]>0
    }
}
wordclustercount_test <- as.data.frame(wordclustercount_test)

news_popularity_training_wordcluster <- cbind(news_popularity_training, wordclustercount_training)
news_popularity_test_wordcluster <- cbind(news_popularity_test, wordclustercount_test)
write.csv(news_popularity_training_wordcluster, "news_popularity_training_wordcluster.csv")
write.csv(news_popularity_test_wordcluster, "news_popularity_test_wordcluster.csv")


news_popularity_training_wordcluster <- read.csv("news_popularity_training_wordcluster.csv")
news_popularity_test_wordcluster <- read.csv("news_popularity_test_wordcluster.csv")


news_popularity_training_and_wordcount$popularity <- as.factor(news_popularity_training_and_wordcount$popularity)
randomforestwords1 <- ranger(popularity ~ ., write.forest = TRUE,
                             data=news_popularity_training_and_wordcount[,-(1:2)], num.trees=500)
rm(news_popularity_training_and_wordcount)
popularity_prediction <- predict(randomforestwords1, news_popularity_test_and_wordcount)
submit <- data.frame(id = news_popularity_test$id, popularity = as.numeric(popularity_prediction$predictions))
write.csv(submit, file = "new_pop_prediction_randomforestwords1.csv", row.names = FALSE)


#trying how well the additional variables work
news_popularity_training <- read.csv("news_popularity_training.csv")
news_popularity_test<- read.csv("news_popularity_test.csv")
news_popularity_training$popularity <- as.factor(news_popularity_training$popularity)
randomforest1 <- ranger(popularity ~ ., write.forest = TRUE,
                             data=news_popularity_training[1:20000,-(1:2)], num.trees=800)
popularity_prediction <- predict(randomforest1, news_popularity_training[20001:30000,-(1:2)])
table(popularity_prediction$predictions, news_popularity_training$popularity[20001:30000])
sum(popularity_prediction$predictions == news_popularity_training$popularity[20001:30000])/10000

news_popularity_training_wordcluster$popularity <- as.factor(news_popularity_training_wordcluster$popularity)
randomforestwords1 <- ranger(popularity ~ ., write.forest = TRUE,
                        data=news_popularity_training_wordcluster[1:20000,-(1:2)], num.trees=800)
popularity_prediction <- predict(randomforestwords1, news_popularity_training_wordcluster[20001:30000,-(1:2)])
table(popularity_prediction$predictions, news_popularity_training$popularity[20001:30000])
sum(popularity_prediction$predictions == news_popularity_training$popularity[20001:30000])/10000

#true shit
randomforestwordcluster1 <- ranger(popularity ~ ., write.forest = TRUE,
                                   data=news_popularity_training_wordcluster[,-(1:2)], num.trees=1000)

popularity_prediction <- predict(randomforestwordcluster1, news_popularity_test_wordcluster)
submit <- data.frame(id = news_popularity_test_wordcluster$id, popularity = as.numeric(popularity_prediction$predictions))
write.csv(submit, file = "new_pop_prediction_randomforestwordcluster1.csv", row.names = FALSE)

randomforestwordcluster2 <- randomForest(as.factor(popularity) ~ .,
                              data=news_popularity_training_wordcluster[,-(1:3)], ntree=500)

popularity_prediction <- predict(randomforestwordcluster2, news_popularity_test_wordcluster)
submit <- data.frame(id = news_popularity_test_wordcluster$id, popularity = as.numeric(popularity_prediction))
write.csv(submit, file = "new_pop_prediction_randomforestwordcluster2.csv", row.names = FALSE)
