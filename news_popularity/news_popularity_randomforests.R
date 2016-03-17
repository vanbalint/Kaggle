# install.packages('randomForest')
library(randomForest)

news_popularity_training <- read.csv("news_popularity_training.csv")
news_popularity_test<- read.csv("news_popularity_test.csv")

news_popularity_training <- news_popularity_training[-which(news_popularity_training$n_non_stop_words>2),]
news_popularity_training$popularity <- as.factor(news_popularity_training$popularity)
news_popularity_training$newfeat <- news_popularity_training$timedelta * news_popularity_training$num_hrefs
news_popularity_test$newfeat <- news_popularity_test$timedelta * news_popularity_test$num_hrefs

set.seed(112)
randomforest1 <- randomForest(popularity ~ .,
                              data=news_popularity_training[,-(1:2)],
                              ntree=400, mtry = 21)

popularity_prediction <- predict(randomforest1, news_popularity_test)
submit <- data.frame(id = news_popularity_test$id, popularity = as.numeric(popularity_prediction))
write.csv(submit, file = "new_pop_prediction_randomforest1.csv", row.names = FALSE)

# popularity_prediction <- predict(randomforest1, news_popularity_training[20001:30000,-(1:2)])
# 
# table(popularity_prediction, news_popularity_training$popularity[20001:30000])
# sum(popularity_prediction == news_popularity_training$popularity[20001:30000])/10000

# set.seed(0)
# tr.idx <- sample(nrow(news_popularity_training), 2/3*nrow(news_popularity_training))
# tr.train <- news_popularity_training[tr.idx, ]
# te.train <- news_popularity_training[-tr.idx, ]
# 
# set.seed(0)
# t.rF <- randomForest(popularity ~ ., data = tr.train[, -c(1,2,3)], ntree = 1000, xtest = te.train[, -c(1,2,3,62)], ytest = te.train[,62])
# plot(t.rF$test[[2]][,1], type = "l")

# news_popularity_training$newfeat2 <- news_popularity_training$timedelta * news_popularity_training$LDA_02
# news_popularity_training$newfeat3 <- news_popularity_training$timedelta * news_popularity_training$LDA_03
# news_popularity_training$newfeat4 <- news_popularity_training$kw_max_avg * news_popularity_training$LDA_03
# news_popularity_training$newfeat5 <- news_popularity_training$kw_max_avg * news_popularity_training$kw_avg_avg
# news_popularity_training$newfeat6 <- news_popularity_training$kw_avg_avg * news_popularity_training$kw_avg_max
# news_popularity_training$newfeat7 <- news_popularity_training$LDA_03 * news_popularity_training$LDA_04
# news_popularity_training$newfeat8 <- news_popularity_training$kw_avg_avg * news_popularity_training$max_positive_polarity
# news_popularity_training$newfeat9 <- news_popularity_training$kw_max_avg * news_popularity_training$kw_min_avg
# news_popularity_training$newfeat10 <- news_popularity_training$kw_max_avg * news_popularity_training$num_hrefs
# news_popularity_training$newfeat11 <- news_popularity_training$kw_max_avg * news_popularity_training$num_imgs

#testing alternative models

# set.seed(453)
# tr.idx <- sample(nrow(news_popularity_training), 2/3*nrow(news_popularity_training))
# tr.train <- news_popularity_training[tr.idx, ]
# te.train <- news_popularity_training[-tr.idx, ]
# 
# randomforest1 <- randomForest(popularity ~ .,
#                               data=tr.train[,-c(1:2, 64:73)],
#                               ntree=400, mtry = 21)
# pred <- predict(randomforest1, te.train)
# cat("\n small model:")
# sum(sum(pred==te.train$popularity)/nrow(te.train))
# 
# randomforest1 <- randomForest(popularity ~ .,
#                               data=tr.train[,-(1:2)],
#                               ntree=400, mtry = 21)
# pred <- predict(randomforest1, te.train)
# cat("\n big model:")
# sum(sum(pred==te.train$popularity)/nrow(te.train))