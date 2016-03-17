train <- read.csv("news_popularity_training.csv")
train$popularity <- as.factor(train$popularity)
test <- read.csv("news_popularity_test.csv")


library(class)

all.knns.train <- list()
for (i in 1:15) {
  all.knns.train[[i]] <- knn.cv(train[,-c(1,2,3,62)], cl = train[,62], k = i)
}
knns.train <- data.frame(cbind(knn1 = all.knns.train[[1]], knn2 = all.knns.train[[2]],
                               knn3 = all.knns.train[[3]], knn4 = all.knns.train[[4]],
                               knn5 = all.knns.train[[5]], knn6 = all.knns.train[[6]],
                               knn7 = all.knns.train[[7]], knn8 = all.knns.train[[8]],
                               knn9 = all.knns.train[[9]], knn10 = all.knns.train[[10]],
                               knn11 = all.knns.train[[11]], knn12 = all.knns.train[[12]],
                               knn13 = all.knns.train[[13]], knn14 = all.knns.train[[14]],
                               knn15 = all.knns.train[[15]]))
write.csv(knns.train, "knns15train.csv", row.names = FALSE)

all.knns.test <- list()
for (i in 1:15) {
  all.knns.test[[i]] <- knn(train = train[,-c(1,2,3,62)], test = test[,-c(1,2,3)], cl = train[,62], k = i)
}

knns.test <- data.frame(cbind(knn1 = all.knns.test[[1]], knn2 = all.knns.test[[2]],
                              knn3 = all.knns.test[[3]], knn4 = all.knns.test[[4]],
                              knn5 = all.knns.test[[5]], knn6 = all.knns.test[[6]],
                              knn7 = all.knns.test[[7]], knn8 = all.knns.test[[8]],
                              knn9 = all.knns.test[[9]], knn10 = all.knns.test[[10]],
                              knn11 = all.knns.test[[11]], knn12 = all.knns.test[[12]],
                              knn13 = all.knns.test[[13]], knn14 = all.knns.test[[14]],
                              knn15 = all.knns.test[[15]]))
write.csv(knns.test, "knns15test.csv", row.names = FALSE)