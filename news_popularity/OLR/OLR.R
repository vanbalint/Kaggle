train <- read.csv("news_popularity_training.csv")
train$popularity <- as.factor(train$popularity)
test <- read.csv("news_popularity_test.csv")


library(MASS)

OLR <- polr(popularity ~ ., data = train[, c(29,43,20,40,18,38,45,16,19,28,62)])
fitOLR.train <- data.frame(OLR$fitted.values)
colnames(fitOLR.train) <- c("c1prob","c2prob","c3prob","c4prob","c5prob")
write.csv(fitOLR.train, "fitOLRtrain.csv", row.names = FALSE)

fitOLR.test <- data.frame(predict(OLR, test[, c(29,43,20,40,18,38,45,16,19,28)], type = "p"))
colnames(fitOLR.test) <- c("c1prob","c2prob","c3prob","c4prob","c5prob")
write.csv(fitOLR.test, "fitOLRtest.csv", row.names = FALSE)