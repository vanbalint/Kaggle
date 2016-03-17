train <- read.csv("news_popularity_training.csv")
train$popularity <- as.factor(train$popularity)
test <- read.csv("news_popularity_test.csv")

set.seed(0)
tr.idx <- sample(nrow(train), 2/3*nrow(train))
tr.train <- train[tr.idx, ]
te.train <- train[-tr.idx, ]

set.seed(0)
t.rF <- randomForest(popularity ~ ., data = tr.train[, -c(1,2,3)],
                     ntree = 1000, xtest = te.train[, -c(1,2,3,62)], ytest = te.train[,62])

dev.off()
plot(t.rF$test[[2]][,1], type = "l", ylim = c(min(t.rF$test[[2]]),1), ylab = "Errors", xlab = "Number of trees")
points(t.rF$test[[2]][,2], type = "l", ylim = c(min(t.rF$test[[2]]),1), col = 2)
points(t.rF$test[[2]][,3], type = "l", ylim = c(min(t.rF$test[[2]]),1), col = 3)
points(t.rF$test[[2]][,4], type = "l", ylim = c(min(t.rF$test[[2]]),1), col = 4)
points(t.rF$test[[2]][,5], type = "l", ylim = c(min(t.rF$test[[2]]),1), col = 5)
points(t.rF$test[[2]][,6], type = "l", ylim = c(min(t.rF$test[[2]]),1), col = 6)
legend(350,0.87,legend = c("oob","c1","c2","c3","c4","c5"), col = c(1,2,3,4,5,6),
       lty = 1, cex = 0.75, ncol = 3, bty = "n")

plot(t.rF$test[[2]][,1], type = "l", ylab = "Out-of-bag error", xlab = "Number of trees")
