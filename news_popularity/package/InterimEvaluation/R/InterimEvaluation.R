# ----------------------------------------------------------------------
# Random Forest for News Popularity
# ----------------------------------------------------------------------
#' Interim Evaluation Package
#'
#' Implement random forest classifier with 400 trees for Predicting online news popularity Kaggle competition.
#'
#' @param dataTrain Training dataset: a data frame or a matrix where rows are observations and columns are features with the last column being response.
#' @param dataTest Test dataset: a data frame or a matrix with new observations and the same set of features as in the training set apart from the output.
#' @param saveCSV Set to FALSE by default; otherwise saves the predictions as csv file.
#' @return Dataframe called submit containing predictions for the test data.
#' @export
#' @import assertthat
#' @import randomForest

InterimEvaluation <- function(dataTrain, dataTest, saveCSV = FALSE) {

  # test the inputs
  library(assertthat)
  not_empty(dataTrain); not_empty(dataTest);
  assert_that(is.logical(saveCSV))
  assert_that(ncol(dataTrain) - 1 == ncol(dataTest))

  # rearrange the data to fit the function
  library(randomForest)
  dataTrain <- dataTrain[-which(dataTrain$n_non_stop_words>2),]
  y <- dataTrain$popularity
  y <- as.factor(y)
  X <- dataTrain[, -ncol(dataTrain)]
  X$newfeat <- X$timedelta * X$num_hrefs
  X <- dataTrain[, -(1:2)]
  Df <- as.data.frame(cbind(y, X))
  newX <- dataTest
  newX$newfeat <- dataTest$timedelta * dataTest$num_hrefs
  newX <- newX[,-(1:2)]
  newid <- dataTest[, 1]
  # ensure reproducibility with the seed
  set.seed(112)
  randomforest1 <- randomForest(y ~ .,
                                data=Df,
                                ntree=400, mtry = 21)
  popularity_prediction1 <- predict(object = randomforest1, newdata = newX)
  submit <- data.frame(id = newid, popularity = as.numeric(popularity_prediction1))
  # write the csv file if specified
  if (saveCSV) {write.csv(submit, file = "popularity_prediction1.csv", row.names = FALSE)}
  # output the dataframe with predictions
  return(submit = submit)
}
