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
  y <- dataTrain[, ncol(dataTrain)]
  y <- as.factor(y)
  X <- dataTrain[, -c(1, 2, 3, ncol(dataTrain))]
  Df <- as.data.frame(cbind(y, X))
  newX <- dataTest[, -c(1, 2, 3)]
  newid <- dataTest[, 1]
  # ensure reproducibility with the seed
  set.seed(0)
  randomforest_0rF400 <- randomForest(y ~ ., data = Df, ntree = 400)
  popularity_prediction_0rF400 <- predict(object = randomforest_0rF400, newdata = newX)
  submit <- data.frame(id = newid, popularity = as.numeric(popularity_prediction_0rF400))
  # write the csv file if specified
  if (saveCSV) {write.csv(submit, file = "popularity_prediction_0rF400.csv", row.names = FALSE)}
  # output the dataframe with predictions
  return(submit = submit)
}
