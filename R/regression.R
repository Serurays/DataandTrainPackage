#' Simple Linear Regression with Train-Test Split
#'
#' This function fits a simple linear regression model using mlr3 with a train-test split.
#'
#' @param df A data frame.
#' @param response The name of the response variable.
#' @param predictor The name of the predictor variable.
#' @param train_ratio The proportion of data to use for training. Default is 0.7.
#' @return A list with the model, training performance, and test performance.
#' @export
#'
#' @examples
#' data(mtcars)
#' simple_lm(mtcars, 'mpg', 'hp')
simple_lm <- function(df, response, predictor, train_ratio = 0.7) {
  library(mlr3)
  library(mlr3learners)
  library(mlr3measures)

  # Create a regression task
  task <- TaskRegr$new(id = "regression", backend = df, target = response)

  # Split the data into training and test sets
  train_set <- sample(task$nrow, size = train_ratio * task$nrow)
  test_set <- setdiff(seq_len(task$nrow), train_set)

  # Define the learner
  learner <- lrn("regr.lm")

  # Train the model
  learner$train(task, row_ids = train_set)
  model <- learner$model

  # Predict on the training set
  train_prediction <- learner$predict(task, row_ids = train_set)
  train_performance <- train_prediction$score(msr("regr.mse"))

  # Predict on the test set
  test_prediction <- learner$predict(task, row_ids = test_set)
  test_performance <- test_prediction$score(msr("regr.mse"))

  return(list(model = model, train_performance = train_performance, test_performance = test_performance))
}
