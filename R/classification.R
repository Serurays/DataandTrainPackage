#' Simple Classification with Train-Test Split
#'
#' This function fits a classification model using mlr3 with a train-test split.
#'
#' @param df A data frame.
#' @param response The name of the response variable.
#' @param predictors A vector of predictor variable names.
#' @param learner_name The name of the learner to use. Default is "classif.rpart".
#' @param train_ratio The proportion of data to use for training. Default is 0.7.
#' @return A list with the trained model and performance metrics.
#' @export
#'
#' @examples
#' data(iris)
#' simple_classification(iris, 'Species', c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width'))
simple_classification <- function(df, response, predictors, learner_name = "classif.rpart", train_ratio = 0.7) {
  library(mlr3)
  library(mlr3learners)
  library(mlr3measures)

  # Create a classification task
  task <- TaskClassif$new(id = "classification", backend = df, target = response)

  # Split the data into training and test sets
  train_set <- sample(task$nrow, size = train_ratio * task$nrow)
  test_set <- setdiff(seq_len(task$nrow), train_set)

  # Define the learner
  learner <- lrn(learner_name)

  # Train the model
  learner$train(task, row_ids = train_set)
  model <- learner$model

  # Predict on the training set
  train_prediction <- learner$predict(task, row_ids = train_set)
  train_performance <- train_prediction$score(msr("classif.acc"))

  # Predict on the test set
  test_prediction <- learner$predict(task, row_ids = test_set)
  test_performance <- test_prediction$score(msr("classif.acc"))

  return(list(model = model, train_performance = train_performance, test_performance = test_performance))
}
