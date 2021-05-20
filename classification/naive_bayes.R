naive_bayes_predict <- function(training, testing) {
  library(caret)
  library(naivebayes)
  
  training$diagnosis <- factor(training$diagnosis)
  testing$diagnosis <- factor(testing$diagnosis)
  
  nvb <- train(diagnosis ~ ., data = training, method = "naive_bayes")
  results <- predict(nvb, newdata = testing)
  
  #Format results for eval function
  results <- data.frame(results)
  names(results) <- c("prediction")
  row.names(results) <- row.names(testing)
  results
}