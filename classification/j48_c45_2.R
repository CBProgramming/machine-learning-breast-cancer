j48_c45_function <- function(training_data, test_data, kfold = FALSE) {
  library(caret)
  require(RWeka)
  
  if (kfold) {
    trCtrl <- trainControl(method = "repeatedcv")
  }
  else {
    trCtrl <- trainControl(method = "none")
  }
  
  #Convert diagnosis to factor to indicate classifiction instead of regression
  j48_training_data = training_data
  j48_training_data$diagnosis=as.factor(j48_training_data$diagnosis)
  
  j48_test_data = test_data
  j48_test_data$diagnosis=as.factor(j48_test_data$diagnosis)
  
  model <- train(diagnosis ~ .,
                 data = j48_training_data,
                 method = "J48",
                 trControl = trCtrl)
  
  result <- data.frame(prediction = predict(model, j48_test_data))
  rownames(result) <- rownames(j48_test_data)
  result
}