svm_function <- function(training_data, test_data) {
  #install.packages("caret")
  #install.packages("e1071")
  library('caret')
  #svm_training_data = selected_features
  training_data$diagnosis=as.factor(training_data$diagnosis)
  test_data$diagnosis=as.factor(test_data$diagnosis)
  #define parameters for training, implementing cross fold validation
  trainControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
  #train  model
  svm <- train(diagnosis ~., data = training_data, method = "svmLinear",trControl=trainControl)
  svm
  # generate prediction
  prediction <- predict(svm, newdata = test_data)
  prediction
  confusionMatrix(table(prediction, test_data$diagnosis))
  grid <- expand.grid(C = c(0, 0.01, 0.05, 0.075, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6,  0.75, 1, 1.25, 1.5, 1.75, 2, 3, 4, 5))
  svm_grid <- train(diagnosis ~., data = training_data, method = "svmLinear",trControl=trctrl,preProcess = c("center", "scale"),tuneGrid = grid,tuneLength= 10)
  svm_grid
  plot(svm_grid)
  df_prediction=data.frame(prediction)
  return df_prediction
}