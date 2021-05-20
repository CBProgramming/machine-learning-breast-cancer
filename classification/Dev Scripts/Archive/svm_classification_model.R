#install.packages("caret")
#install.packages("e1071")
library('caret')
#convert diagnosis to a factor to allow for random forest classification
svm_training_data = selected_features
svm_training_data$diagnosis=as.factor(svm_training_data$diagnosis)
svm_test_data = test_data
svm_test_data$diagnosis=as.factor(svm_test_data$diagnosis)
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
svm_Linear <- train(diagnosis ~., data = svm_training_data, method = "svmLinear",trControl=trctrl,preProcess = c("center", "scale"),tuneLength = 10)
svm_Linear
test_pred <- predict(svm_Linear, newdata = svm_test_data)
test_pred
confusionMatrix(table(test_pred, svm_test_data$diagnosis))
grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
svm_Linear_Grid <- train(diagnosis ~., data = svm_training_data, method = "svmLinear",trControl=trctrl,preProcess = c("center", "scale"),tuneGrid = grid,tuneLength= 10)
svm_Linear_Grid
plot(svm_Linear_Grid)
