random_forest_function <- function(training_data, test_data) 
{
  #install.packages("randomForest")
  #install.packages("caTools")
  library(randomForest)
  require(caTools)
  #convert diagnosis to a factor to allow for random forest classification
  training_data$diagnosis=as.factor(training_data$diagnosis)
  test_data$diagnosis=as.factor(test_data$diagnosis)
  #train model on training data, can try different number of trees for different results
  #can add mtry=number as arg to change number of random features used in each tree (default is sqrt(num_features))
  rforest = randomForest(formula = diagnosis ~ ., data = training_data, ntree = 100, importance = TRUE)
  rforest
  #plot the importance assigned to each feature
  varImpPlot(rforest)
  #predict diagnosis on test data
  prediction = predict(rforest, newdata = test_data [-31])
  #generate confusion matrix to observe accuracy
  confusion_matrix = table(test_data[,31], prediction)
  confusion_matrix
  results <- data.frame(prediction)
  return(results)
}