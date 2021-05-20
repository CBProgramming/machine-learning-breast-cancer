# returns dataframe of training data with reduced number of features
# this should only be passed the training data after being split
# test data should not be given to this function
stepwise_function <- function(training_data, test_data) 
{
  library(olsrr)
  
  #build model of all data
  model <- lm(diagnosis ~ . , data = training_data)
  
  #perform stepwise feature selection
  stepwise <- ols_step_both_aic(model)
  stepwise
  
  #get names of data frame
  ready_data_names <- names(ready_data)
  
  #get names of selected features 
  selected_feature_names <- stepwise[1]
  selected_feature_names <- unlist(selected_feature_names, use.names=FALSE)
  
  #create data frame containing diagnosis
  selected_features_train <- data.frame("Diagnosis" = training_data[31])
  selected_features_test <- data.frame("Diagnosis" = test_data[31])
  #populate dataframe with features selected
  x<-1
  while(x<length(ready_data_names))
  {
    current_feature = ready_data_names[x]
    if(is.element(current_feature,selected_feature_names))
    {
      new_column_train <- training_data[x]
      selected_features_train <- cbind(selected_features_train,Name=c(new_column_train))
      new_column_test <- test_data[x]
      selected_features_test <- cbind(selected_features_test,Name=c(new_column_test))
    }
    x<-x+1
  }
  new_data = list("train" = selected_features_train, "test" = selected_features_test)
  return(new_data)
}