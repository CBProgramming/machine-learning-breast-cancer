library(olsrr)
# returns dataframe of training data with reduced number of features
# this should only be passed the training data after being split
stepwise_function <- function(training_data) 
{
  copy <- training_data
  copy$diagnosis <- as.numeric(training_data$diagnosis) - 1
  
  #Hacky solution to ols_step_both_aic erroring with 'training_data' not found
  #but cannot currently find a better way
  stepwise_training_data_global <<- copy
  
  #build model of all data
  model <- lm(diagnosis ~ ., data = stepwise_training_data_global)
  
  #perform stepwise feature selection
  stepwise <- ols_step_both_aic(model)
  stepwise
  
  #get names of data frame
  ready_data_names <- names(training_data)
  
  #get names of selected features 
  selected_feature_names <- stepwise[1]
  selected_feature_names <- unlist(selected_feature_names, use.names=FALSE)
  
  #create data frame containing diagnosis
  selected_features_train <- data.frame("diagnosis" = training_data$diagnosis)
  #populate dataframe with features selected
  x<-1
  while(x<length(ready_data_names))
  {
    current_feature = ready_data_names[x]
    if(is.element(current_feature,selected_feature_names))
    {
      new_column_train <- training_data[x]
      selected_features_train <- cbind(selected_features_train,Name=c(new_column_train))
    }
    x<-x+1
  }
  
  row.names(selected_features_train) <- row.names(training_data)
  return(selected_features_train)
}