library(olsrr)

#build model of all data
model <- lm(diagnosis ~ . , data = training_data)

#perform stepwise feature selection
#stepwise <- ols_step_both_p(model, pent = .05, prem = .05)
stepwise <- ols_step_both_aic(model)
stepwise

#get names of data frame
ready_data_names <- names(ready_data)

#get names of selected features 
selected_feature_names <- stepwise[1]
selected_feature_names <- unlist(selected_feature_names, use.names=FALSE)

#create data frame containing diagnosis
selected_features <- data.frame("Diagnosis" = training_data[31])

x<-1
while(x<length(ready_data_names))
{
  current_feature = ready_data_names[x]
  if(is.element(current_feature,selected_feature_names))
  {
    new_column <- training_data[x]
    selected_features <- cbind(selected_features,Name=c(new_column))
  }
  x<-x+1
}