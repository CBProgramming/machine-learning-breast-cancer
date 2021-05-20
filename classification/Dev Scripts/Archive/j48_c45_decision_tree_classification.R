#install.packages("RWeka")
#install.packages("caTools")
library(RWeka)
library(caTools)
#convert diagnosis to a factor to allow for J48 classification
j48_training_data = selected_features
j48_training_data$diagnosis=as.factor(j48_training_data$diagnosis)
j48_test_data = test_data
j48_test_data$diagnosis=as.factor(j48_test_data$diagnosis)
#train model on training data
j48 = J48(formula = diagnosis ~ ., data = j48_training_data)
j48
#plot the importance assigned to each feature
varImpPlot(j48)
#predict diagnosis on test data
prediction = predict(j48, newdata = j48_test_data [-31])
#generate confusion matrix to observe accuracy

confusion_matrix = table(j48_test_data[,31], prediction)
confusion_matrix