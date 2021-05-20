#install.packages("neuralnet")
library(neuralnet)
#setup training data for ANN
#training_data = selected_features (seem to get better results without feature selection)
training_data$diagnosis=as.factor(training_data$diagnosis)
factors1 <- sapply(training_data,is.factor)
M1<-sapply(training_data[,factors1],unclass)
ann_training_data<-cbind(training_data[,!factors1],M1)
ann_training_data = na.omit(ann_training_data)
#setup test data for ANN
test_data$diagnosis=as.factor(test_data$diagnosis)
factors2 <- sapply(test_data,is.factor)
M2<-sapply(test_data[,factors2],unclass)
ann_test_data<-cbind(test_data[,!factors2],M2)
ann_test_data = na.omit(ann_test_data)
#set seed
set.seed(7)
#fit neural network
NN = neuralnet(M1 ~ .,ann_training_data, hidden =10, linear.output = T)
plot(NN)
#predict
predict_testNN = compute(NN, ann_test_data [-31])
#generate results
results <- data.frame(actual = ann_test_data$M, prediction = predict_testNN$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
