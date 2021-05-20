library(MASS)
source("prepare_data.R")
lda = lda(formula = diagnosis ~ ., data = training_data)
training_data = as.data.frame(predict(lda, training_data))
training_data = training_data[c(4,1)]
test_data = as.data.frame(predict(lda, test_data))
test_data = test_data[c(4,1)]

names(training_data)[names(training_data) == "class"] <- "diagnosis"
names(test_data)[names(test_data) == "class"] <- "diagnosis"