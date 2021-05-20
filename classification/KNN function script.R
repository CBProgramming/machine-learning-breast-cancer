knn.function <- function(train.data, test.data) {
  library(class)

  train.data_diagnosis <- train.data["diagnosis"]
  test.data_diagnosis <- test.data["diagnosis"]
  
  train.data$diagnosis <- NULL
  test.data$diagnosis <- NULL
  
  i=1
  k.optm=1
  for(i in 1:100){ knn.mod <- knn(train=train.data, test=test.data, cl=train.data_diagnosis$diagnosis, k=i)
  k.optm[i] <- 100 * sum(test.data_diagnosis$diagnosis == knn.mod)/NROW(test.data_diagnosis)
  k=i}
  
  results <- knn(train=train.data, test=test.data, cl=train.data_diagnosis$diagnosis, k=which.max(k.optm))
  
  #Format results for eval function
  results <- data.frame(results)
  names(results) <- c("prediction")
  row.names(results) <- row.names(test.data)
  
  return(results)
}