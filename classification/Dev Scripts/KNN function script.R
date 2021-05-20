knn.function <- function(train.data, test.data) {

  train.data_diagnosis <- tail(train.data)
  test.data_diagnosis <- tail(test.data)
  
  tail(train.data) <- null
  tail(test.data) <- null
  
  i=1
  k.optm=1
  for(i in 1:100){ knn.mod <- knn(train=train.data, test=test.data, cl=train.data_diagnosis, k=i)
  k.optm[i] <- 100 * sum(test.data_labels == knn.mod)/NROW(test.data_diagnosis)
  k=i}
  
  return(knn(train=train.data, test=test.data, cl=train.data_diagnosis, k=which.max(k.optm)))
}