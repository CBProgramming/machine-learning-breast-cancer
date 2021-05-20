pca.function <- function(dataset) {
  diagnosis <- dataset$diagnosis
  
  #Remove factor as prcomp requires numerics
  dataset <- dataset[,1:(ncol(dataset) - 1)]
  
  pca <- prcomp(t(dataset), scale=TRUE)
  pca.var <- pca$sdev^2
  pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
  
  p=1/NCOL(dataset)*100
  
  i=0
  while(pca.var.per[i+1] > p) {
    i <- i+1
  }
  
  ret <- data.frame(pca$rotation[,1:i])
  ret <- cbind(ret, diagnosis)
  return(ret)
}