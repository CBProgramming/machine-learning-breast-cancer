pca.function <- function(dataset) {
  pca <- prcomp(t(dataset), scale=TRUE)
  pca.var <- pca$sdev^2
  pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
  
  i=0
  p=0
  while(p < 95) {
    i <- i+1
    p <- p + pca.var.per[i]
  }
  
  return(pca$rotation[,1:i])
}