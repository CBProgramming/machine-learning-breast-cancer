# give this function the entire dataset prior to splitting into test and train
lda_feature_selection <- function(data) {
  library(MASS)
  options(max.print=1000)
  diagnosis = data[31]
  lda = lda(formula = diagnosis ~ ., data = data)
  #generate reduced dimensions
  reduced = as.data.frame(predict(lda, data))
  #remove posterior columns as not required, LD1 retained
  reduced = reduced[c(4,1)]
  #rername class back to diagnosis
  names(reduced)[names(reduced) == "class"] <- "diagnosis"
  reduced[2] = diagnosis[1]
  #(reduced)
  return(reduced)
}
