lda_predict_function <- function(training, testing) {
  library(MASS)
  
  #Build formula of all 30 vars
  col_names <- names(training)
  col_names <- col_names[-match(c("diagnosis"), col_names)]
  
  form_all <- paste(c("diagnosis"), "~", paste(col_names, collapse = " + "))
  
  all_lda <- lda(as.formula(paste(form_all)), data = training)
  all_lda.predict <- predict(all_lda, newdata = testing)
  
  results <- data.frame(all_lda.predict$class)
  names(results) <- c("prediction")
  row.names(results) <- row.names(testing)
  
  results
}