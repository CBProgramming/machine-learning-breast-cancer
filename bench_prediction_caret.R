library(caret)
library(caTools)
require(randomForest)
require(nnet)
require(kernlab)
require(RWeka)
require(MASS)
require(naivebayes)
source("feature_selection/lda_feature_selection_2.R")
source("feature_selection/PCA function script.R")
source("feature_selection/stepwise_2.R")

prepareData <- function() {
  data <- read.csv("wdbc.data", header = F)
  
  features <- c("radius", "texture", "perimeter", "area", "smoothness", "compactness", "concavity", "concave_points", "symmetry", "fractal_dimension")
  names(data) <- c("id", "diagnosis", paste0(features, "_mean"), paste0(features, "_std_err"), paste0(features, "_worst"))
  
  wdbc.data <- data[,c(3:32)]
  row.names(wdbc.data) <- data$id
  ready_data <- cbind(wdbc.data, factor(data$diagnosis))
  colnames(ready_data)[31] <- "diagnosis"
  return(ready_data)
}

apply_norm <- function(df) {
  min_max_norm <- function(data_list) {
    norm <- (data_list - min(data_list)) / (max(data_list) - min(data_list))
    return(norm)
  }
  
  pos <- which(colnames(df) == "diagnosis")
  cols <- df[-pos]
  cols <- as.data.frame(lapply(cols[1:ncol(cols)], min_max_norm))
  
  rownames(cols) <- rownames(df)
  
  cols$diagnosis <- df$diagnosis
  cols
}

translateClass <- function(name_class) {
  class_fun <- NULL
  if (name_class == "KNN") {
    class_fun <- "knn"
  }
  else if (name_class == "Random Forrest") {
    class_fun <- "rf"
  }
  else if (name_class == "ANN") {
    class_fun <- "nnet"
  }
  else if (name_class == "SVM") {
    class_fun <- "svmLinear"
  }
  else if (name_class == "C4.5") {
    class_fun <- "J48"
  }
  else if (name_class == "LDA") {
    class_fun <- "lda"
  }
  else if (name_class == "Naive Bayes") {
    class_fun <- "naive_bayes"
  }
  return(class_fun)
}

trainModel <- function(method_name, training_data, crossfold = FALSE) {
  trCtrl <- NULL
  if (crossfold) {
    trCtrl <- trainControl(method = "repeatedcv")
  }
  else {
    trCtrl <- trainControl(method = "none")
  }
  
  #naive bayes will crash if this is set
  if (!crossfold && method_name == "naive_bayes") {
    model <- train(
      diagnosis ~ .,
      data = training_data,
      method = method_name
    )
  }
  else {
    model <- train(
      diagnosis ~ .,
      data = training_data,
      method = method_name,
      trControl = trCtrl
    )
  }
  return(model)
}

testRun <- function(filter_classification = NULL, filter_feature_selection = NULL, crossfold = FALSE) {
  set.seed(17)
  
  base_data <- prepareData()
  sample <- sample.split(base_data$diagnosis, SplitRatio = .7)
  
  #Generate combinations
  combinations <- expand.grid(c("None", "Min/Max"), c("None", "PCA", "LDA", "Stepwise"), c("KNN", "Random Forrest", "ANN", "SVM", "C4.5", "LDA", "Naive Bayes"))
  
  results <- data.frame(
    x1 = character(),
    x2 = character(),
    x3 = character(),
    x4 = double(),
    x5 = double(),
    x6 = double()
  )
  
  for (i in 1:nrow(combinations)) {
    conf <- combinations[i, ]
    
    norm_fun <- NULL
    if (conf[1] == "Min/Max") {
      norm_fun <- apply_norm
    }
    
    fs_fun <- NULL
    if (conf[2] == "PCA") {
      fs_fun <- pca.function
    }
    else if (conf[2] == "LDA") {
      fs_fun <- lda_feature_selection
    }
    else if (conf[2] == "Stepwise") {
      fs_fun <- stepwise_function
    }
    
    n <- as.character(conf[,1])
    f <- as.character(conf[,2])
    c <- as.character(conf[,3])
    
    class_fun <- translateClass(c)
    
    if (
        (is.null(filter_classification) || c == filter_classification) &&
        (is.null(filter_feature_selection) || f == filter_feature_selection)
      ) {
      #Do training and prediction
      print(paste(paste("Running configuration No.", i, ":", sep = ""), paste(n, f, c, sep = ", ")))

      #Handle normalization
      run_data <- base_data
      if (!is.null(norm_fun)) {
        run_data <- norm_fun(run_data)
      }

      #Handle feature selection
      if (!is.null(fs_fun)) {
        run_data <- fs_fun(run_data)
      }

      training_data <- subset(base_data, sample == TRUE)
      test_data <- subset(base_data, sample == FALSE)

      model <- trainModel(class_fun, training_data = training_data, crossfold = crossfold)
      pred <- predict(model, test_data)
      obs <- test_data$diagnosis
      cm <- confusionMatrix(data = pred, reference = obs)

      new_row <- c(use.names = FALSE, n, f, c, cm$byClass["Sensitivity"], cm$byClass["Specificity"], cm$overall["Accuracy"])
      results <- rbind(results, new_row)
    }
  }
  
  names(results) <- c("Normalization Function", "Feature Selection Algorithm", "Classification Algorithm", "Sensitivity", "Specificity", "Accuracy")
  return(results)
}