data <- read.csv("wdbc.data", header = F)

#Column names from wdbc.names
features <- c("radius", "texture", "perimeter", "area", "smoothness", "compactness", "concavity", "concave_points", "symmetry", "fractal_dimension")
names(data) <- c("id", "diagnosis", paste0(features, "_mean"), paste0(features, "_std_err"), paste0(features, "_worst"))

#Separate ID from variables, convert diagnosis to a numeric. (M=1, B=0)
wdbc.data <- data[,c(3:32)]
row.names(wdbc.data) <- data$id
ready_data <- cbind(wdbc.data, as.numeric(factor(data$diagnosis))-1)
colnames(ready_data)[31] <- "diagnosis"

robust_scalar<- function(x){(x- median(x)) /(quantile(x,probs = .75)-quantile(x,probs = .25))}

robust_scalar_data <- as.data.frame(lapply(ready_data, robust_scalar))