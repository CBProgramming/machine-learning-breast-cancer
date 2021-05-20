library(MASS)

data <- read.csv("wdbc.data", header = F)

#Column names from wdbc.names
features <- c("radius", "texture", "perimeter", "area", "smoothness", "compactness", "concavity", "concave_points", "symmetry", "fractal_dimension")
names(data) <- c("id", "diagnosis", paste0(features, "_mean"), paste0(features, "_std_err"), paste0(features, "_worst"))

#Separate ID from variables, convert diagnosis to a numeric. (M=1, B=0)
wdbc.data <- data[,c(3:32)]
row.names(wdbc.data) <- data$id
ready_data <- cbind(wdbc.data, as.numeric(factor(data$diagnosis))-1)
colnames(ready_data)[31] <- "diagnosis"

#Small sample set
sample_size <- floor(0.75 * nrow(ready_data))
training_rows <- sample(nrow(ready_data), size = sample_size)
sample_train_df <- ready_data[training_rows, ]
sample_test_df <- ready_data[-training_rows, ]

#LDA

#Build formula of all 30 vars
form_all <- paste(names(sample_train_df)[31], "~", paste(names(sample_train_df)[-31], collapse = " + "))

all_lda <- lda(as.formula(paste(form_all)), data = sample_train_df)
#Try prediction on different data
all_lda.predict <- predict(all_lda, newdata = sample_test_df)

#Get 2dp percent chances
res <- as.data.frame(round(all_lda.predict$posterior * 100, 2))
names(res) <- c("Benign Chance (%)", "Malignant Chance(%)")