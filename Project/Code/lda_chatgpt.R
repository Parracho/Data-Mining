# Install and load necessary packages
install.packages("MASS")
install.packages("caret")
library(MASS)
library(caret)
setwd("C:/Users/bfpar/Documents/GitHub/Data-Mining/Project/Code/")      # Change working directory
library(foreign)
library(MASS)
higgs <- read.arff("dataset.arff")
data_frame <- data.frame(higgs)
# sample 10000 rows from the data set
sample_indices <- sample(1:nrow(data_frame), 1000, replace = FALSE)
data <- data_frame[sample_indices, ]
# Load your Higgs dataset
# Assuming your data frame is named 'data' with 28 variables and a 'class' column

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(data$class, p = 0.8, list = FALSE)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

# Standardize the features
train_data_scaled <- scale(train_data[, -which(names(train_data) == "class")])
test_data_scaled <- scale(test_data[, -which(names(test_data) == "class")])

# Perform Linear Discriminant Analysis
lda_model <- lda(class ~ ., data = train_data)
train_lda <- predict(lda_model, train_data_scaled)
test_lda <- predict(lda_model, test_data_scaled)

# Train a classifier on the transformed data (e.g., logistic regression)
logreg_model <- glm(class ~ LD1, data = data.frame(class = train_data$class, LD1 = train_lda$x[, 1]), family = "binomial")

# Make predictions on the test set
test_predictions <- predict(logreg_model, newdata = data.frame(LD1 = test_lda$x[, 1]), type = "response")

# Convert probabilities to class labels
test_pred_labels <- ifelse(test_predictions > 0.5, 1, 0)

# Evaluate the model
conf_matrix <- confusionMatrix(data = factor(test_pred_labels), reference = factor(test_data$class))
accuracy <- conf_matrix$overall["Accuracy"]

# Print results
print(paste("Accuracy:", accuracy))
print(conf_matrix)
