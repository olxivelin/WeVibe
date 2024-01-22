# Load necessary libraries
library(data.table)  # for data handling
library(caret)       # for data partitioning and model evaluation
library(party)       # for Ctree
library(RWeka)       # for J48 Tree
library(class)       # for k-NN
library(ggplot2)     # for plotting

# Load and prepare the data
data <- fread("C:yourDirectory")
numeric_vars <- c("age", "balance", "day", "duration", "campaign", "pdays", "previous")
data_numeric <- data[, c(numeric_vars, "y"), with = FALSE]
data_numeric[, (numeric_vars) := lapply(.SD, scale), .SDcols = numeric_vars]
data_numeric$y <- factor(data_numeric$y, levels = c("no", "yes"))

# Split the data into training and testing sets
set.seed(123) # for reproducibility
trainIndex <- createDataPartition(data_numeric$y, p = .8, list = FALSE, times = 1)
trainData <- data_numeric[trainIndex, ]
testData <- data_numeric[-trainIndex, ]

# Prepare features and labels for training
train_features <- trainData[, -ncol(trainData), with = FALSE]
train_labels <- trainData$y
test_features <- testData[, -ncol(testData), with = FALSE]
test_labels <- testData$y

# Train the models
ctreeModel <- ctree(y ~ ., data = trainData)
j48Model <- J48(y ~ ., data = trainData)
linearModel <- glm(y ~ ., data = trainData, family = "binomial")
knnModel <- knn(train = train_features, test = test_features, cl = train_labels, k = 5)

# Generating and processing predictions for each model
# Ctree
pred_ctree <- predict(ctreeModel, newdata = testData, type = "response")
pred_ctree <- factor(pred_ctree, levels = levels(test_labels))

# J48
pred_j48 <- predict(j48Model, newdata = testData, type = "class")
pred_j48 <- factor(pred_j48, levels = levels(test_labels))

# Linear
pred_linear_raw <- predict(linearModel, newdata = testData, type = "response")
pred_linear <- ifelse(pred_linear_raw > 0.5, "yes", "no")
pred_linear <- factor(pred_linear, levels = levels(test_labels))

# k-NN
pred_knn <- knn(train = train_features, test = test_features, cl = train_labels, k = 5)
pred_knn <- factor(pred_knn, levels = levels(test_labels))

# Model Evaluation
results <- data.frame(Model = character(), Accuracy = numeric(), ErrorRate = numeric(), Precision = numeric(), Recall = numeric(), F1Score = numeric(), stringsAsFactors = FALSE)
predictions <- list(ctree = pred_ctree, j48 = pred_j48, linear = pred_linear, knn = pred_knn)

for(model_name in names(predictions)) {
  pred <- predictions[[model_name]]
  
  # Compute the confusion matrix
  confusionMatrix <- confusionMatrix(pred, test_labels)
  
  accuracy <- confusionMatrix$overall['Accuracy']
  errorRate <- 1 - accuracy
  precision <- confusionMatrix$byClass['Precision']
  recall <- confusionMatrix$byClass['Recall']
  f1Score <- 2 * (precision * recall) / (precision + recall)
  
  results <- rbind(results, data.frame(Model = model_name, Accuracy = accuracy, ErrorRate = errorRate, Precision = precision, Recall = recall, F1Score = f1Score))
}


# Calculate range for Error Rate graph
min_error_rate <- min(results$ErrorRate)
max_error_rate <- max(results$ErrorRate)

# Find minimum values for Precision, Recall, and F1
min_precision <- min(results$Precision)
min_recall <- min(results$Recall)
min_f1 <- min(results$F1Score)

#Add a padding value to better visualize graphs
padding <- 0.007

# Find the minimum accuracy value from the results to set as the lower limit
min_accuracy <- min(results$Accuracy)

# Visualizations with ggplot2 with dynamically set zoomed y-axis using coord_cartesian for all classifiers
ggplot(results, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Classifier Comparison Based on Accuracy", x = "Classifier", y = "Accuracy") +
  coord_cartesian(ylim = c(min_accuracy - 0.001, 0.9))

# Error Rate Graph
ggplot(results, aes(x = Model, y = ErrorRate, fill = Model)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Classifier Comparison Based on Error Rate", x = "Classifier", y = "Error Rate") +
  coord_cartesian(ylim = c(min_error_rate - 0.03, max_error_rate + 0.03))

# Precision Graph
ggplot(results, aes(x = Model, y = Precision, fill = Model)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Classifier Comparison Based on Precision", x = "Classifier", y = "Precision") +
  coord_cartesian(ylim = c(min_precision - padding, 1))

# Recall Graph
ggplot(results, aes(x = Model, y = Recall, fill = Model)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Classifier Comparison Based on Recall", x = "Classifier", y = "Recall") +
  coord_cartesian(ylim = c(min_recall - padding, 1))

# F1-Score Graph
ggplot(results, aes(x = Model, y = F1Score, fill = Model)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Classifier Comparison Based on F1-Score", x = "Classifier", y = "F1-Score") +
  coord_cartesian(ylim = c(min_f1 - padding, 1))
