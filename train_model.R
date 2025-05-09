# Load required packages
library(glmnet)
library(readr)
library(pROC)
library(caret)
# Load the processed/cleaned dataset
df <- read_csv("processed_add.csv")
# Rename the label column
colnames(df)[ncol(df)] <- "label"
# Convert label to factor (already 0/1)
df$label <- as.factor(df$label)
df[ ,-ncol(df)] <-lapply(df[ ,-ncol(df)], function(x) as.numeric(as.character(x)))
df <-na.omit(df)
# Prepare features and labels
X <-as.matrix(df[ ,-ncol(df)])
y <-df$label # Already a factor
# Split into train/test sets
set.seed(42)
train_index <-createDataPartition(y, p = 0.8, list = FALSE)
X_train <-X[train_index, ]
X_test <-X[-train_index, ]
y_train <-y[train_index]
y_test <-y[-train_index]
# Train Lasso Logistic Regression with cross-validation
cvfit <-cv.glmnet(X_train, y_train, family = "binomial", alpha = 1, type.measure = "class")
# Plot CV result
plot(cvfit)
# Best lambda
best_lambda <-cvfit$lambda.min
cat("Best lambda:", best_lambda, "\n")
# Predict on test set
pred <-predict(cvfit, newx = X_test, s = best_lambda, type = "class")
# Confusion matrix and accuracy
conf_matrix <-confusionMatrix(as.factor(pred), y_test)
print(conf_matrix)

# === ROC Curve and AUC ===
# Predict probabilities instead of class labels
prob <- predict(cvfit, newx = X_test, s = best_lambda, type = "response")

# Compute ROC
roc_obj <- roc(as.numeric(as.character(y_test)), as.numeric(prob))

# Plot ROC
plot(roc_obj, col = "#1c61b6", lwd = 2, main = "ROC Curve")

# Print AUC
auc_value <- auc(roc_obj)
cat("AUC:", auc_value, "\n")
# Get coefficients (including intercept) at best lambda
coef_values <- coef(cvfit, s = "lambda.min")

# Extract intercept (bias term)
bias <- coef_values[1]
cat("Bias (Intercept):", bias, "\n")

# Extract feature coefficients (excluding intercept)
coefficients <- coef_values[-1]
cat("\nCoefficients:\n")
print(coefficients)

# Optional: Create a data frame for better visualization
coef_df <- data.frame(
  Feature = rownames(coef_values)[-1],  # Remove intercept from features
  Coefficient = as.numeric(coefficients)
)

# Remove zero coefficients (Lasso's feature selection)
coef_df <- coef_df[coef_df$Coefficient != 0, ]
print(coef_df)