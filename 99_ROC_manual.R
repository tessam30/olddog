#ROC

data <- readr::read_csv("../../../Downloads/dummy_data_roc.csv")
# Load the dummy data


# Extract actual and predicted values
actual <- data$actual
predicted <- data$predicted

# Sort data by predicted probabilities
data <- data[order(data$predicted, decreasing = TRUE),]

# Initialize variables
TPR <- FPR <- numeric(length(unique(data$predicted)))
prev_prob <- -Inf
i <- 1

# Calculate TPR and FPR for each threshold
for(prob in unique(data$predicted)) {
  thresholded_predictions <- ifelse(data$predicted >= prob, 1, 0)
  TP <- sum(thresholded_predictions == 1 & data$actual == 1)
  FP <- sum(thresholded_predictions == 1 & data$actual == 0)
  TN <- sum(thresholded_predictions == 0 & data$actual == 0)
  FN <- sum(thresholded_predictions == 0 & data$actual == 1)
  
  TPR[i] <- TP / (TP + FN)
  FPR[i] <- FP / (FP + TN)
  
  i <- i + 1
}

# Plot ROC Curve
plot(FPR, TPR, type = "l", col = "blue", xlab = "False Positive Rate", ylab = "True Positive Rate")
abline(a = 0, b = 1, col = "red", lty = 2) # Diagonal line for reference

# Calculate AUC (Approximate)
auc <- sum(diff(FPR) * (head(TPR, -1) + tail(TPR, -1)) / 2)
print(paste("AUC:", auc))

  