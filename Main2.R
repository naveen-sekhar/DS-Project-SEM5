# ---
# Project: ML-Driven Intrusion Detection for Industrial IoT Environments
# Language: R
# Model: Random Forest
# ---

# --- 1. Install and Load Required Packages ---
# You may need to run these install commands once in your R console
# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("caret")
# install.packages("randomForest")

library(tidyverse)    # For data manipulation (dplyr, readr)
library(ggplot2)      # For plotting
library(caret)        # For data splitting and model evaluation
library(randomForest) # For the Random Forest classification model

# --- 2. Load the Data ---
# Set the file path to your dataset
file_path <- "iiot_intrusion_dataset_enriched.csv"
iiot_data <- readr::read_csv(file_path)

print("Data loaded successfully.")
print("--- Initial Data Structure ---")
str(iiot_data)


# --- 3. Preprocessing and Feature Engineering ---

# Select relevant columns. We'll drop 'timestamp' (for this model) and
# 'attack_type' because 'label' is our binary (0/1) target.
data_clean <- iiot_data %>%
  select(-timestamp, -attack_type)

# Convert categorical features to factors (R's way of handling categories)
data_clean <- data_clean %>%
  mutate(
    device_id = as.factor(device_id),
    device_type = as.factor(device_type),
    protocol = as.factor(protocol)
  )

# Convert the numeric 'label' (0 or 1) into a descriptive factor.
# This is CRITICAL for R's classification models.
data_clean <- data_clean %>%
  mutate(
    label = as.factor(ifelse(label == 0, "Normal", "Attack"))
  )

# Check for and handle any missing values (NA)
# We'll use a simple strategy: remove any rows with missing data.
rows_before <- nrow(data_clean)
data_clean <- na.omit(data_clean)
rows_after <- nrow(data_clean)
print(paste("Removed", rows_before - rows_after, "rows with missing values."))

print("--- Cleaned Data Structure ---")
str(data_clean)


# --- 4. Split Data into Training and Testing Sets ---
# We use set.seed to make our split reproducible
set.seed(123)

# Create a stratified 80/20 split. Stratification ensures that the
# proportion of "Normal" and "Attack" labels is the same in
# both the training and testing sets.
train_index <- createDataPartition(data_clean$label,
                                   p = 0.8, # 80% for training
                                   list = FALSE,
                                   times = 1)

train_data <- data_clean[train_index, ]
test_data  <- data_clean[-train_index, ]

print(paste("Training set size:", nrow(train_data)))
print(paste("Testing set size:", nrow(test_data)))

# Check the distribution of labels in both sets
print("Training set label distribution:")
print(prop.table(table(train_data$label)))
print("Testing set label distribution:")
print(prop.table(table(test_data$label)))


# --- 5. Train the Machine Learning Model (Random Forest) ---
print("Starting model training (Random Forest)...")
# We use the formula: label ~ .
# This means "predict 'label' using all other ('.') features in the data"
# We use ntree=100 (100 trees) for a quick result.
rf_model <- randomForest(
  label ~ .,
  data = train_data,
  ntree = 100,
  importance = TRUE # We need this to see feature importance later
)

print("Model training complete.")
print(rf_model)


# --- 6. Evaluate the Model ---
print("--- Model Evaluation on Test Set ---")

# Use the trained model to make predictions on the unseen test data
predictions <- predict(rf_model, test_data)

# Generate a confusion matrix and detailed statistics
# We set 'positive = "Attack"' to get metrics from the attacker's perspective
cm <- confusionMatrix(predictions, test_data$label, positive = "Attack")

print("Confusion Matrix:")
print(cm$table)

print("Overall Statistics:")
print(cm$overall[c("Accuracy", "Kappa")])

print("Class-Specific Statistics (for 'Attack' class):")
# Extract Precision, Recall, and F1-Score
print(cm$byClass[c("Precision", "Recall", "F1")])


# --- 7. View Feature Importance ---
print("--- Feature Importance ---")

# Get the importance matrix from the model
importance_df <- as.data.frame(importance(rf_model))
importance_df$Feature <- rownames(importance_df)

# Order by MeanDecreaseGini
importance_df <- importance_df %>%
  arrange(desc(MeanDecreaseGini))

print("Top 10 Most Important Features:")
print(head(importance_df[, c("Feature", "MeanDecreaseGini")], 10))

# Create a plot of feature importance
importance_plot <- ggplot(importance_df, aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() + # Flip coordinates to make it a horizontal bar chart
  labs(
    title = "Feature Importance for Intrusion Detection",
    x = "Features",
    y = "Importance (Mean Decrease Gini)"
  ) +
  theme_minimal()

# Save the plot to your computer
ggsave("feature_importance_plot.png", importance_plot, width = 10, height = 8)

print("A plot named 'feature_importance_plot.png' has been saved.")


# ---
# --- Section 8: Predict on New User-Defined Data
# ---

print("--- Starting Prediction on New User Data ---")

# 1. Create a data.frame with new data (e.g., from a live device)
# We will create two examples: one normal, one suspicious
new_iot_data <- data.frame(
  
  # Categorical
  device_id = c("IIOT_5", "IIOT_9"),
  device_type = c("Sensor", "Actuator"),
  protocol = c("MQTT", "Modbus"),
  
  # Numeric (Row 1: Normal-looking / Row 2: Suspicious-looking)
  packet_rate = c(130, 950),        # Row 2: Suspiciously high packet rate
  byte_rate = c(2100, 89000),      # Row 2: Suspiciously high byte rate
  error_rate = c(0.02, 0.01),
  temperature = c(31.5, 48.2),      # Row 2: Getting hot
  cpu_usage = c(45.0, 98.5),        # Row 2: Maxed out CPU
  memory_usage = c(60.1, 70.0),
  latency = c(88, 350),           # Row 2: Very high latency
  command_freq = c(8, 2),
  packet_rate_change = c(5, 800),
  byte_rate_change = c(150, 75000),
  cpu_temp_ratio = c(1.42, 2.04),
  avg_latency_5s = c(90.5, 340.0),
  high_latency_flag = c(0, 1)       # Row 2: Flag is high
)

# 2. Match the categorical factor levels from the original training data
# This is a critical step!
new_iot_data$device_id <- factor(new_iot_data$device_id, levels = levels(train_data$device_id))
new_iot_data$device_type <- factor(new_iot_data$device_type, levels = levels(train_data$device_type))
new_iot_data$protocol <- factor(new_iot_data$protocol, levels = levels(train_data$protocol))


# 3. Make the final prediction
final_prediction <- predict(rf_model, new_iot_data)

# 4. Get the prediction probabilities (confidence scores)
prediction_scores <- predict(rf_model, new_iot_data, type = "prob")


# --- Show the Results ---
print("--- New Data to be Checked ---")
print(new_iot_data)

print("--- Final Prediction Label ---")
# This will output [1] "Normal" "Attack" (or similar)
print(final_prediction)

print("--- Prediction Confidence Scores ---")
# This will show the model's confidence for each class
print(prediction_scores)

# You can combine them for a nice final report:
final_report <- data.frame(
  Device = new_iot_data$device_id,
  Prediction = final_prediction,
  Attack_Confidence = prediction_scores[, "Attack"],
  Normal_Confidence = prediction_scores[, "Normal"]
)

print("--- Combined Final Report ---")
print(final_report)

print("--- ENTIRE SCRIPT COMPLETE ---")