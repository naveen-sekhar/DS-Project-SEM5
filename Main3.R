# --- 1. Install and Load Required Packages ---
# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("caret")
# install.packages("randomForest")
# install.packages("pROC")      
# install.packages("reshape2")  

library(tidyverse)    # For data manipulation (dplyr, readr)
library(ggplot2)      # For plotting
library(caret)        # For data splitting and model evaluation
library(randomForest) # For the Random Forest classification model
library(pROC)         # For ROC curve analysis
library(reshape2)     # For formatting the confusion matrix for ggplot

# --- 2. Load the Data ---
file_path <- "iiot_intrusion_dataset_enriched.csv"
iiot_data <- readr::read_csv(file_path)

print("Data loaded successfully.")
print("--- Initial Data Structure ---")
str(iiot_data)


# --- 3. Preprocessing and Feature Engineering ---
data_clean <- iiot_data %>%
  select(-timestamp, -attack_type)

# Convert categorical features to factors
data_clean <- data_clean %>%
  mutate(
    device_id = as.factor(device_id),
    device_type = as.factor(device_type),
    protocol = as.factor(protocol)
  )

# Convert the numeric 'label' (0 or 1) into a descriptive factor
data_clean <- data_clean %>%
  mutate(
    label = as.factor(ifelse(label == 0, "Normal", "Attack"))
  )

# Check for and handle any missing values (NA)
rows_before <- nrow(data_clean)
data_clean <- na.omit(data_clean)
rows_after <- nrow(data_clean)
print(paste("Removed", rows_before - rows_after, "rows with missing values."))

print("--- Cleaned Data Structure ---")
str(data_clean)


# --- 4. Split Data into Training and Testing Sets ---
set.seed(123)
train_index <- createDataPartition(data_clean$label,
                                   p = 0.8, # 80% for training
                                   list = FALSE,
                                   times = 1)

train_data <- data_clean[train_index, ]
test_data  <- data_clean[-train_index, ]

print(paste("Training set size:", nrow(train_data)))
print(paste("Testing set size:", nrow(test_data)))


# --- 5. Train the Machine Learning Model (Random Forest) ---
print("Starting model training (Random Forest)...")

rf_model <- randomForest(
  label ~ .,
  data = train_data,
  ntree = 100,
  importance = TRUE
)

print("Model training complete.")
print(rf_model)


# --- 6. Evaluate the Model (Text Output) ---
print("--- Model Evaluation on Test Set ---")

# Use the trained model to make predictions on the unseen test data
predictions <- predict(rf_model, test_data)

# Generate a confusion matrix and detailed statistics
cm <- confusionMatrix(predictions, test_data$label, positive = "Attack")

print("Confusion Matrix:")
print(cm$table)

print("Overall Statistics:")
print(cm$overall[c("Accuracy", "Kappa")])
print(paste("Test Accuracy:", round(as.numeric(cm$overall["Accuracy"]), 4)))

print("Class-Specific Statistics (for 'Attack' class):")
print(cm$byClass[c("Precision", "Recall", "F1")])


# --- 7. Visualization: Confusion Matrix and ROC Curve ---

# Convert confusion matrix to dataframe for plotting
cm_table <- as.data.frame(cm$table)
colnames(cm_table) <- c("Prediction", "Reference", "Freq")

# --- Confusion Matrix Heatmap ---
cm_plot <- ggplot(cm_table, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color = "black", size = 6) +
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  labs(
    title = "Confusion Matrix - Intrusion Detection (Random Forest)",
    x = "Actual Class",
    y = "Predicted Class"
  ) +
  theme_minimal(base_size = 14)

ggsave("confusion_matrix_plot.png", cm_plot, width = 7, height = 6)
print("Confusion matrix heatmap saved as 'confusion_matrix_plot.png'.")

# --- ROC Curve ---
# Convert labels to numeric for ROC
test_data$label_numeric <- ifelse(test_data$label == "Attack", 1, 0)
pred_prob <- predict(rf_model, test_data, type = "prob")[, "Attack"]

roc_obj <- roc(test_data$label_numeric, pred_prob)

roc_plot <- ggplot() +
  geom_line(aes(x = 1 - roc_obj$specificities, y = roc_obj$sensitivities), color = "blue", size = 1.2) +
  geom_abline(linetype = "dashed", color = "gray") +
  labs(
    title = paste("ROC Curve (AUC =", round(auc(roc_obj), 3), ")"),
    x = "False Positive Rate",
    y = "True Positive Rate"
  ) +
  theme_minimal(base_size = 14)

ggsave("roc_curve_plot.png", roc_plot, width = 7, height = 6)
print("ROC curve saved as 'roc_curve_plot.png'.")


# --- 8. View Feature Importance ---
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
print("Feature importance plot saved as 'feature_importance_plot.png'.")


# --- 9. Predict on New User-Defined Data ---
print("--- Starting Prediction on New User Data ---")

# 1. Create a data.frame with new data
new_iot_data <- data.frame(
  
  # Categorical
  device_id = c("IIOT_5", "IIOT_9"),
  device_type = c("Sensor", "Actuator"),
  protocol = c("MQTT", "Modbus"),
  
  # Numeric (Row 1: Normal-looking / Row 2: Suspicious-looking)
  packet_rate = c(130, 950),
  byte_rate = c(2100, 89000),
  error_rate = c(0.02, 0.01),
  temperature = c(31.5, 48.2),
  cpu_usage = c(45.0, 98.5),
  memory_usage = c(60.1, 70.0),
  latency = c(88, 350),
  command_freq = c(8, 2),
  packet_rate_change = c(5, 800),
  byte_rate_change = c(150, 75000),
  cpu_temp_ratio = c(1.42, 2.04),
  avg_latency_5s = c(90.5, 340.0),
  high_latency_flag = c(0, 1)
)

# 2. Match the categorical factor levels from the original training data
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

# Combine them for a nice final report:
final_report <- data.frame(
  Device = new_iot_data$device_id,
  Prediction = final_prediction,
  Attack_Confidence = prediction_scores[, "Attack"],
  Normal_Confidence = prediction_scores[, "Normal"]
)

print("--- Combined Final Report ---")
print(final_report)


# --- 10. Visualization for Final Predictions ---

prediction_df <- data.frame(
  Device = new_iot_data$device_id,
  Attack_Prob = prediction_scores[, "Attack"],
  Normal_Prob = prediction_scores[, "Normal"])

# Melt the dataframe for easy plotting with ggplot
prediction_df_melt <- reshape2::melt(prediction_df, id.vars = "Device", 
                                     variable.name = "Prediction_Type", 
                                     value.name = "Probability")


prob_plot <- ggplot(prediction_df_melt, aes(x = Device, y = Probability, fill = Prediction_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Attack_Prob" = "#E41A1C", "Normal_Prob" = "#4DAF4A"),
                    labels = c("Attack", "Normal")) +
  labs(
    title = "Prediction Confidence for New IoT Data",
    x = "Device ID",
    y = "Confidence Probability",
    fill = "Prediction"
  ) +
  theme_minimal(base_size = 14)

ggsave("prediction_confidence_plot.png", prob_plot, width = 7, height = 6)
print("Prediction confidence bar chart saved as 'prediction_confidence_plot.png'.")


print("--- ENTIRE SCRIPT COMPLETE ---")