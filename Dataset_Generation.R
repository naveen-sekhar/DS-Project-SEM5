# Load required libraries
library(dplyr)
library(lubridate)

set.seed(42)
n <- 5000

# Base features
timestamp <- seq(from = as.POSIXct("2025-10-27 10:00:00"),
                 by = "5 min", length.out = n)
device_id <- paste0("IIOT_", sample(1:10, n, replace = TRUE))
device_type <- sample(c("Sensor", "Actuator", "Controller"), n, replace = TRUE, prob = c(0.5, 0.3, 0.2))
protocol <- sample(c("MQTT", "Modbus", "HTTP"), n, replace = TRUE, prob = c(0.4, 0.4, 0.2))

packet_rate <- round(rnorm(n, mean = 120, sd = 40))
byte_rate <- round(rnorm(n, mean = 2000, sd = 700))
error_rate <- round(runif(n, 0, 0.1), 3)
temperature <- round(rnorm(n, mean = 30, sd = 5), 1)
cpu_usage <- round(runif(n, 20, 90), 1)
memory_usage <- round(runif(n, 30, 85), 1)
latency <- round(rnorm(n, mean = 100, sd = 40))
command_freq <- round(runif(n, 1, 10))

# Introduce time-based changes
packet_rate_change <- c(NA, diff(packet_rate))
byte_rate_change <- c(NA, diff(byte_rate))
cpu_temp_ratio <- round(cpu_usage / temperature, 2)

# Rolling average latency (simulate last 5 samples)
avg_latency_5s <- zoo::rollmean(latency, k = 5, fill = NA, align = "right")

# Flag for high latency
high_latency_flag <- ifelse(latency > 150, 1, 0)

# Simulated attack labels
attack_type <- sample(
  c("Normal", "DoS", "MITM", "Data Injection", "Spoofing"),
  n, replace = TRUE, prob = c(0.6, 0.15, 0.1, 0.1, 0.05)
)
label <- ifelse(attack_type == "Normal", 0, 1)

# Combine all columns
iiot_data <- data.frame(
  timestamp, device_id, device_type, protocol, packet_rate, byte_rate,
  error_rate, temperature, cpu_usage, memory_usage, latency, command_freq,
  packet_rate_change, byte_rate_change, cpu_temp_ratio, avg_latency_5s,
  high_latency_flag, attack_type, label
)

# Replace NAs in rolling windows with mean
iiot_data$packet_rate_change[is.na(iiot_data$packet_rate_change)] <- 0
iiot_data$byte_rate_change[is.na(iiot_data$byte_rate_change)] <- 0
iiot_data$avg_latency_5s[is.na(iiot_data$avg_latency_5s)] <- mean(iiot_data$latency, na.rm = TRUE)

# View sample
head(iiot_data)

# Save dataset
write.csv(iiot_data, "iiot_intrusion_dataset_enriched.csv", row.names = FALSE)
cat("Enriched IIoT dataset saved as 'iiot_intrusion_dataset_enriched.csv'\n")

