# ML-Driven Intrusion Detection for Industrial IoT Environments

![Language: R](https://img.shields.io/badge/Language-R-blue.svg) ![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)

A compact R-based pipeline that trains and evaluates a Random Forest model to detect network intrusions in Industrial IoT (IIoT) traffic. The repository includes a pre-generated dataset, dataset generation script, model training/evaluation script, and visualization/export of results.

## Table of Contents

- [Quickstart](#quickstart)
- [Prerequisites](#prerequisites)
- [Project files](#project-files)
- [Dataset](#dataset)
- [Features & Pipeline](#features--pipeline)
- [Running the project](#running-the-project)
- [Outputs](#outputs)
- [Contributing](#contributing)
- [License](#license)

## Quickstart

1. Make sure you have R (>= 3.6) installed.
2. From R or RStudio, install the required packages (see Prerequisites).
3. Run `Main2.R` to train and evaluate the model, or run `Dataset_Generation.R` to re-generate the dataset.

## Prerequisites

Install required packages from within R or RStudio:

```r
install.packages(c("tidyverse", "ggplot2", "caret", "randomForest", "pROC", "reshape2"))
```

If you prefer a minimal subset, `caret` and `randomForest` are required for training; `ggplot2`/`reshape2` help with plotting.

## Project files

- `Main2.R`         - Main end-to-end script: preprocessing, model training, evaluation, visualizations, and live prediction examples.
- `Dataset_Generation.R` - Optional script to (re)generate the `iiot_intrusion_dataset_enriched.csv` dataset.
- `iiot_intrusion_dataset_enriched.csv` - Pre-generated dataset used by `Main2.R`.
- `READ.md`         - This documentation file.

## Dataset

The project uses `iiot_intrusion_dataset_enriched.csv`, a simulated IIoT traffic dataset. Key columns include:

- Categorical: `device_id`, `device_type`, `protocol`
- Numeric: `packet_rate`, `byte_rate`, `error_rate`, `cpu_usage`, `memory_usage`, `latency`
- Target: `label` (0 = Normal, 1 = Attack)

To re-generate the dataset (optional):

```r
# In RStudio or R console
source("Dataset_Generation.R")
```

## Features & Pipeline

This project demonstrates a standard machine learning pipeline implemented in `Main2.R`:

- Data loading and cleaning (factor conversions, missing value handling)
- Stratified 80/20 train/test split using `caret::createDataPartition`
- Model training using `randomForest` with formula `label ~ .`
- Evaluation: confusion matrix, accuracy, precision, recall, F1-score
- Visualizations: confusion heatmap, ROC curve with AUC, and feature importance
- Live prediction: example showing how to classify new user-defined packets

## Running the project

Recommended (interactive): open `Main2.R` in RStudio and click Run (or press Ctrl+Shift+Enter).

From an R console:

```r
setwd("path/to/project")   # set to the repository root where Main2.R lives
source("Main2.R")
```

Alternatively, run headless via Rscript from your shell (Windows PowerShell):

```powershell
# From the repo root
Rscript Main2.R
```

Notes:
- If you use the included `iiot_intrusion_dataset_enriched.csv`, no additional data preparation is required.
- If you re-generate the CSV using `Dataset_Generation.R`, ensure the output file is saved as `iiot_intrusion_dataset_enriched.csv` in the project root.

## Outputs

Running `Main2.R` produces:

- Model metrics printed to the console (accuracy, precision, recall, F1)
- Saved plot images (PNG):
  - Confusion matrix heatmap
  - ROC curve with AUC annotation
  - Feature importance plot
- Optionally printed and/or saved example live-prediction results for user-defined packets

## Contributing

Contributions are welcome. For substantial changes, please open an issue first to discuss the proposal.

## License

This project is licensed under the MIT License. See the `LICENSE` file for details.

```