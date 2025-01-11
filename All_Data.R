# Install required libraries if not already installed
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")

# Load required libraries
library(dplyr)

# Set the directory containing your CSV files
csv_directory <- "divvy-tripdata"

# List all CSV files in the directory
csv_files <- list.files(path = csv_directory, pattern = "\\.csv$", full.names = TRUE)

# Read all CSV files and combine them into one data frame
trip_data <- do.call(rbind, lapply(csv_files, read.csv))

# View the combined data frame
head(trip_data)