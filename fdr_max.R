# Load necessary libraries
library(dplyr)

# Set directory path where CSV files are stored
dir_path <- "fdr_sim_models"

# List all files in the directory that contain "_sig_"
file_list <- list.files(path = dir_path, pattern = "_sig", full.names = TRUE)

# Set the FDR threshold
q <- 0.05

# Loop through each file in the directory
for (file in file_list) {
  # Read the CSV file
  p_data <- read.csv(file)
  
  # Sort p-values and keep track of original indices
  sorted_indices <- order(p_data$p_value)
  sorted_p_values <- p_data$p_value[sorted_indices]
  
  # Number of tests (total number of p-values)
  m <- length(p_data$p_value)  # All genes
  
  # Apply the Benjamini-Hochberg procedure to control FDR
  p_data$significant_fdr <- p.adjust(sorted_p_values, method = "BH") <= q
  
  # Write the updated data with FDR to a new CSV file
  output_file <- paste0(dir_path, "/p_values_fdr_", basename(file))
  write.csv(p_data, output_file, row.names = FALSE)
  
  # Print status for the user
  cat("Processed and saved:", output_file, "\n")
}
