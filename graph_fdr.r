library(readr)
library(ggplot2)

# Alle gespeicherten Variablen löschen
rm(list = ls())

# Path to the folder where the files are stored
files = list.files(path = "fdr_sim_models", pattern = "p_values_fdr_ ", full.names = TRUE)

# Create an empty data frame to store the results
sim_df = data.frame(
  file_type = character(0),
  number_of_sim_data = numeric(0),
  significant = numeric(0),
  non_significant = numeric(0),
  fdr_significant = numeric(0),
  fdr_non_significant = numeric(0)
)

# Loop through each file and process based on file type
for (file in files) {
  
  # Identify the file type based on its name
  #if (grepl("nonsig", file)) {
  #  file_type = "nonsig"
  #} else if (grepl("sig", file)) {
  #  file_type = "sig"
  #} else {
  #  next  # Skip files that don't match the criteria
  #}
  file_type = "sig"
  # Extract the number of simulations from the filename using parse_number()
  n = parse_number(file)
  
  # Read the file into a data frame
  df = read_csv(file)
  
  # Count significant and non-significant genes
  significant = sum(df$significant == TRUE)
  non_significant = sum(df$significant == FALSE)
  
  # Count FDR significant and non-significant genes
  fdr_significant = sum(df$significant_fdr == TRUE)
  fdr_non_significant = sum(df$significant_fdr == FALSE)
  
  # Add the results to sim_df
  sim_df[nrow(sim_df) + 1, ] = c(file_type, n, significant, non_significant, fdr_significant, fdr_non_significant)
}

# Convert necessary columns to numeric, because everything is stored as character in a data.frame
sim_df$number_of_sim_data <- as.numeric(sim_df$number_of_sim_data)
sim_df$fdr_significant <- as.numeric(sim_df$fdr_significant)

# Use ggplot2 to create the plot
plot = ggplot(sim_df, aes(x = number_of_sim_data, y = fdr_significant, color = file_type)) +
  geom_point(size = 3) +    # Plot points
  geom_line(aes(group = file_type), size = 1) +  # Add lines connecting the dots
  scale_color_manual(values = c("sig" = "blue", "nonsig" = "red")) +  # Set colors manually
  labs(x = "Number of Simulated Data", 
       y = "FDR Significant Genes", 
       color = "File Type",  # Label for the legend
       title = "FDR Significant Genes vs Number of Simulated Data (Sig vs NonSig)") 

ggsave(filename = "fdr_significant_genes_plot.png",   # File name
       plot = plot,                                  # Plot object
       width = 10,                                  # Width in inches
       height = 6,                                  # Height in inches
       dpi = 300)     

write.csv(sim_df, "plot_df.csv")
