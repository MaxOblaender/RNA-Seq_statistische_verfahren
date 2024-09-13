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
  fdr_non_significant = numeric(0),
  wrong_sig = numeric(0),
  fdr = numeric(0)
)

# Loop through each file and process based on file type
for (file in files) {

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

  wrong_sig = significant - fdr_significant

  fdr = wrong_sig / significant
  
  # Add the results to sim_df
  sim_df[nrow(sim_df) + 1, ] = c(file_type, n, significant, non_significant, fdr_significant, fdr_non_significant, wrong_sig, fdr)
}

# Convert necessary columns to numeric, because everything is stored as character in a data.frame
sim_df$number_of_sim_data <- as.numeric(sim_df$number_of_sim_data)
sim_df$fdr_significant <- as.numeric(sim_df$fdr_significant)
sim_df$significant <- as.numeric(sim_df$significant)
sim_df$wrong_sig <- as.numeric(sim_df$wrong_sig)
sim_df$fdr <- as.numeric(sim_df$fdr)

# Use ggplot2 to create the plot
plot = ggplot(sim_df, aes(x = number_of_sim_data)) +
  geom_point(aes(y = significant, color = "Significant"), size = 3) +   # Plot points for significant
  geom_line(aes(y = significant, color = "Significant", group = file_type), size = 1) +  # Line for significant
  
  geom_point(aes(y = fdr_significant, color = "FDR Significant"), size = 3) +   # Plot points for FDR significant
  geom_line(aes(y = fdr_significant, color = "FDR Significant", group = file_type), size = 1) +  # Line for FDR significant
  
  scale_color_manual(values = c("Significant" = "blue", "FDR Significant" = "red")) +  # Set custom colors
  labs(x = "Anzahl der Simulationen", 
       y = "Anzahl signifikanter Gene", 
       color = "Signifikanz",  # Label for the legend
       title = "Significante Gene vor und nach FDR Analyse")  

# Save the plot
ggsave(filename = "fdr_and_significant_genes_plot.png",   # File name
       plot = plot,                                       # Plot object
       width = 10,                                        # Width in inches
       height = 6)

#write.csv(sim_df, "plot_df.csv")

# Use ggplot2 to create the plot
plot = ggplot(sim_df, aes(x = number_of_sim_data)) +
  geom_point(aes(y = wrong_sig, color = "Significant"), size = 3) +   # Plot points for significant
  geom_line(aes(y = wrong_sig, color = "Significant", group = file_type), size = 1) +  # Line for significant
  
  scale_color_manual(values = c("Significant" = "blue")) +  # Set custom colors
  labs(x = "Anzahl der Simulationen", 
       y = "Anzahl falsch signifikanter Gene", 
       color = "Signifikanz",  # Label for the legend
       title = "Falsch klassifizierte Gene")  

# Save the plot
ggsave(filename = "wrongly_significant.png",   # File name
       plot = plot,                                       # Plot object
       width = 10,                                        # Width in inches
       height = 6)

       # Use ggplot2 to create the plot
plot = ggplot(sim_df, aes(x = number_of_sim_data)) +
  geom_point(aes(y = fdr, color = "Significant"), size = 3) +   # Plot points for significant
  geom_line(aes(y = fdr, color = "Significant", group = file_type), size = 1) +  # Line for significant
  
  scale_color_manual(values = c("Significant" = "blue")) +  # Set custom colors
  labs(x = "Anzahl der Simulationen", 
       y = "Anzahl falsch signifikanter Gene", 
       color = "Signifikanz",  # Label for the legend
       title = "Falsch klassifizierte Gene")  

# Save the plot
ggsave(filename = "fdr.png",   # File name
       plot = plot,                                       # Plot object
       width = 10,                                        # Width in inches
       height = 6)