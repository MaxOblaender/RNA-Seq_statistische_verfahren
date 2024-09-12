# Example p-values
p_data=read.csv("fdr_sim_models/ 3_sig _p_values_fdr.csv")

# Sort p-values and keep track of original indices
sorted_indices <- order(p_data$p_value)
sorted_p_values <- p_data$p_value[sorted_indices]
q <- 0.05

# Number of tests (total number of p-values)
m <- length(p_data$p_values)  # All genes

# Apply the Benjamini-Hochberg procedure to control FDR
p_data$significant_fdr <- p.adjust(sorted_p_values, method = "BH") <= q

write.csv(p_data, "p_values_fdr_max.csv")
