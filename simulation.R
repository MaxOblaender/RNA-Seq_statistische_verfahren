require(MASS)
library(dplyr)

files = list.files(path = "models", full.names = TRUE)

# Extrahieren von signifikanten Genen
p_values = read.csv("p_values_for_genes.csv")

significant_ids = p_values %>%
  filter(significant == TRUE) %>%
  pull(id)

# hier Lehrzeichen vor file Endung beachten
significant_files <- files[basename(files) %in% paste0(significant_ids, " .RData")]

# extrahieren von nicht signifikanten Genen
non_significant_ids = p_values %>%
  filter(significant == FALSE) %>%
  pull(id)

non_significant_files <- files[basename(files) %in% paste0(non_significant_ids, " .RData")]

# Simulation von Daten basierend auf der negativen Binomialverteilung -> response gibt Mittelwert für die nb verteilung zurück?

load_model <- function(file) {
    readRDS(file)
  }

# Load models
significant_models <- lapply(significant_files, load_model)
non_significant_models <- lapply(non_significant_files, load_model)

# Function to simulate gene expression data
simulate_gene_expression <- function(model, num_simulations = 1) {
    mu <- predict(model, newdata = data.frame(treatment = "mock", time = c(1, 2, 3)), type = "response")
    theta <- model$theta
    replicate(num_simulations, rnegbin(n = length(mu), mu = mu, theta = theta))
  }

# Simulate data for non-significant genes (same expression)
simulated_data_non_significant <- lapply(non_significant_models, simulate_gene_expression)

# Simulate data for significant genes (different expression)
# Assuming a change in mean expression for significant genes
simulate_gene_expression_significant <- function(model, num_simulations = 1) {
    mu <- predict(model, newdata = data.frame(treatment = "hrcc", time = c(1, 2, 3)), type = "response")
    theta <- model$theta
    replicate(num_simulations, rnegbin(n = length(mu), mu = mu, theta = theta))
  }

# Simulate data for significant genes
simulated_data_significant <- lapply(significant_models, simulate_gene_expression_significant)
print(simulated_data_non_significant)

# Example to combine and process the simulated data
# Combine all simulated data for further analysis
all_simulated_data <- c(simulated_data_non_significant, simulated_data_significant)

# Example processing: Convert to a data frame if needed
# This step depends on your specific analysis needs

write.csv(all_simulated_data, "all_simulated_data.csv", row.names = FALSE)
print("Simulation complete.")