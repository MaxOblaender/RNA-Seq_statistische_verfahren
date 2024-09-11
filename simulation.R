# Alle gespeicherten Variablen löschen
rm(list = ls())

# Laden der benötigten Bibliotheken
require(MASS)
library(dplyr)

# Laden der Dateien aus dem Verzeichnis "sim_models"
files <- list.files(path = "sim_models", full.names = TRUE)

# Extrahieren von signifikanten Genen
p_values <- read.csv("p_values_for_genes.csv")

significant_ids <- p_values %>%
  filter(significant == TRUE) %>%
  pull(id)

# Beachte das Leerzeichen vor der Dateiendung
significant_files <- files[basename(files) %in% paste0(significant_ids, " .RData")]

# Extrahieren von nicht signifikanten Genen
non_significant_ids <- p_values %>%
  filter(significant == FALSE) %>%
  pull(id)

non_significant_files <- files[basename(files) %in% paste0(non_significant_ids, " .RData")]

# Funktion zum Laden von Modellen
load_model <- function(file) {
  readRDS(file)
}

# Modelle laden und IDs (Dateinamen ohne Pfad und Erweiterung) zuordnen
significant_models <- lapply(significant_files, load_model)
names(significant_models) <- gsub("\\ .RData$", "", basename(significant_files))

non_significant_models <- lapply(non_significant_files, load_model)
names(non_significant_models) <- gsub("\\ .RData$", "", basename(non_significant_files))

# Funktion zur Simulation von Genexpressionsdaten für nicht signifikante Gene
simulate_non_significant <- function(model, model_name, num_simulations) {
  mu_mock <- predict(model, newdata = data.frame(treatment = "mock", type = "response"))
  theta <- model$theta
  
  # Simulation von Daten
  mock_sim <- replicate(num_simulations, rnegbin(n = length(mu_mock), mu = mu_mock, theta = theta))
  #hrcc_sim <- replicate(num_simulations, rnegbin(n = length(mu_mock), mu = mu_mock, theta = theta))
  
  # Simulierte Daten in ein DataFrame umwandeln
  sim_df <- as.data.frame(t(c(mock_sim, mock_sim)))
  return(sim_df)
}

# Funktion zur Simulation von Genexpressionsdaten für signifikante Gene
simulate_significant <- function(model, model_name, num_simulations) {
  mu_mock <- predict(model, newdata = data.frame(treatment = "mock", type = "response"))
  mu_hrcc <- predict(model, newdata = data.frame(treatment = "hrcc", type = "response"))
  theta <- model$theta
  
  # Simulation von Daten
  mock_sim <- replicate(num_simulations, rnegbin(n = length(mu_mock), mu = mu_mock, theta = theta))
  hrcc_sim <- replicate(num_simulations, rnegbin(n = length(mu_hrcc), mu = mu_hrcc, theta = theta))
  
  # Simulierte Daten in ein DataFrame umwandeln
  sim_df <- as.data.frame(t(c(mock_sim, hrcc_sim)))
  return(sim_df)
}

# Schleife über die Werte von num_simulations von 1 bis 10
for (num_simulations in 1:10) {
  
  # Simulierte Daten für nicht signifikante Gene
  simulated_data_non_significant <- mapply(simulate_non_significant, 
                                           non_significant_models, 
                                           names(non_significant_models), 
                                           num_simulations = num_simulations)
  print(paste0("Number of simulations: ", num_simulations))
  print("Dim of non sig. data: ")
  print(dim(simulated_data_non_significant))
  
  names <- c(rep("mock", num_simulations), 
            rep("hrcc", num_simulations))
  rownames(simulated_data_non_significant) <- names
  
  write.csv(simulated_data_non_significant, paste0("sim_results/", num_simulations, "_nonsig.csv"))
  
  # Simulierte Daten für signifikante Gene
  simulated_data_significant <- mapply(simulate_significant, 
                                       significant_models, 
                                       names(significant_models), 
                                       num_simulations = num_simulations)
  print("Dim of sig. data: ")
  print(dim(simulated_data_significant))
  
  rownames(simulated_data_significant) <- names
  
  write.csv(simulated_data_significant, paste0("sim_results/", num_simulations,"_sig.csv"))
}

# Hinweis: In ein paar spalten werden NAs produziert -> vielleicht noch entfernen?
