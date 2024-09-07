require(MASS)
library(dplyr)

files = list.files(path = "models", full.names = TRUE)

# Extrahieren von signifikanten Genen
p_values = read.csv("p_values_for_genes.csv")

significant_ids = p_values %>%
  filter(significant == TRUE) %>%
  pull(id)

# Beachte das Leerzeichen vor der Dateiendung
significant_files = files[basename(files) %in% paste0(significant_ids, " .RData")]

# Extrahieren von nicht signifikanten Genen
non_significant_ids = p_values %>%
  filter(significant == FALSE) %>%
  pull(id)

non_significant_files = files[basename(files) %in% paste0(non_significant_ids, " .RData")]

# Funktion zum Laden von Modellen
load_model = function(file) {
    readRDS(file)
}

# Modelle laden und IDs (Dateinamen ohne Pfad und Erweiterung) zuordnen
significant_models = lapply(significant_files, load_model)
names(significant_models) = gsub("\\.RData$", "", basename(significant_files))

non_significant_models = lapply(non_significant_files, load_model)
names(non_significant_models) = gsub("\\.RData$", "", basename(non_significant_files))

# Funktion zur Simulation von Genexpressionsdaten
simulate_gene_expression = function(model, model_name, num_simulations = 1000) {
    mu = predict(model, newdata = data.frame(treatment = "mock", time = c(1, 2, 3)), type = "response")
    theta = model$theta
    sim_data = replicate(num_simulations, rnegbin(n = length(mu), mu = mu, theta = theta))
    
    # Simulierte Daten in ein DataFrame umwandeln
    sim_df = as.data.frame(t(sim_data))
    sim_df$id = model_name  # Verwende den Namen der Datei als ID
    return(sim_df)
}

# Funktion zur Simulation von signifikanten Genen
simulate_gene_expression_significant = function(model, model_name, num_simulations = 1000) {
    mu = predict(model, newdata = data.frame(treatment = "hrcc", time = c(1, 2, 3)), type = "response")
    theta = model$theta
    sim_data = replicate(num_simulations, rnegbin(n = length(mu), mu = mu, theta = theta))
    
    # Simulierte Daten in ein DataFrame umwandeln
    sim_df = as.data.frame(t(sim_data))
    sim_df$id = model_name  # Verwende den Namen der Datei als ID
    return(sim_df)
}

# Simulierte Daten für nicht signifikante Gene
simulated_data_non_significant = mapply(simulate_gene_expression, non_significant_models, names(non_significant_models), SIMPLIFY = FALSE)

# Simulierte Daten für signifikante Gene
simulated_data_significant = mapply(simulate_gene_expression_significant, significant_models, names(significant_models), SIMPLIFY = FALSE)

# Kombinieren der simulierten Daten in einem DataFrame
simulated_data_non_significant_df = do.call(rbind, simulated_data_non_significant)
simulated_data_significant_df = do.call(rbind, simulated_data_significant)

# Optional: Eine Spalte hinzufügen, um anzugeben, ob ein Gen signifikant ist oder nicht
simulated_data_non_significant_df$significant = FALSE
simulated_data_significant_df$significant = TRUE

# Alle simulierten Daten kombinieren
all_simulated_data_df = rbind(simulated_data_non_significant_df, simulated_data_significant_df)

# CSV-Datei speichern
# Im Moment werden die wiederholten SImulationen noch hinter einander in das file geschrieben. 
# Ich wusste nicht, ob ich den durschschnitt berechnen soll, oder lieber sehen, wie oft sie significant sind (von 1000 Wiederholungen)
write.csv(all_simulated_data_df, "all_simulated_data.csv", row.names = FALSE)
print("Simulation complete.")
