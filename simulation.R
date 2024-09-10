require(MASS)
library(dplyr)

files = list.files(path = "sim_models", full.names = TRUE)

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
names(significant_models) = gsub("\\ .RData$", "", basename(significant_files))

non_significant_models = lapply(non_significant_files, load_model)
names(non_significant_models) = gsub("\\ .RData$", "", basename(non_significant_files))

# Funktion zur Simulation von Genexpressionsdaten
simulate_non_significant = function(model, model_name, num_simulations) {
  # mu = mean(data für Gen)
  # var = var(data für gen)
  # theta = mu^2/(var-mu)

  mu_mock = predict(model, newdata = data.frame(treatment = "mock", type = "response"))
  mu_hrcc = predict(model, newdata = data.frame(treatment = "hrcc", type = "response"))

  theta = model$theta

  mock_sim = replicate(num_simulations, rnegbin(n = length(mu_mock), mu = mu_mock, theta = theta))
  hrcc_sim = replicate(num_simulations, rnegbin(n = length(mu_hrcc), mu = mu_hrcc, theta = theta))
    
  # Simulierte Daten in ein DataFrame umwandeln
  sim_df = as.data.frame(t(c(mock_sim, hrcc_sim)))
  #sim_df$id = model_name  # Verwende den Namen der Datei als ID
  a = c(rep("mock", num_simulations), rep("hrcc", num_simulations))
  print(a)
  print(nrow(sim_df))

  return(sim_df)
}

# TODO schleife über anz der replikate
# sim_models einlesen
# 
#1,2,3 ... 10 neue daten für jedes gen simulieren
# neue modelle darauf erstellen
#fdr darauf untersuchen

#nicht signifikant: ein mittelwert und ein theta aus ausgangsdaten -> 1 x n treatment und nicht treatment simulieren
#signifikant: 1 x n predict mit treated und 1 x n predict mock.

# Funktion zur Simulation von signifikanten Genen
simulate_significant = function(model, model_name, num_simulations) { #nicht num silmulations sonder num werten

  mu_mock = predict(model, newdata = data.frame(treatment = "mock", type = "response"))
  mu_hrcc = predict(model, newdata = data.frame(treatment = "hrcc", type = "response"))

  theta = model$theta

  mock_sim = replicate(num_simulations, rnegbin(n = length(mu_mock), mu = mu_mock, theta = theta))
  hrcc_sim = replicate(num_simulations, rnegbin(n = length(mu_hrcc), mu = mu_hrcc, theta = theta))
    
  # Simulierte Daten in ein DataFrame umwandeln
  sim_df = as.data.frame(t(c(mock_sim, hrcc_sim)))

  return(sim_df)
}

num_simulations = 3
# Simulierte Daten für nicht signifikante Gene
simulated_data_non_significant = mapply(simulate_non_significant, non_significant_models, names(non_significant_models), num_simulations = num_simulations)

# Simulierte Daten für signifikante Gene
simulated_data_significant = mapply(simulate_significant, significant_models, names(significant_models), num_simulations = num_simulations)
rownames(simulated_data_significant) = c(rep("mock", num_simulations), rep("hrcc", num_simulations))
write.csv(simulated_data_significant,"sim_3_sig.csv")

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
