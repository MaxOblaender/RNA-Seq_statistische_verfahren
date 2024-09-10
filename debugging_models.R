# Benötigte Pakete laden
library(MASS)  # Für glm.nb

# CSV-Datei einlesen
arab_data <- read.csv("arab.csv")

# Die erste Spalte entfernen (irrelevant)
arab_data <- arab_data[, -1]

# Die Namen der Gen-Spalten extrahieren (alles nach den ersten drei Spalten)
gene_names <- colnames(arab_data)[-(1:3)]

# Liste für die Modelle erstellen
models <- list()

# Schleife über alle Gen-Spalten
for (gene in gene_names) {
  # Überprüfe, ob die Gen-Spalte genügend Daten hat
  # Modell erstellen
  formula <- as.formula(paste(gene, "~ as.factor(time) + as.factor(treatment)"))
  tryCatch({
    model <- glm.nb(formula, data = arab_data, link = "identity")
    models[[gene]] <- model
  }, error = function(e) {
    warning(paste("Fehler bei der Anpassung des Modells für", gene, ":", e$message))
    print(gene)
  })
}