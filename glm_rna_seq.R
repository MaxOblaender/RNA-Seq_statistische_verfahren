# Programm erstellt Modelle für alle Gene

require(MASS)

### READ IN ###

arab_data = read.csv("arab.csv")

# Initialisierung der Ergebnisliste
results = list()

# Schleife über alle Gene um den Behandlungseffekt für jedes Gen zu schätzen
# erste drei Zeilen sind nur treatment und zeit
i=0
for (gene in arab_data[, 4:ncol(arab_data)]) {
    i=i+1
    # Modell wird für jedes Gen berechnet und in die Liste results hinzugefügt
    # TODO: link-Funktion
    model = glm.nb(gene ~ as.factor(treatment)+time, data = arab_data)
    results[[i]]= model
    #results=list(results, model)
    
    #results = append(results, model)
}

saveRDS(results, file="model.RData")
