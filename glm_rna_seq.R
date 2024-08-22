# Programm erstellt Modelle für alle Gene

require(MASS)

### READ IN ###

arab_data = read.csv("arab.csv")

names=colnames(arab_data)

#model = glm.nb(arab_data[,7] ~ as.factor(treatment)+time, data = arab_data)
#saveRDS(model, file=paste("models/",names[7],".RData"))

# Schleife über alle Gene um den Behandlungseffekt für jedes Gen zu schätzen
# erste drei Zeilen sind nur treatment und zeit
i=3
for (gene in arab_data[, 4:ncol(arab_data)]) {
    i=i+1
    # Modell wird für jedes Gen berechnet und in die Liste results hinzugefügt
    # TODO: link-Funktion
    model = glm.nb(gene ~ as.factor(treatment)+time, data = arab_data)
    saveRDS(model, file=paste("models/",names[i],".RData"))
    
    #results[[i]]= model
    #results=list(results, model)
    
    #results = append(results, model)
}
