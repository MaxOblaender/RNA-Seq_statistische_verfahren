# TODO 

    # Programm erstellt Modelle für alle Gene

    require(MASS)

    ### READ IN ###

    arab_data = read.csv("arab.csv")

    # Namen der Gene
    names=colnames(arab_data)

    # Schleife über alle Gene um den Behandlungseffekt für jedes Gen zu schätzen
    # erste drei Zeilen sind nur treatment und zeit
    i=3
    for (gene in arab_data[, 4:ncol(arab_data)]) {
        i=i+1
        # Modell wird für jedes Gen berechnet und einzeln gespeichert im Ordner "models"
        # TODO: link-Funktion -> iteration limit reached
        model = glm.nb(gene ~ as.factor(treatment), data = arab_data, link = "identity")
        #saveRDS(model, file=paste("sim_models/",names[i],".RData"))
        print(summary(model))
        print(i)
    }

