#TODO: Problem bei Berechnung einiger Modelle
# vermute, dass die Simulation für einige Gene wieder solche Werte produziert, für die kein Modell aufgestellt werden kann (0 0 0 etc.)
# Überprüfung aber noch ausstehend

require(MASS)
library(readr)
library(stringr)

files = list.files(path="sim_results", full.names=TRUE)
name_file=str_extract(files,regex("\\d+_\\w+"))

k=1
for (filename in files)
{
    sim_data=read.csv(filename)

    p_values=data.frame(
        id=character(0),
        p_value=numeric(0),
        significant=logical(0),
        significant_fdr=logical(0)
    )


    names=colnames(sim_data)

    # Bestimmung der Signifikanz über p-Test für simulierte Daten
    i=1
    for (gene in sim_data[, 2:ncol(sim_data)]) {
        i=i+1
        tryCatch({
            model = glm.nb(gene ~ as.factor(treatment), data = sim_data)
            p=summary(model)$coefficients[,4]
            signif=TRUE
            if (p[2]>=0.05){
                signif=FALSE
            }
            p_values[nrow(p_values)+1,]=c(names[i],p[2],signif,FALSE)
        }, error = function(e) {
        warning(paste("Fehler bei der Anpassung des Modells für", gene, ":", e$message))
        print(names[i])
        print(e$message)
    })
    }

    # Bestimmung der FDR für die Gene
    p_values=p_values[order(p_values$p_value),]

    #Schritt 2: Berechnung der Signifikanz
    q=0.05
    m=length(p_values$p_value)

    for (i in 1:length(p_values$p_value)){
        if (p_values$p_value[i] <= ((i/m)*q)){
            p_values$significant_fdr[i]=TRUE
        }
        else{
            p_values$significant_fdr[i]=FALSE
        }
    }
    write.csv(p_values, paste("fdr_sim_models/",name_file[k],"_p_values_fdr.csv"))
    k=k+1
}
