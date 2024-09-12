# Programm bestimmt, ob die Gene (durch die jeweiligen Modelle beschrieben) signifikante Expression vorweisen
# Alle gespeicherten Variablen löschen
rm(list = ls())

files = list.files(path="models", full.names=TRUE)

### Bestimmmung der Signifikanz anhand der p-Werte 
p_values=data.frame(
    id=character(0),
    p_value=numeric(0),
    significant=logical(0)
)

for (file in files) {
    model <- readRDS(file)
    # Alle p-Werte des Modells (Spalte Pr>|t|)
    # zweiter Wert von p stellt p-Wert des treatments dar
    # ist dieses nicht signifikant, ist dieser über 0.05
    p=summary(model)$coefficients[,4]
    signif=TRUE
    if (p[4]>=0.05){
        signif=FALSE
    }
    name=strsplit(file, split="/")[[1]][2]
    name=strsplit(name, split=" ")[[1]][1]
    p_values[nrow(p_values)+1,]=c(name,p[4],signif)
    
}

write.csv(p_values, "p_values_for_genes.csv")

count_significance <- sum(p_values$significant == TRUE)
count_non_significant = sum(p_values$significant == FALSE)