p_data=read.csv("p_values for genes.csv")

#Schritt 1: Sortieren der p-Werte
 p_data=p_data[order(p_data$p_value),]

#Schritt 2: Berechnung der Signifikanz
q=0.05
m=length(p_data$p_value)

for (i in 1:length(p_data$p_value)){
    if (p_data$p_value[i] <= ((i/m)*q)){
        p_data$significant_fdr[i]=FALSE
    }
    else{
        p_data$significant_fdr[i]=TRUE
    }
}
write.csv(p_data, "p_values fdr.csv")