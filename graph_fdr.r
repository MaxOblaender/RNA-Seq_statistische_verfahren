library(readr)

# Generieren eines df mit der Anzahl an simulierten Daten und der jeweiligen Anzahl an signifikanten und nicht-signifikanten Genen
files = list.files(path="fdr_sim_models", full.names=TRUE)

sim_df=data.frame(
    number_of_sim_data=numeric(0),
    significant=numeric(0),
    non_significant=numeric(0),
    fdr_significant=numeric(0),
    fdr_non_significant=numeric(0)
)

for (file in files){
    n=parse_number(file) #Anzahl der simulierten Werte

    significant=sum(p_values$significant == TRUE)
    non_significant=sum(p_values$significant == FALSE)
    fdr_significant=sum(p_values$significant_fdr == TRUE)
    fdr_non_significant=sum(p_values$significant_fdr == FALSE)
    sim_df[nrow(sim_df)+1,]=c(n,significant,non_significant,fdr_significant,fdr_non_significant)
}
print(sim_df)

plot(sim_df$number_of_sim_data, sim_df$fdr_significant,
     xlab = "Number of Simulated Data",  # Label for the x-axis
     ylab = "FDR Significant Genes",     # Label for the y-axis
     main = "FDR Significant Genes vs Number of Simulated Data",  # Title of the plot
     pch = 19,  # Point character (solid circle)
     col = "blue"  # Color of the points
)

# plotten der Daten
# Histogramm von Differenz der FDR signifikanz zu normaler?