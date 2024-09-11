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

# plotten der Daten
# Histogramm von Differenz der FDR signifikanz zu normaler?