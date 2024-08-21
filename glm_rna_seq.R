require(MASS)

### READ IN ###

arab_data = read.csv("arab.csv")

### MODEL ### Zum testen

gene1 = arab_data[,4]
model = glm.nb(gene1 ~ as.factor(treatment) + time, data = arab_data) # hier können wir uns noch eine link funktion überlegen
# mit link = log z.B. 
#Ich bin mir nicht mehr sicher mit dem treatment und den Dummyvariablen?

print(summary(model))

save(model, file = "test_model.RData")

results = list()

# Schleife über alle Gene um den Behandlungseffekt für jedes Gen zu schätzen
# erste drei Zeilen sind nur treatment und zeit
for (gene in arab_data[,4:ncol(arab_data)]) {

    #as.formula kann den string als Formel auszuführen
    formula = as.formula(paste(gene, "~ as.factor(treatment) + time"))

    model = glm.nb(formula, data = arab_data) # ich weiß nicht mehr genau, wie man die glms richtig hier aufschreibt in R. # nolint: line_length_linter.
    # @Johanna wollen wir die Uebungen auch einfach in Git hochladen?
    results[[gene]] = summary(model)
    #hier ist die Frage ob wir die summary oder das modell selber speichern möchten

# Extrahiere den p-Wert für die Behandlungsvariable
    treatment_p_value <- coef(model_summary)["treatment2", "Pr(>|z|)"] # p-Wert für die zweite Treatment-Stufe (Behandlung)
    
    # Wenn der p-Wert kleiner als 0,05 ist, als signifikant betrachten
    if (!is.na(treatment_p_value) && treatment_p_value < 0.05) {
        significant_genes[[gene_name]] <- treatment_p_value
    }
}

save(results, file="complete_model.RData")
print(results$Gen1)
