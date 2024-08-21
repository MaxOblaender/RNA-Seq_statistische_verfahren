require(MASS)

### READ IN ###

arab_data = read.csv("arab.csv")


### MODEL ### Zum testen

gene1 = arab_data[,4]
model = glm.nb(gene1 ~ as.factor(treatment) + time, data = arab_data) # hier können wir uns noch eine link funktion überlegen
# mit link = log z.B. 
#Ich bin mir nicht mehr sicher mit dem treatment und den Dummyvariablen?

print(summary(model))

results = list()

# Schleife über alle Gene um den Behandlungseffekt für jedes Gen zu schätzen
# erste drei Zeilen sind nur treatment und zeit
for (gene in colnames(data)[4:ncol(data)]) {

    #as.formula kann den string als Formel auszuführen
    formula = as.formula(paste(gene, "~ as.factor(treatment) + time"))

    model = glm.nb(formula, data = arab_data) # ich weiß nicht mehr genau, wie man die glms richtig hier aufschreibt in R. # nolint: line_length_linter.
    # @Johanna wollen wir die Uebungen auch einfach in Git hochladen?
    results[[gene]] = summary(model)
}

print(results$Gen1)
