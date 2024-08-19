### READ IN ###

arab_data = read.csv("arab.csv")

#weiß nicht, ob das nötig ist. Eigentlich nicht
arab_data$time = as.factor(arab_data$time)
arab_data$treatment = as.factor(arab_data$treatment)

### MODEL ###

model = glm.nb(gene1 ~ treatment + time, data = arab_data)
#Ich bin mir nicht mehr sicher mit dem treatment und den Dummyvariablen?

summary(model)

results = list()

# Schleife über alle Gene um den Behandlungseffekt für jedes Gen zu schätzen
# erste drei Zeilen sind nur treatment und zeit
for (gene in colnames(data)[4:ncol(data)]) {

    # as.formula kann den string als Formel auszuführen
    formula = as.formula(paste(gene, "~ treatment + time"))

    model = glm.nb(formula, data = arab_data) # ich weiß nicht mehr genau, wie man die glms richtig hier aufschreibt in R. # nolint: line_length_linter.
    # @Johanna wollen wir die Uebungen auch einfach in Git hochladen?
    results[[gene]] = summary(model)
}

results$Gen1