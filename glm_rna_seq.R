### READ IN ###

arab_data = read.csv("arab.csv")

#weiß nicht, ob das nötig ist. Eigentlich nicht
arab_data$time <- as.factor(arab_data$time)
arab_data$treatment <- as.factor(arab_data$treatment)

### MODEL ###

model <- glm.nb(gene1 ~ treatment + time, data = arab_data)

summary(model)

results <- list()

# Schleife über alle Gene um den Behandlungseffekt für jedes Gen zu schätzen
for (gene in colnames(data)[4:ncol(data)]) { # erste drei Zeilen sind nur treatment und zeit
    
    formula <- as.formula(paste(gene, "~ treatment + time")) # as.formula kann den string als formel auszuführen

    model <- glm(formula, family = negative.binomial(1), data = arab_data) # ich weiß nicht mehr genau, wie man die glms richtig hier aufschreibt in R. @JOhanna wollen wir die Uebungen auch einfach in Git hochladen?
    results[[gene]] <- summary(model)
}

results$Gen1