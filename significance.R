# Programm bestimmt, ob die Gene (durch die jeweiligen Modelle beschrieben) signifikante Expression vorweisen
arab_model=load("model.RData")


# Ansatz 1: p-Wert der Behandlungsvariable

# Kommentare aus anderen Programm


    # Extrahiere den p-Wert für die Behandlungsvariable
    #treatment_p_value <- coef(model_summary)["treatment2", "Pr(>|z|)"] #TODO hier müssen wir uns überlegen welcher wert significant sein  muss!
    
    # Wenn der p-Wert kleiner als 0,05 ist, als signifikant betrachten
    #if (!is.na(treatment_p_value) && treatment_p_value < 0.05) {
    #    significant_genes[[gene_name]] <- treatment_p_value
    #}


# Ansatz 2: Übung 11_b (Likelihood-Quotienten-Test)

