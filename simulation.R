       require(MASS)
    ### Signifikansüberprüfung: siehe Übung 11_b (Likelihood-Quotienten-Test?) ###

    ### Einlesen von den Modelldaten ###
    model_list=list()
    files = list.files(path="models", full.names=TRUE)

    # Simulation von Daten basierend auf der negativen Binomialverteilung -> response gibt Mittelwert für die nb verteilung zurück?)

    simulated_results_mock <- list()
    simulated_results_treated <- list()
    num_simulations = 10 #hier auf gewuenschten Wert anpassen TODO

    for (file in files) {
        model <- readRDS(file)

        mu_mock <- predict(model, newdata = data.frame(treatment = "mock", time = c(1, 2, 3)), type = "response")# type = "response" gibt den Mittelwert (mu) zurück
        mu_treated <- predict(model, newdata = data.frame(treatment = "hrcc", time = c(1, 2, 3)), type = "response") #kann man bestimmt auch in einem berechnen. Hab das jetzt nur zur Übersicht in zwei geteilt.
        
        simulated_data_mock <- replicate(num_simulations, rnegbin(n = length(mu), mu = mu_mock, theta = model$theta)) # mit n = 1000 hätte die means berechnung schwerer gemacht. deshalb replicate
        simulated_data_treated <- replicate(num_simulations, rnegbin(n = length(mu), mu = mu_treated, theta = model$theta))#jedes modell hat wohl schon ein theta
        
        mean_simulated_data_mock <- rowMeans(simulated_data_mock) #hier wird der durchschnitt berechnet
        mean_simulated_data_treated <- rowMeans(simulated_data_treated)

        simulated_results_mock[[file]] <- mean_simulated_data_mock
        simulated_results_treated[[file]] <- mean_simulated_data_treated
    }

    print(simulated_results_mock)
    print(simulated_results_treated)