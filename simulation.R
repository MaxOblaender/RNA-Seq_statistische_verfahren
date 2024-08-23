    ### Signifikansüberprüfung: siehe Übung 11_b (Likelihood-Quotienten-Test?)
    require(MASS)

    # Einlesen von den Modelldaten
    model_list=list()
    files = list.files(path="models", full.names=TRUE)

    # Überlegungen
    new_data <- data.frame(treatment = "mock", time = c(1, 2, 3))

    # Simulation von Daten basierend auf der negativen Binomialverteilung -> response gibt Mittelwert für die nb verteilung zurück?)

    simulated_results <- list()
    num_simulations = 10

    for (file in files) {
        model <- readRDS(file)
        mu <- predict(model, newdata = data.frame(treatment = "mock", time = c(1, 2, 3)), type = "response")# type = "response" gibt den Mittelwert (mu) zurück
        simulated_data <- replicate(num_simulations, rnegbin(n = length(mu), mu = mu, theta = model$theta)) # mit n = 1000 hätte die means berechnung schwerer gemacht. deshalb replicate
        #jedes modell hat wohl schon ein theta

        mean_simulated_data <- rowMeans(simulated_data) #hier wird der durchschnitt berechnet

        simulated_results[[file]] <- mean_simulated_data
    }

    print(simulated_results)