    ### Signifikansüberprüfung: siehe Übung 11_b (Likelihood-Quotienten-Test?)

    require(MASS)

    # Einlesen von den Modelldaten
    model_list=list()
    files = list.files(path="models", full.names=TRUE)
    predict_list=list()


    ### Ausprobieren der Simulierung

    #model=readRDS(files[1])

    # für die Simulierung müssen zufällige Zahlen anhand der neg. Bin. Verteilung und des Modells generiert werden
    # hierfür hab ich noch keine richtige Lösung gefunden
    # in der Übung haben wir das nur für die Poisson-Verteilung gemacht, wo der Syntax anders ist -> geht hier nicht
    #test=rnegbin(1000, lambda=predict(model, newdata=data.frame(treatment="mock",time=c(1,2,3))))
    #print(test)

    # Überlegungen
    new_data <- data.frame(treatment = "mock", time = c(1, 2, 3))
    mu <- predict(model, newdata = new_data, type = "response") # type = "response" gibt den Mittelwert (mu) zurück

    # Simulation von Daten basierend auf der negativen Binomialverteilung -> response gibt Mittelwert für die nb verteilung zurück?
    simulated_data <- rnegbin(n = length(mu), mu = mu, theta = model$theta)
    print(simulated_data)
    print(model$theta)

    simulated_results <- list()

    for (file in files) {
        model <- readRDS(file)
        mu <- predict(model, newdata = data.frame(treatment = "mock", time = c(1, 2, 3)), type = "response")
        simulated_data <- rnegbin(n = length(mu), mu = mu, theta = model$theta) #jedes modell hat wohl schon ein theta
        simulated_results[[file]] <- simulated_data
    }

    print(simulated_results)

    # Modelle können nicht mit ihrer Funktion so einfach in einer Liste gespeichert werden
    # deswegen müssen während des Einlesens alle nötigen Infos generiert werden
    #i=0
    #for (file in files){
    #    i=i+1
    #    model=readRDS(file)
    #    model_list[[i]] = model
    #    predict_list[[i]]=predict(model)

    #    for (i in 1:1000){

    #    }
    #}

