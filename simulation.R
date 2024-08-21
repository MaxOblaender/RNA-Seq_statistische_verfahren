### Signifikansüberprüfung: siehe Übung 11_b (Likelihood-Quotienten-Test?)

arab_model=load("model.RData")

new_sim_arab_data=""
i=0

for (model in arab_model){
    for (i){
        #Simulieren anhand der Annahme für das Treatment
        predict(model, newdata=data.frame(treatment="mock"))
        predict(model, newdata=data.frame(treatment="hcc"))
    }
    #Durchschnitt der 1000 Simulationen
    
    
}
