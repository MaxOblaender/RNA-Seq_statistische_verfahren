# Daysabs_i ~ Poission(\mu_i)
# Modellhierarchie:
# M0: \doubleE Daysabs_i = \beta_1
# M1: \doubleE Daysabs_i = \beta_1 + \beta_2*male_i                                           H_0^((1)):\beta_2=0
# M2: \doubleE Daysabs_i = \beta_1 + \beta_2*male_i + \beta_3*math_i                          H_0^((2)):\beta_3=0
# M3: \doubleE Daysabs_i = \beta_1 + \beta_2*male_i + \beta_3*math_i + \beta_4*male_i*math_i  H_0^((3)):\beta_4=0
#     =\beta_1 + \beta_3*math_i,  male_i==0
#     =\beta_1 + beta_2 + (\beta_3 + \beta_4)*math_i,  male_i==1

schule.data=read.csv("schule.csv")

m0=glm(daysabs~1, schule.data, family = poisson(link = "identity"))
m1=glm(daysabs~1+male, schule.data, family = poisson(link = "identity"))
m2=glm(daysabs~1+male+math, schule.data, family = poisson(link = "identity"), start=c(6.7,-1.8,0))  #Startwerte sind Parameter aus vorherigen Modell
m3=glm(daysabs~1+male+math+male:math, schule.data, family = poisson(link = "identity"), start=c(9.5,-2.2,-0.05,0))


plot(daysabs~math, schule.data)

summary(m1)
summary(m2)
summary(m3)
summary(m4)

plot(daysabs~math, schule.data, pch=16, col=male+1)

# Simulation von Pseudobeobachtungen

ysim= rpois(316, lambda = predict(m3, type="response"))

points(schule.data$math, ysim, pch=16, col=schule.data$male+3)

# zeigt, dass das Poisson-Modell nicht für die Fragestellung geeignet ist, obwohl die statistischen Tests positive Ergebnisse liefern
# Problem liegt in der Varianzvariablilität
# negative Binomialverteilung: Varianz ist höher als der Erwartungswert

# Daysasb_i ~ Neg. Bionomialverteilung
# ist r fixiert, stellt dies eine Dispersionsfamilie dar, ansonsten nicht
# deswegen nicht standardmäßig im glm

require(MASS)

m0.nb=glm.nb(daysabs~1,data=schule.data, link=("identity"))
m1.nb=glm.nb(daysabs~1+male,data=schule.data, link=("identity"))
m2.nb=glm.nb(daysabs~1+male+math,data=schule.data, link=("identity"),start=c(6.7,-1.8,0))
m3.nb=glm.nb(daysabs~1+male+math+male:math,data=schule.data, link=("identity"),start=c(9.3,-2.3,-0.04,0))

summary(m1.nb)
summary(m2.nb)
summary(m3.nb)    #Überschreitungswahrscheinlichkeit ist größer 0.05 -> keine signifikante Interaktion

ysim2=rnegbin(316,predict(m2.nb, type="response"),theta = 0.755)     #theta entspricht r in Formel

plot(daysabs~math, schule.data, pch=16, col=male+1)
points(schule.data$math, ysim2, pch=16, col=schule.data$male+3)

# Pseudozufallszahlen helfen gut bei der Bewertung der Modelle

AIC(m0, m1, m2, m3)     #bekommen Ranking der Modelle
summary(m3) #auch möglich über Deviance (in Summary); Residual-Deviance entspricht Deviance aus der Vorlesung
summary(m0) #Faustregel: Modell ist gut, wenn Deviance so groß wie n-Parameter; auch in Summary angegeben
            #für m3: "Residual deviance: 2269.7  on 312  degrees of freedom" -> kein gutes Modell

#für negat. binom. Analyse
AIC(m0.nb, m1.nb, m2.nb, m3.nb)   #AIC: m3 ist das beste Modell, Unterschied zu m2 ist aber gering; absolute Werte von AIC sind nicht interpretierbar
summary(m3.nb)
