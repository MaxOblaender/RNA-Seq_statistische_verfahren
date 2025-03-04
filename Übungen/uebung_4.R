# model.matrix(<lineares Modell>) -> gibt Design-Matrix f�r das Modell aus
#
# diagnostische Plots:
# Q-Q (quantile-quantile): Quantile der Residuen mit Quantilen der Werte verglichen
#    Normalverteilung der Residuen wird verglichen
# Scale-Location: Wurzel der standardisierten Residuen gegen gefittete Variablen
#    Linie sollte parallel zur x-Achse sein
#
# Marginalit�tsprinzip: wenn Terme h�herer Ordnung in einem Modell enthalten sind, 
#    sollten auch alle Terme niederer Ordnung enthalten sein (Potenzen oder Interaktionen)
# Beispiel: f�gt man nur die Einflussgr��e quadratisch hinzu, so muss man das Modell entsprechend ver�ndern,
#    sodass der Scheitelpunkt nicht automatisch bei 0 ist.
#    \beta_1+\beta_2*bjahr^2 ist falsch -> \beta_1+\beta_2*bjahr+\beta_3*bjahr
#
# Bemerkung asfactor(): binarisiert Einflussgr��e, muss bei Kategorie mit nur {0,1} nicht verwendet werden


#Pr�zenz-�bung 4:

stem.data = read.csv("stembiomass.csv", sep=";")
head(stem.data)

plot(stem~d, stem.data)

# vorgeschlagenes Modell: stem_i ~ \beta_1 * d^\beta_2 -> allometrisches Modell
# log(stem_i) \approx log(\beta_1)+ \beta_2 * log(d_i)
# \doubleE log(stem_i) = \beta_1^* + \beta_2 * log(d_i)

plot(log(stem)~log(d),stem.data)

m1 = lm(log(stem)~1+log(d),stem.data)
m1
alpha = 0.05
n = 71
k = 2

t.quantil = qt(1-alpha/2,n-k) #Quantilfunktion der t-Funktion, Faustregel: ist ungef�hr 2

# ---Konfidenzintervalle---
# f�r \beta_j: \beta\hat_j +- t_(n-k,1-\alpha/2)\sqrt(\sigma\tilde^2 * (\doubleX^T*\doubleX)^-1_jj)
# \alpha = 0.05
# n = 71 (Stichprobenumfang)
# k = 2 (Anzahl der Parameter im Modell)

X = model.matrix(m1)
head(X)

# \sigma\tilde^2 = 1/(n-k) \sum_(i=1)^n(Y_i-Y\hat_i)^2 -> Sch�tzung f�r die Varianz
sigma2.tilde = sum(residuals(m1)^2/(n-k))

coef(m1)[2]
coef(m1)[2]-t.quantil*sqrt(sigma2.tilde*solve(t(X)%*%X)[2,2]) #obere Grenze der Konfidenzintervalle
coef(m1)[2]+t.quantil*sqrt(sigma2.tilde*solve(t(X)%*%X)[2,2]) #untere Grenze

confint(m1) #R-Funktion in R f�r die Grenzen der Konfidenzintervalle
confint(m1, level = 0.99) #auf 99% Konfidenzintervall, standardm��ig sonst auf 95%

# f�r \doubleE Y_neu: Y\hat_neu +- t__(n-k,1-\alpha/2) \sqrt()
predict(m1)
pred90=predict(m1, interval="confidence", level=0.9) #erwartete Grenzen f�r erwartete Biomasse f�r 90%
pred95=predict(m1, interval="confidence")
head(pred95)

plot(log(stem)~log(d),stem.data)
points(log(stem.data$d),pred95[,2],pch="+", col=2) #untere Grenze = pred95[]
points(log(stem.data$d),pred95[,1],pch="+", col=1) #erwarteter Wert
points(log(stem.data$d),pred95[,3],pch="+", col=3) #obere Grenze

# viele Werte liegen au�erhalb -> brauchen Prognoseintervall f�r neue Beobachtungen

prognose90 = predict(m1, interval="prediction", level=0.9)
prognose95=predict(m1, interval="prediction")

plot(log(stem)~log(d),stem.data)
points(log(stem.data$d),prognose95[,2],pch="+", col=2) #untere Grenze
points(log(stem.data$d),prognose95[,1],pch="+", col=1) #erwarteter Wert
points(log(stem.data$d),prognose95[,3],pch="+", col=3) #obere Grenze

prognose90 = predict(m1, interval="prediction", level=0.9, newdata = data.frame(d=exp(seq(1,4,0.01)))) # brauchen gleichen Df namen
prognose95=predict(m1, interval="prediction",newdata = data.frame(d=exp(seq(1,4,0.01))))

plot(log(stem)~log(d),stem.data)
points(seq(1,4,0.01),prognose95[,2],pch="+", col=2) #untere Grenze
points(seq(1,4,0.01),prognose95[,1],pch="+", col=1) #erwarteter Wert
points(seq(1,4,0.01),prognose95[,3],pch="+", col=3) #obere Grenze
