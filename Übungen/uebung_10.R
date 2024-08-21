#Bisherige Modellierung für das Problem (Galapagos)
#Erwartungswert: \doubleE Y_i = exp{\beta_1 + \beta_2 \cdot x_i}
#                log{\doubleE Y_i} = \beta_1 + \beta_2 \cdot x_i

#glm predict Ergebnis muss skaliert werden auf die Daten -> Anwendung der Response-Funktion, welche verwendet wurde
#Alternative (in R): predict(... , type="response")
#vcov() : Covarianz (Inverse aus Fischer-Information) aus glm Objekt

#für Aufgabe bessere (?) Funktion zur Modellierung: quadratische Funktion
#   \doubleE Y_i = (\beta_1 + \beta_2 \cdot x_i)^2
#   \sqrt(\doubleE Y_i) = \beta_1 + \beta_2 \cdot x_i -> Wurzelfunktion als Link-Funktion
#
#Problem: nichtkanonische Link-Funktion kann dazu führen, dass nach Initialwerten gefragt wird
#weiteres Problem: für viele Werte ist diese Loglikelihood-Funktion nicht definiert -> Iterationsverfahren stoppt am Rand des möglichen Bereiches
#Ergebnis: Exponentialfunktion stellt Sachverhalt besser dar als Wurzel-Linkfunktion

#Haselhuhn
#Aufgabe 1)
#   gwald: viele Daten bei geringer Entfernung, nur wenige bei weiter Entfernung -> Transformation für glm nötig
#   Transformation: log
#   Überprüfung der Transformierng: über tapply in Teilabschnitten Vorkommen anschauen
# Anmerkung: keine Angabe der link Funktion bei "family= <> (link="...") nutzt automatisch logit
#            cloglog: wird genutzt, wenn Verteilung nicht "gleichmäßig" ist

#Aufgabe 2)
#   für offen: Histogramm zeigt wieder viele Werte bei geriner m-Anzahl -> logarithmische Transformation
#   Link-Funktion: cloglog (wegen Beobachtungen aus erster Aufgabe)


#Neues Beispiel: Ödlandschrecke

offen.data=read.csv("offenboden.csv")
head(offen.data)

#Modell
#   \doubleE vorkommen_i = \mu_i = h(\beta_1 + \beta_2 \cdot offen_i)

hist(offen.data$offen)  #keine Transformation nötig, relativ balancierte Daten
m1 = glm(vorkommen ~ 1+offen, data = offen.data, family=binomial(link="logit"))
m1

plot(vorkommen~offen,offen.data)
points(offen.data$offen, predict(m1,type="response"), pch=16, col=2)
abline(h=0,lty=2)
abline(h=1,lty=2)

#Teilintervalle zur Überprüfung der Modellierung

offen.int = cut(offen.data$offen, breaks=seq(0,35,7))
rel.hfk = tapply(offen.data$vorkommen, offen.int, mean)
rel.hfk
points(seq(3.5, 31.5, 7), rel.hfk, pch=16, col=4)

#Modell, welches Extrema abbilden kann: quadratisch
#   \doubleE vorkommen_i = \mu_i = h(\beta_1 + \beta_2 \cdot offen_i + \beta_3 \cdot offen_i^2)

m2=glm(vorkommen ~ 1+offen+I(offen^2), data = offen.data, family=binomial(link="logit"))
m2
points(offen.data$offen, predict(m2,type="response"), pch=16, col=3)

#Vorhersagen über das Modell heraus
x=seq(0,70,0.1)
plot(vorkommen~offen,offen.data, xlim=c(0,70))
points(x, predict(m1,type="response",newdata = data.frame(offen=x)), pch=16, col=2, type="l")
points(x, predict(m2,type="response",newdata = data.frame(offen=x)), pch=16, col=3, type="l")

#Modellwahl: Überprüfen der Hypothesen
#   H_0: beta_3 = 0
#   H_1: beta_3 \ne 0
#   Teststatistik: T(\underbar Y) = (\beta\hat_j)/(\sqrt(I^-1 (\underbar\beta\hat))_jj)

coef(m2)[3]
vcov(m2)[3,3]

test.stat = coef(m2)[3]/sqrt(vcov(m2)[3,3])
test.stat

#kritischer Wert
alpha=0.05
qnorm(1-alpha/2)  #Wert der Teststatistik ist kleiner -> kein Grund für Ablehnung Hypothese

#Überschreitungswahrscheinlichkeit
pnorm(test.stat)*2    #Wert ist größer -> kein Grund für Ablehnung der 0-Hypothese

# bedeutet: Modell ohne quadratischen Term ist das bessere Modell

#in R:
summary(m2)
#   Pr(|z|): Überschreitungswahrscheinlichkeit (hier z-Test, weil verallgemeinertes lineares Modell)
