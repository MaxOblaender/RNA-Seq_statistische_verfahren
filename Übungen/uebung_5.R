#Modellauswahl aus �bungsserie:
#haben Modellhierarchie
#M0: log(stem)~1                            
#M1: log(stem)~1+log(d)                     H_0^(1):\beta_2=0
#M2: log(stem)~1+log(d)+log(d)^2            H_0^(2):\beta_3=0
#M3: log(stem)~1+log(d)+log(d)^2++log(d)^3  H_0^(3):\beta_4=0

#t-Test: Teststatistik: T(Y)=(\beta\hat_i)/(\sqrt(\sigma\tilde^2(X^TX)^-1_jj))
#Cov(\beta\hat)=\sigma^2(X^TX)^-1
#vcov(m1)=\sigma\tilde^2(X^TX)^-1
#Wenn H_0 richtig ist, dann gilt T(Y)~t_(n-k)
# -> Faustregel Quantile bei 2

#Umsetzung in R f�r Hypothesentests:summary()
#   f�r lm-Funktion
#   fasst alle Werte tabellarisch zusammen
#   t value: bedeutet Wert f�r Teststatistik
#   letzte Spalte (Pr>|t|): �berschreitungswahrscheinlichkeit f�r Hypothesetests (�bliche Grenze: 0.05, alles darunter wird abgelehnt)
#   Bemerkung: alle Modelle m�ssen einzeln berechnet und betrachtet werden. 
#   Im Beispiel: es w�rde jeweils nichts dagegen sprechen, dass alle Hypothesen angenommen werden, d.h. alle \beta=0

#Varianzinhomogenes System: in Maximum Likelihood-sch�tzung statt \sigma mit \sigma_i rechnen
#erhalten \Sigma=diag(\sigma^2_1,\sigma^2_2,...,\sigma^2_n)
#aber: k�nnen Varianz nicht aus einem Wert sch�tzen
#L�sung: Teilintervalle, in denen gesch�tzt wird

#alternative Variante: nehmen an, dass Sigma sich proportional ver�ndert
#\sigma^2_i=\gamma*age^2_i

irr.data = read.csv("irrigation.csv",sep=";")
str(irr.data)
table(irr.data$sorte, irr.data$beregnung)
plot(y~beregnung, irr.data)
