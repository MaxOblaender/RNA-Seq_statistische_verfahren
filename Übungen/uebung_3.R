miete.data=read.csv("mietspiegel99.csv")
head(miete.data)
#orts�bliche Vergleichsmiete: \E Miete_i=\beta_1+\beta_2*fl�che_i  -> lineares Modell, deterministischer Teil
#Miete_i~N(\mu_i,\sigma^2) -> stochastischer Teil
#hat(\beta)=(X^T X)^-1*y
#X=(einsvektor,fl�che)

X=cbind(1,miete.data$flaeche)
y=miete.data$miete

beta.hat=solve(t(X)%*%X)%*%t(X)%*%y   #\beta_2: Anstieg, bedeutet Anteil, um den der Mietpreis pro Quadratmeter steigt
                             #beta_1: kleinster Preis
#l�sen linearer Modelle in R: lm(formula,data)
#formula: Modell wird beschrieben durch geeigneten Syntax
#formula: zielgr��e ~ 1 + fl�che (d.h. \beta_1+beta_2*x)-> beschreibt deterministischen 
#Anmerkung: 1+ ist standardm��ig, kann weggelassen werden
#data: Datensatz

miete.lm1=lm(miete~1+flaeche, data=miete.data)
lm(miete~flaeche, data=miete.data)


beta.hat[1]+beta.hat[2]*100

cbind(miete.data$flaeche, predict(miete.lm1))[1:5,]       #gesch�tzte Miete je nach Fl�che der Daten

predict(miete.lm1, newdata = data.frame(flaeche=100))    #in neuen Datensatz muss Spalte gleich hei�en
predict(miete.lm1, newdata = data.frame(flaeche=seq(50,100,10))) #Erwartete Mietpreise f�r 50, 60 etc. qm

plot(miete.data$flaeche, miete.data$miete)
abline(beta.hat, col=2, lwd=2)

#bessere Modellierung: �nderung des Stochastischen Teils -> vorher Annahme, dass Variabilit�t konstant ist
#aber: Variabilit�t nimmt zu

#diagnostische Plots: Residuenplot
#sind die Abweichungen der Beobachteten Werten zu den vorhergesagten Werten
#Y_i-hat(Y_i)=hat(\varepsilon_i)

y.hat=predict(miete.lm1)

resi=y-y.hat

#y-Achse: Residuem
#x-Achse: fl�che, miete, y.hat? miete ist ungeeignet, um Annahmen zu machen
#y.hat ist nur eine Reskalierung der fl�che
par(mfrow=c(1,3))
plot(miete.data$flaeche, resi)
plot(miete.data$miete, resi)
plot(y.hat, resi)

par(mfrow=c(1,1))
plot(miete.lm1, which=1) #Standardm��ig Analyseplot
#scheinbar ungeeignete Modellannahme

#anderes Modell:
#M�glichkeit 1: Y_i~N(\mu_i,\sigma_i^2)
#M�glichkeit 2: \E Miete_i=\beta_1+\beta_2*fl�che_i+\beta_3*lage -> Anstieg ist durch Lage noch zu erkl�ren

miete.lm2=lm(miete~1+flaeche+lage, miete.data)
plot(miete~1+flaeche, miete.data)        #Anmerkung: wenn dieser Syntax, dann erst Zielgr��e, dann Einflussgr��e
points(miete.data$flaeche,predict(miete.lm2), col=miete.data$lage+1,pch=16) #Linien parallel, weil Lage von 1-3 codiert ist (haben \beta_3*1)

#Qualitative Einflussgr��en in Statistik faktoren
miete.lm3=lm(miete~1+flaeche+as.factor(lage),miete.data)
plot(miete~1+flaeche, miete.data)
points(miete.data$flaeche,predict(miete.lm3), col=miete.data$lage+1,pch=16)

#Dummy-Variablen:
#x_(i3)={1, lage_i=2}{0, sonst}
#x_(i4)={1, lage_i=3}{0, sonst}
#es ergibt sich: \E Miete_i=\beta_1+\beta_2*fl�che_i+\beta_3*x_(i3)+\beta_4*x_(i4)

#Interaktion: Der Effekt einer Einflussgr��e auf die Zufallsgr��e wird durch eine zweite Einflussgr��e modifiziert
#es ergibt sich: \E Miete_i=\beta_1+\beta_2*fl�che_i+\beta_3*x_(i3)+\beta_4*x_(i4)+\beta_5*x_(i5)+\beta_6*x_(i6)
#x_(i5)=x_(i3)*flaeche_i
#x_(i6)=x_(i4)*flaeche_i
#f�r die Lagen ergibt sich:
#lage1: \beta_1+\beta_2*flaeche_i
#lage2: (\beta_1+\beta_3)+(\beta_2+\beta_5)*flaeche_i
#lage3: (\beta_1+\beta_4)+(\beta_2+\beta_6)*flaeche_i

miete.lm4=lm(miete~1+flaeche+as.factor(lage)+flaeche:as.factor(lage),miete.data)
plot(miete~1+flaeche, miete.data)
points(miete.data$flaeche,predict(miete.lm4), col=miete.data$lage+1,pch=16)


