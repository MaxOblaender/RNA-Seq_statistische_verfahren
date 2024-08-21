stem.data=read.csv("stembiomass.csv",sep=";")

# Zerlegung in Trainings- und Testdaten

set.seed(2)
index=sample(1:71,55)
stem.train = stem.data[index,]
stem.test=stem.data[-index,]      #-: Zeilen werden herausgenommen

#Schätzung der Parameter in Trainingsdatensatz
m1=lm(log(stem)~1+log(d),stem.train)
m2=lm(log(stem)~1+log(d)+I(log(d)^2),stem.train)
m3=lm(log(stem)~1+log(d)+I(log(d)^2)+I(log(d)^3),stem.train)

#Schätzen des Prognosefehlers in Testdatensatz
#(SEP)\hat^((M))=\sum_(i\in I_Test)(y_i-y\hat_i^((M))_(i,train))

sep1=sum((log(stem.test$stem)-predict(m1,newdata = stem.test))^2)
sep2=sum((log(stem.test$stem)-predict(m2,newdata = stem.test))^2)
sep3=sum((log(stem.test$stem)-predict(m3,newdata = stem.test))^2)
sep1
sep2
sep3

#Kreuzvalidierung
d=9
index=rep(1:d,length.out=71)      #Vektor, welcher die Zahlen von 1 bis 9 wiederholt, bis er Größe 71 erreicht
index=sample(index)               #zufällige Permutation der Zahlen

sep1=sep2=sep3=0


for (j in 1:d){
  # Zerlegung in Test und Trainingsdaten
  stem.test=stem.data[index==j,]        #index==j: Auswahlkriterium für Beobachtungen
  stem.train = stem.data[index!=j,]     #alle anderen Daten
  #Schätzung der Parameter
  m1=lm(log(stem)~1+log(d),stem.train)
  m2=lm(log(stem)~1+log(d)+I(log(d)^2),stem.train)
  m3=lm(log(stem)~1+log(d)+I(log(d)^2)+I(log(d)^3),stem.train)
  #Schätzen des SEP
  sep1=sep1+sum((log(stem.test$stem)-predict(m1,newdata = stem.test))^2)
  sep2=sep2+sum((log(stem.test$stem)-predict(m2,newdata = stem.test))^2)
  sep3=sep3+sum((log(stem.test$stem)-predict(m3,newdata = stem.test))^2)
  
}
sep1
sep2
sep3

# basierend auf RSS (ganzer Datensatz)
m1=lm(log(stem)~1+log(d),stem.train)
m2=lm(log(stem)~1+log(d)+I(log(d)^2),stem.train)
m3=lm(log(stem)~1+log(d)+I(log(d)^2)+I(log(d)^3),stem.train)


RSS1=sum(residuals(m1)^2)
RSS2=sum(residuals(m2)^2)
RSS3=sum(residuals(m3)^2)
RSS1
RSS2
RSS3

#Schätzung für \sigma\tilde^2
#\sigma\tilde^2=1/(n-k)*RSS

sigma2.full=RSS3/(71-4)

sep1=RSS1+2*sigma2.full*2
sep2=RSS2+2*sigma2.full*3
sep1=RSS3+2*sigma2.full*4

sep1
sep2
sep3

AIC(m1, m2, m3)
summary(m2)     #Multiple R-squared: Fehler des Modells (Achtung: nicht adjusted R-squared benutzen!!)
