#Sokahl/Rohlf: Biometry
#   y:  Durchschn. SO_2-Belastung
#   x2: Durchschn. Temp
#   x3: #Unternehmen mit >=20
#   x4: Populationsgröße
#   x5: Durchschn. Wind
#   x6: Durchschn. Niederschlag
#   x7: #Regentage


cities.data=read.csv("cities.csv")
head(cities.data)

#Kandidatenmodelle:
#   M0: y~1     -> keine der Größen hat Einfluss auf das Modell
#   M1: y~1+x4
#   M2: y~1+x3
#   M3: y~1+x3+x4+x5
#   M4: y~1+x4+x2+x2:x4
#   M5: y~1+x4+x5+x4:x5

#Kreuzvalidierung
d=7
set.seed(1)
index=rep(1:d, length.out=41)
index=sample(index)

sep0=sep1=sep2=sep3=sep4=sep5=0


for (j in 1:d){
  # Zerlegung in Test und Trainingsdaten
  cities.test=cities.data[index==j,]        #index==j: Auswahlkriterium für Beobachtungen
  cities.train = cities.data[index!=j,]     #alle anderen Daten
  #Schätzung der Parameter
  m0=lm(y~1,cities.train)
  m1=lm(y~1+x4,cities.train)
  m2=lm(y~1+x3,cities.train)
  m3=lm(y~1+x3+x4+x5,cities.train)
  m4=lm(y~1+x4+x2+x2:x4,cities.train)
  m5=lm(y~1+x4+x5+x4:x5,cities.train)

  #Schätzen des SEP
  sep0=sep0+sum((cities.test$y-predict(m0,newdata=cities.test))^2)
  sep1=sep1+sum((cities.test$y-predict(m1,newdata=cities.test))^2)
  sep2=sep2+sum((cities.test$y-predict(m2,newdata=cities.test))^2)
  sep3=sep3+sum((cities.test$y-predict(m3,newdata=cities.test))^2)
  sep4=sep4+sum((cities.test$y-predict(m4,newdata=cities.test))^2)
  sep5=sep5+sum((cities.test$y-predict(m5,newdata=cities.test))^2)
  
}
sep0
sep1
sep2
sep3
sep4
sep5
