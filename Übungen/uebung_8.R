ozone.data=read.csv("ozone.csv")

require(lattice)
xyplot(O3~temp|cut(wind,3),layout=c(3,1),ozone.data, panel=function(x,y){
  panel.xyplot(x,y)
  panel.abline(lm(y~x))
}) #Wind wird in 3 Teilintervalle eingeteilt und gegen die Temperatur betrachtet

m6=lm(O3~(1+temp+I(temp^2))*ibt,ozone.data)     #einfachste Form für Beschreibung der funktionalen Wechselwirkung

plot(O3~doy,ozone.data)
