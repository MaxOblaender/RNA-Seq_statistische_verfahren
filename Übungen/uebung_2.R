beh.data=read.csv("Behandlungszeiten.csv", sep=";")
hist(beh.data$Gesamt)

#Y_i: zufällige Behandlungszeit von Patient i
#Y_i~Exp(\theta) unabhängig
#ML-Schätzung des Parameter \theta
#Schritte:
# 1. Likelihood-Funktion -> L(\theta)=\prod_(i=1)^n(\theta*exp{-\theta*Y_i})
# 2. Loglikelihood-Funktion -> l(\theta)=\sum_(i=1)^n([log(\theta)-\theta*Y_i])=n*log(\theta)-\theta*\sum_(i=1)^n(Y_i)
# 3. Score-Funktion
# 4. Score-Gleichung

#---------------------------------
#Schritt 1:
y=beh.data$Gesamt
likelihood = function(theta){
  prod(theta*exp(-theta*y))
}

likelihood(0.01)
likelihood(0.02)
likelihood(0.03)

theta.seq=seq(0,0.1,0.001)
likelihood(theta.seq)

#Vektorisierung der Funktion
likelihood.vec = Vectorize(likelihood, vectorize.args="theta")

likelihood.vec(theta.seq)
plot(theta.seq, likelihood.vec(theta.seq), type="l")


#---------------------------------
#Schritt 2:
loglikelihood =function(theta){
  sum(log(theta)-theta*y)
}
loglikelihood.vec=Vectorize(loglikelihood, vectorize.args = "theta")

loglikelihood.vec(theta.seq)
plot(theta.seq, loglikelihood.vec(theta.seq), type="l")

#---------------------------------
#Schritt 3:
score = function(theta){
  n=length(y)
  n/theta-sum(y)
}
score.vec=Vectorize(score, vectorize.args = "theta")

plot(theta.seq, score.vec(theta.seq), type="l", xlim=c(0.01,0.02), ylim=c(-10,1000))

#---------------------------------
#Schritt 4:
abline(h=0, col=2, lty=2)

theta.hat = length(y)/sum(y)
theta.hat
abline(v=theta.hat, col=3, lty=2)

#Exponentialverteilung
x=seq(0,300)
plot(x, dexp(x, rate=theta.hat),type="l", col=2, lwd=2)


hist(y, freq = F)
points(x, dexp(x, rate=theta.hat),type="l", col=2, lwd=2) #dexp, pexp: Funktionen der Exponentialfnkt.


plot(ecdf(y))
points(x, pexp(x, rate=theta.hat),type="l", col=2, lwd=2)
hist(y, freq = F, breaks=seq(0,300,25))

#Gammaverteilung
#f(y)=(1/S^a\Gamma(a))Y^(a-1)exp{-Y/S}

plot(x,dgamma(x,shape=2, scale=50),type="l")
