seeds.data=read.csv("seeds.csv")
head(seeds.data)

# Zielgröße: seeds
# Einflussgröße: treatment
#                 "control", "caterpillar", "scissors"

levels(seeds.data$treatment)
m1 = glm(seeds ~ 1 + as.factor(treatment), seeds.data, family=poisson(link="identity"))
m1    #haben im Modell nur "scissors" und "control" als as.factor, "caterpillar" wird als Referenz genutzt

seeds.data$treatment= relevel(factor(seeds.data$treatment),ref="control")  #control wird als Referenz gesetzt
m0 = glm(seeds ~ 1, seeds.data, family=poisson(link="identity"))
m1 = glm(seeds ~ 1 + as.factor(treatment), seeds.data, family=poisson(link="identity"))
#Frage: hat treatment einen Effekt? -> Hypothese, welche 2 Parameter betrifft, H_0: \beta_2 = \beta_3 = 0
# ... = \beta_1, caterpillar
# ... = \beta_1 + beta_2, control
# ... = \beta_1 + beta_3, scissors


#Likelihood-Quotienten-Test
test.stat=2*(logLik(m1)-logLik(m0))

pchisq(test.stat, lower.tail = F, df =2)  #df: Anzahl der Parameter, auf die sich die Teststatistik stützt
# Nullhypothese wird abgelehnt, treatment hat Einfluss auf Samenproduktion

anova(m0, m1, test="LRT") #für Likelihood-Quotienten-Test -> test="LRT"

# Betrachtung der Stress-Level
levels(seeds.data$stress)

m2 = glm(seeds ~ 1 + as.factor(treatment) + stress, seeds.data, family=poisson(link="identity"))
anova(m0, m1,m2, test="LRT")   #zeigt, dass der Stress-Parameter nicht signifikant ist
# Aber: haben überhaupt nicht die richtige Frage damit untersucht -> wollen Interaktion wissen

m3 = glm(seeds ~ 1 + as.factor(treatment) + stress+as.factor(treatment):stress, seeds.data, family=poisson(link="identity"))
anova(m0, m1,m2,m3, test="LRT")

interaction.plot(x.factor = seeds.data$treatment, trace.factor = seeds.data$stress, response = seeds.data$seeds)
