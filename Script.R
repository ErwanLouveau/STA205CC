library(survival)
library(dplyr)

source("C:/Users/erwan/Desktop/M2 Biostat/STA205/Projet CC/ProjetCC/setup.R")

str(data)
colSums(is.na(data))

# Transformation des premières variables en facteurs
data$dc <- as.factor(data$dc)
data$sexe <- as.factor(data$sexe)
data$tabac <- as.factor(data$tabac)
data$hta <- as.factor(data$hta)
data$diabete <- as.factor(data$diabete)
data$anemie <- as.factor(data$anemie)

# Transformation de creat et fraction en variable binaire 
data$insufisanceR <- as.factor(ifelse(data$creat > 1.5, 1, 0))
data$fractionF <- as.factor(case_when(data$fraction <= 30 ~ 0,
                                      data$fraction >= 45 ~ 2, 
                                      TRUE ~ 1)) 

# data$fractionF <- as.factor(case_when(data$fraction <= 30 ~ 1,
#                                       data$fraction >= 45 ~ 2, 
#                                       TRUE ~ 0)) 

# Centrage de l'âge 
data$AgeC <- data$Age - median(data$Age)

str(data)
summary(data)

# Modèle univarié
fit_fraction <- coxph(Surv(temps, dc == 1) ~ fractionF, data, ties = "breslow")
summary(fit_fraction) #significatif
fit_sexe <- coxph(Surv(temps, dc == 1) ~ sexe, data, ties = "breslow")
summary(fit_sexe)
fit_age <- coxph(Surv(temps, dc == 1) ~ AgeC, data, ties = "breslow")
summary(fit_age) #significatif
fit_tabac <- coxph(Surv(temps, dc == 1) ~ tabac, data, ties = "breslow")
summary(fit_tabac)
fit_hta <- coxph(Surv(temps, dc == 1) ~ hta, data, ties = "breslow")
summary(fit_hta) #significatif
fit_diabete <- coxph(Surv(temps, dc == 1) ~ diabete, data, ties = "breslow")
summary(fit_diabete)
fit_sodium <- coxph(Surv(temps, dc == 1) ~ Sodium, data, ties = "breslow")
summary(fit_sodium) #significatif
fit_anemie <- coxph(Surv(temps, dc == 1) ~ anemie, data, ties = "breslow")
summary(fit_anemie)
fit_creat <- coxph(Surv(temps, dc == 1) ~ insufisanceR, data, ties = "breslow")
summary(fit_creat) #significatif
fit_creatK <- coxph(Surv(temps, dc == 1) ~ creatk, data, ties = "breslow")
summary(fit_creatK)

# Interactions
fit <- coxph(Surv(temps, dc == 1) ~ fractionF + sexe + fractionF*sexe, data, ties = "breslow")
summary(fit)
fit <- coxph(Surv(temps, dc == 1) ~ fractionF + AgeC + fractionF*AgeC, data, ties = "breslow")
summary(fit)
fit <- coxph(Surv(temps, dc == 1) ~ fractionF + tabac + fractionF*tabac, data, ties = "breslow")
summary(fit)
fit <- coxph(Surv(temps, dc == 1) ~ fractionF + hta + fractionF*hta, data, ties = "breslow")
summary(fit)
fit <- coxph(Surv(temps, dc == 1) ~ fractionF + diabete + fractionF*diabete, data, ties = "breslow")
summary(fit) # Significatif
fit <- coxph(Surv(temps, dc == 1) ~ fractionF + Sodium + fractionF*Sodium, data, ties = "breslow")
summary(fit)
fit <- coxph(Surv(temps, dc == 1) ~ fractionF + anemie + fractionF*anemie, data, ties = "breslow")
summary(fit)
fit <- coxph(Surv(temps, dc == 1) ~ fractionF + insufisanceR + fractionF*insufisanceR, data, ties = "breslow")
summary(fit) # Significatif
fit <- coxph(Surv(temps, dc == 1) ~ fractionF + creatk + fractionF*creatk, data, ties = "breslow")
summary(fit)


### Modèle 1 multivarié classique
  # modèle simple avec fractionF (fraction d'éjection) en exposition principale
fit1 <- coxph(Surv(temps, dc == 1) ~ fractionF + sexe + AgeC + tabac + hta + diabete + 
                Sodium + anemie + insufisanceR + creatk, 
              data = data, ties = 'breslow')
  # même modèle simple avec fractionF (fraction d'éjection) en exposition principale mais avec
  # creatk mis au log. Raison : L'unité de mesure de la créatine kénase est d'un ordre bien plus
  # important que les autres variables
fit1 <- coxph(Surv(temps, dc == 1) ~ fractionF + sexe + AgeC + tabac + hta + diabete + 
              Sodium + anemie + insufisanceR + log(creatk), 
              data = data, ties = 'breslow')

summary(fit1)
prop1 <- cox.zph(fit1,transform="identity")
print(prop1)
  # la proportionnalité des risques n'est pas respectée globalement pour la variable fraction d'éjection
  # ni pour l'insufisance rénale. Le modèle global comporte un problème de proportionnalité des riques



### Modèle 2 multivarié avec stratification sur l'insufisanceR
fit2 <- coxph(Surv(temps, dc == 1) ~ fractionF + sexe + AgeC + tabac + hta + diabete + 
                Sodium + anemie + creatk + strata(insufisanceR), 
              data = data, ties = 'breslow')
summary(fit2)
prop2 <- cox.zph(fit2,transform="identity")
print(prop2)

  # En stratifiant par l'insufisance rénale, on supprime l'effet de la variable sur les autres.
  # Les résidus de schoenfeld montre un respect de la proportionalité des risques pour la gloalité du modèle
  # La fraction d'éjection à un p-value < à 0.05 (mais proche), cependant cela ne semble pas très impactant
  # sur l'estimation.
  # Attention à la créatine kénase dont la p-value diminue proche de 0.05

KM1 <- survfit(Surv(temps, dc == 1) ~ fractionF, data=data,
               type = "kaplan-meier")
plot(KM1)
plot(KM1, conf.int=TRUE, col=c("grey","blue"), lty=1:2 , mark.time=F, xlab="Jours", ylab="Survie")
plot(KM1, fun="cloglog", ylab= "log(-log(S(t)))")



### Modèle 3 multivarié avec stratification sur l'insufisanceR et interaction de la fractionF avec le temps
fit2 <- coxph(Surv(temps, dc == 1) ~ fractionF + sexe + AgeC + tabac + hta + diabete + 
                Sodium + anemie + creatk + strata(insufisanceR), 
              data = data, ties = 'breslow')
summary(fit2)
prop2 <- cox.zph(fit2,transform="identity")
print(prop2)

# regarder interaction avec le temps. Soit faire une période a partir de quand les residus change ou faire un fois un t
# Question à PJ --> pourquoi mettre au log la variable creatk 
# Si a cause d'un problème d'echelle, pourquoi pas sodium ?
fit3 <- coxph(Surv(log(temps), dc == 1) ~ fractionF + sexe + AgeC + tabac + hta + diabete + 
                Sodium + anemie + insufisanceR + creatk + tt(as.numeric(fractionF)), 
                data = data, ties = 'breslow', tt=function(x, t, ...){x * t})
summary(fit3)
prop3 <- cox.zph(fit3,transform="identity") # IMPOSSIBLE car interaction avec le temps
  # /!\ TROUVER COMMENT VERIFIER L'ADEQUATION AU MODELE QUAND ON A UN TERME D'INTERACTION AVEC LE TEMPS
print(prop3)



# plot(prop1_1[10])
# 
# fit1_2 <- coxph(Surv(log(temps), dc == 1) ~ fractionF + sexe + AgeC + tabac + hta + diabete + 
#                   Sodium + anemie + insufisanceR + creatk + 
#                   tt(as.numeric(fractionF)) + tt(as.numeric(insufisanceR)), 
#                 data = data, ties = 'breslow', tt=function(x, t, ...){x * log(t)})
# summary(fit1_2)
# prop1_2 <- cox.zph(fit1_2,transform="identity")
# print(prop1_2)
# 
# 
# # Modèle 2 multivarié avec interaction fraction x sexe
# fit2 <- coxph(Surv(temps, dc == 1) ~ fractionF + sexe + fractionF*sexe + AgeC + tabac + 
#                 hta + diabete + Sodium + anemie + insufisanceR + creatk, 
#               data = data, ties = 'breslow') 
# summary(fit2)
# 
# # Modèle 3 stratification par le sexe
# fit3 <- coxph(Surv(temps, dc == 1) ~ fractionF + AgeC + tabac + 
#                 hta + diabete + Sodium + anemie + insufisanceR + creatk + strata(sexe), 
#               data = data, ties = 'breslow')
# summary(fit3)
# prop3 <- cox.zph(fit3,transform="identity")
# print(prop3)
# prop3 <- cox.zph(fit3,transform="log")
# print(prop3)
# 
# # --> Question pour PJ comment choisir log temps et autre transformation log
# 
# # Modèle 4 stratification par l'insufisanceR
# fit4 <- coxph(Surv(temps, dc == 1) ~ fractionF + AgeC + tabac + 
#                 hta + diabete + Sodium + anemie + sexe + creatk + strata(insufisanceR), 
#               data = data, ties = 'breslow')
# summary(fit4)
# prop4 <- cox.zph(fit4,transform="identity")
# print(prop4)
