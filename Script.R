
library(dplyr)
library(gtsummary)

##  ---  Description des données  ---  ##  
data <- read.delim('Data/projet3.txt')
str(data)
colSums(is.na(data))

# Transformation des premières variables en facteurs
data$dc <- as.factor(data$dc)
data$sexe <- as.factor(data$sexe)
data$tabac <- as.factor(data$tabac)
data$hta <- as.factor(data$hta)
data$diabete <- as.factor(data$diabete)
data$anemie <- as.factor(data$anemie)

# Transformation de creat et fraction en variable binaire (nécessaire ?)
data$insufisanceR <- as.factor(ifelse(data$creat > 1.5, 1, 0))
data$fractionF <- as.factor(case_when(data$fraction <= 30 ~ 0,
                                     data$fraction >= 45 ~ 2, 
                                     TRUE ~ 1)) 

# Centrage de l'âge 
data$AgeC <- data$Age - median(data$Age)

str(data)
summary(data)
data %>% 
  tbl_summary(by = dc, 
              statistic = all_continuous() ~ '{mean} ({sd})',
              digits = all_continuous() ~ c(1,1), 
              missing = 'no')
# Les valeurs semblent toutes cohérentes


##  ---  Kaplan-Meier  ---  ##
library(survival)
library(survminer)
km <- survfit(Surv(temps, dc == 1) ~ 1, data = data, type = "kaplan-meier")
ggsurvplot(km, 
           data = data,
           conf.int = TRUE, 
           conf.int.fill = "pink",
           palette = "red", 
           ggtheme = theme_minimal(), 
           title = "Courbe de survie de Kaplan-Meier", 
           xlab = "Temps", 
           ylab = "Probabilité de survie", 
           # risk.table = TRUE, 
           censor = FALSE,
           # censor.shape = 3, 
           # censor.size = 4, 
           font.main = c(14, "bold"), 
           font.x = c(12), 
           font.y = c(12), 
           ylim = c(0.45,1))
# Stratification 
km2 <- survfit(Surv(temps, dc == 1) ~ sexe, data = data, type = "kaplan-meier")
ggsurvplot(km2, 
           data = data,
           conf.int = TRUE, 
           palette = c("blue", 'red'), 
           ggtheme = theme_minimal(), 
           title = "Courbe de survie de Kaplan-Meier", 
           xlab = "Temps", 
           ylab = "Probabilité de survie", 
           legend.labs = c("Homme", "Femme"),
           # risk.table = TRUE, 
           censor = FALSE,
           # censor.shape = 3, 
           # censor.size = 4, 
           font.main = c(14, "bold"), 
           font.x = c(12), 
           font.y = c(12), 
           ylim = c(0.45,1))


## --  Modèle de Cox -- ##
events <- data$temps[data$dc == 1]
table(events) # Pas beaucoup d'ex-aequo => Breslow suffisant
datam <- data %>% select(-ID)

# 1er modèle complet
fit <- coxph(Surv(temps, dc == 1) ~ ., data = datam, ties = 'breslow') 
summary(fit) 
# => Fraction en facteur n'est pas du tout significatif, contrairement à l'article.
# => creat et creatF sont à la limite. 

# Hypothèses de proportionnalité 
cox.zph(fit, transform="identity") 
# => Pb avec fractionF et creatF, on choisi simplement de conserver les variables en quantitatif ?
cox.zph(fit, transform="log")
# => Toujours limite avec fractionF et creatF, mais moins marqué

# KM 
km3 <- survfit(Surv(temps, dc == 1) ~ creatF, data = datam, type = "kaplan-meier")
ggsurvplot(km3, 
           data = datam,
           conf.int = TRUE, 
           palette = c("blue", 'red'), 
           ggtheme = theme_minimal(), 
           title = "Courbe de survie de Kaplan-Meier", 
           xlab = "Temps", 
           ylab = "Probabilité de survie", 
           legend.labs = c("<= 1.5", "> 1.5"),
           # risk.table = TRUE, 
           censor = FALSE,
           # censor.shape = 3, 
           # censor.size = 4, 
           font.main = c(14, "bold"), 
           font.x = c(12), 
           font.y = c(12), 
           ylim = c(0.45,1))
km4 <- survfit(Surv(temps, dc == 1) ~ fractionF, data = datam, type = "kaplan-meier")
ggsurvplot(km4, 
           data = datam,
           conf.int = TRUE, 
           palette = c("blue", 'red', 'green'), 
           ggtheme = theme_minimal(), 
           title = "Courbe de survie de Kaplan-Meier", 
           xlab = "Temps", 
           ylab = "Probabilité de survie", 
           legend.labs = c("<= 30", "> 30 & < 45", ">= 45"),
           # risk.table = TRUE, 
           censor = FALSE,
           # censor.shape = 3, 
           # censor.size = 4, 
           font.main = c(14, "bold"), 
           font.x = c(12), 
           font.y = c(12), 
           ylim = c(0.45,1))

# 2ème modèle 
fit2 <- coxph(Surv(temps, dc == 1) ~ Age + sexe + hta + anemie + fraction + creat + creatk, data = datam, ties = 'breslow') 
summary(fit2) # Il doit avoir des intéractions entre plusieurs variables, dur à dire comme ça 


