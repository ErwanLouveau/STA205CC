

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

# Modèle univarié
