library(survival)
library(dplyr)
library(gtsummary)

# script_principal.R

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

# Transformation de creat et fraction en variable binaire (nécessaire ?)
data$insufisanceR <- as.factor(ifelse(data$creat > 1.5, 1, 0))
data$fractionF <- as.factor(case_when(data$fraction <= 30 ~ 0,
                                      data$fraction >= 45 ~ 2, 
                                      TRUE ~ 1)) 

# data$fractionF <- as.factor(case_when(data$fraction <= 30 ~ 1,
#                                       data$fraction >= 45 ~ 2, 
#                                       TRUE ~ 0)) 

# Centrage de l'âge 
data$AgeC <- data$Age - median(data$Age)

### TABLEAU 1 - Description en fonction de deces

theme_gtsummary_language("fr", decimal.mark = ",", big.mark = " ")
# ,
# temps
data %>% select(-ID, -AgeC) %>% select(dc, Age, sexe, tabac, hta, diabete,
                fractionF, fraction, insufisanceR, creat, anemie, 
                Sodium, creatk) %>% 
  mutate(dc = if_else(dc==0,"Non","Oui"),
         sexe = if_else(sexe==1,"Hommes","Femmes"),
         tabac = if_else(tabac==0,"Non fumeur","Fumeur"),
         hta = if_else(hta==0,"Non","Oui"),
         diabete = if_else(diabete==0,"Non","Oui"),
         anemie = if_else(anemie==0,"Non","Oui"),
         fractionF = case_when(fractionF==0~"<30",
                               fractionF==1~"[30;45[",
                               fractionF==2~">=45",
                               TRUE ~ NA),
         insufisanceR = if_else(insufisanceR==0,"Non","Oui")) %>% 
  mutate(fractionF = factor(fractionF, levels = c("<30", "[30;45[", ">=45"))) %>% 
  tbl_summary(by = dc,
              type = list(c(Age,fraction,creat,Sodium, creatk) ~ "continuous2"),
              statistic = list(all_continuous2() ~ c("{mean} ({sd})"),
                               all_categorical() ~ c("{n} ({p}%)")),
              # missing = "no",
              digits = list(all_continuous2()~c(1,1),
                            all_categorical()~c(0,1)),
              label = list(Sodium~"Concentration de sodium dans le plasma (en mmol/L)",
                           tabac~"Statut tabagique",
                           hta~"Présence d'hypertension",
                           diabete~"Diabète",
                           fractionF~"Catégorie de Fraction d'éjection (en %)",
                           fraction~"Fraction d'éjection (en %)",
                           insufisanceR~"Insufisance rénale",
                           creat~"Clérence rénale de la créatinine (en mL/s)",
                           anemie~"Présence d'anémie",
                           creatk~"Créatine kinase (en UI/L)")) %>%
                           # temps~"Temps de suivi (en jour)"))
  italicize_labels() %>% 
  add_p(list(all_continuous2()~"t.test",
             all_categorical()~"chisq.test"))
# %>% 
  # as_gt %>% 
  # as_latex()
# 
# %>% 
#   modify_footnote(everything() ~ NA)

data %>% 
  select(-ID, -AgeC) %>% 
  select(dc, Age, sexe, tabac, hta, diabete, fractionF, fraction, insufisanceR, creat, anemie, Sodium, creatk) %>% 
  mutate(
    dc = if_else(dc == 0, "Non", "Oui"),
    sexe = if_else(sexe == 1, "Hommes", "Femmes"),
    tabac = if_else(tabac == 0, "Non fumeur", "Fumeur"),
    hta = if_else(hta == 0, "Non", "Oui"),
    diabete = if_else(diabete == 0, "Non", "Oui"),
    anemie = if_else(anemie == 0, "Non", "Oui"),
    fractionF = case_when(
      fractionF == 0 ~ "<30",
      fractionF == 1 ~ "[30;45[",
      fractionF == 2 ~ ">=45",
      TRUE ~ NA
    ),
    insufisanceR = if_else(insufisanceR == 0, "Non", "Oui")
  ) %>% 
  mutate(fractionF = factor(fractionF, levels = c("<30", "[30;45[", ">=45"))) %>% 
  tbl_strata(
    strata = insufisanceR, # La variable de stratification
    .tbl_fun = ~ .x %>%
      tbl_summary(
        by = dc,
        type = list(c(Age, fraction, creat, Sodium, creatk) ~ "continuous2"),
        statistic = list(
          all_continuous2() ~ c("{mean} ({sd})"),
          all_categorical() ~ c("{n} ({p}%)")
        ),
        digits = list(
          all_continuous2() ~ c(1, 1),
          all_categorical() ~ c(0, 1)
        ),
        label = list(
          Sodium ~ "Concentration de sodium dans le plasma (en mmol/L)",
          tabac ~ "Statut tabagique",
          hta ~ "Présence d'hypertension",
          diabete ~ "Diabète",
          fractionF ~ "Catégorie de Fraction d'éjection (en %)",
          fraction ~ "Fraction d'éjection (en %)",
          creat ~ "Clérance rénale de la créatinine (en mL/s)",
          anemie ~ "Présence d'anémie",
          creatk ~ "Créatine kinase (en UI/L)"
        )
      ) %>% 
      italicize_labels() %>% 
      add_p(list(
        all_continuous2() ~ "t.test",
        all_categorical() ~ "fisher.test"
      ))
  ) %>%
  as_gt %>%
  as_latex()
