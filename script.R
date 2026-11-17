install.packages("haven")
install.packages("dplyr")
install.packages("pROC")
install.packages("margins")
install.packages("pROC")
install.packages("survey")
install.packages("ggplot2")
install.packages("broom")
install.packages("DescTools")
install.packages("surveyROC")
library(dplyr)
library(pROC)
library(haven)
library(ResourceSelection)
library(margins)
library(survey)
library(ggplot2)
library(broom)
library(DescTools)




setwd("C:/Users/PERKOUMA/Desktop/Memoire_Malnutrition_Burkina")
getwd()
enfants <- read_dta("01_Donnees_Brutes/BFKR81DT (enfants)/BFKR81FL.DTA")
dim(enfants)

names(enfants)[grep("hw70", names(enfants))]
grep("hw70", names(enfants), value = TRUE)

#### Nettoyage du HAZ
# Créer haz = hw70/100
enfants <- enfants %>%
  mutate(haz = hw70 / 100)

# Remplacer les codes manquants (9998, etc.) par NA
enfants <- enfants %>%
  mutate(haz = ifelse(hw70 >= 9990, NA, haz))

# Exclure les valeurs aberrantes biologiquement impossibles (< -6 ou > 6)
enfants <- enfants %>%
  mutate(haz = ifelse(haz < -6 | haz > 6, NA, haz))

# Vérifier le résumé
summary(enfants$haz)

####### Créer stunting 
enfants <- enfants %>%
  mutate(stunting = ifelse(haz < -2, 1, 0)) %>%
  mutate(stunting = ifelse(is.na(haz), NA, stunting))

# Fréquence absolue et relative
table(enfants$stunting, useNA = "ifany")
prop.table(table(enfants$stunting, useNA = "ifany"))

save(enfants, file = "02_Donnees_Nettoyees/base_enfants_propre.RData")
load("02_Donnees_Nettoyees/base_enfants_propre.RData")

names(enfants)

# Création des variables explicatives
enfants <- enfants %>%
  mutate(
    age_enfant = hw1,                # âge en mois
    sexe = factor(b4, levels = c(1,2), labels = c("Masculin", "Féminin")),
    milieu = factor(v025, levels = c(1,2), labels = c("Urbain", "Rural")),
    instruction = factor(v106, levels = c(0,1,2), 
                         labels = c("Aucune", "Primaire", "Secondaire+")),
    richesse = v190,                 # 1 à 5
    eau = v113,                      # à recoder plus tard
    vaccin_dtc3 = h5                 # à vérifier
  )


head(enfants[, c("age_enfant", "sexe", "milieu", "instruction", "richesse", "eau", "vaccin_dtc3")])

####  Tableaux de fréquences pour les variables qualitatives
table(enfants$sexe, useNA = "ifany")
table(enfants$milieu, useNA = "ifany")
table(enfants$instruction, useNA = "ifany")
table(enfants$richesse, useNA = "ifany")
table(enfants$vaccin_dtc3, useNA = "ifany")

summary(enfants$age_enfant)

#### Recodage de la variable eau en binaire (améliorée / non améliorée)

table(enfants$eau, useNA = "ifany")

######## Création de la variable eau_amelioree dans R
# Sources améliorées : 11(robinet maison), 12(robinet voisin), 
# 13(fontaine publique), 14(puits tubé/forage), 21(puits protégé),
# 31(source protégée), 41(eau de pluie), 51(eau en bouteille)
# Sources NON améliorées : 32(puits non protégé), 43(surface),
# 96(autre), 97(manquant)

enfants <- enfants %>%
  mutate(eau_amelioree = case_when(
    eau %in% c(11, 12, 13, 14, 21, 31, 41, 51) ~ 1,
    eau %in% c(32, 43, 96) ~ 0,
    eau == 97 ~ NA_real_,
    TRUE ~ NA_real_
  ))

table(enfants$eau_amelioree, useNA = "ifany")

#### les autres variables

table(enfants$sexe, useNA = "ifany")
table(enfants$milieu, useNA = "ifany")
table(enfants$instruction, useNA = "ifany")
table(enfants$richesse, useNA = "ifany")
table(enfants$vaccin_dtc3, useNA = "ifany")
summary(enfants$age_enfant)

### Ajout de variables : IMC de la mère,Statut nutritionnel, et  nombre de cpn,
# Taille du ménage, Rang de naissance, Duree allaitement

enfants <- enfants %>%
  mutate(
    # IMC de la mère (v445 divisé par 100)
    imc_mere = ifelse(v445 >= 9990, NA, v445 / 100),
    
    # Statut nutritionnel de la mère
    mere_malnutrie = ifelse(imc_mere < 18.5, 1, 
                     ifelse(is.na(imc_mere), NA, 0)),
    
    # Nombre de CPN (m14)
    cpn_adequat = ifelse(m14 >= 4, 1, 
                  ifelse(m14 == 98 | m14 == 99, NA,
                  ifelse(is.na(m14), NA, 0))),
    
    # Taille du ménage
    taille_menage = v136,
    
    # Rang de naissance
    rang_naissance = bord,
    
    # Allaitement (m4 : durée allaitement)
    allaitement = ifelse(m4 >= 95, NA, m4)
  )

# Vérifier
summary(enfants$imc_mere)
table(enfants$cpn_adequat, useNA = "ifany")
table(enfants$mere_malnutrie, useNA = "ifany")

### Reconstruire d_clean avec toutes les variables

d_clean <- enfants %>%
  filter(!is.na(stunting)) %>%
  mutate(
    stunting      = as.numeric(stunting),
    vaccin_bin    = ifelse(vaccin_dtc3 == 1, 1,
                    ifelse(vaccin_dtc3 == 0, 0, NA)),
    richesse_f    = factor(richesse, levels = 1:5,
                    labels = c("Très pauvre","Pauvre",
                               "Moyen","Riche","Très riche")),
    instruction   = relevel(instruction, ref = "Aucune"),
    milieu        = relevel(milieu,      ref = "Urbain"),
    sexe          = relevel(sexe,        ref = "Masculin")
  )

# Déclarer le design APRÈS reconstruction
design <- svydesign(
  id      = ~v001,
  strata  = ~v022,
  weights = ~I(v005 / 1000000),
  data    = d_clean,
  nest    = TRUE
)

# ================================================
# MODELE FINAL PROPRE — version backward
# ================================================

# On garde : sexe, richesse_f, age_enfant, milieu,
# instruction, mere_malnutrie (justifiées théoriquement)
# On retire : eau_amelioree, taille_menage (non sig + peu théorique)

model_v2 <- svyglm(
  stunting ~ sexe + milieu + instruction + richesse_f +
             age_enfant + mere_malnutrie,
  design = design,
  family = quasibinomial(link = "logit")
)

summary(model_v2)

# Odds-ratios
cat("\n=== ODDS-RATIOS MODEL V2 ===\n")
or_v2 <- exp(cbind(OR = coef(model_v2), confint(model_v2)))
round(or_v2, 3)

# Odds-ratios et IC 95%
exp(cbind(OR = coef(model_final), confint(model_final)))

# -----------------------------------------------
# ETAPE 3 : ANALYSE DESCRIPTIVE
# -----------------------------------------------

## Filtrer les enfants avec stunting valide

d_clean <- enfants %>%
  filter(!is.na(stunting)) %>%
  mutate(
    stunting       = as.numeric(stunting),
    vaccin_bin     = ifelse(vaccin_dtc3 == 1, 1,
                     ifelse(vaccin_dtc3 == 0, 0, NA)),
    richesse_f     = factor(richesse, levels = 1:5,
                     labels = c("Très pauvre","Pauvre",
                                "Moyen","Riche","Très riche")),
    instruction    = relevel(instruction, ref = "Aucune"),
    milieu         = relevel(milieu,      ref = "Urbain"),
    sexe           = relevel(sexe,        ref = "Masculin")
  )

dim(d_clean)

# -----------------------------------------------
# TABLEAUX DE PREVALENCE PAR VARIABLE
# -----------------------------------------------

# Sexe
cat("\n--- SEXE ---\n")
table(d_clean$sexe, d_clean$stunting)
round(prop.table(table(d_clean$sexe, d_clean$stunting), margin = 1) * 100, 1)

# Milieu
cat("\n--- MILIEU ---\n")
table(d_clean$milieu, d_clean$stunting)
round(prop.table(table(d_clean$milieu, d_clean$stunting), margin = 1) * 100, 1)

# Instruction
cat("\n--- INSTRUCTION ---\n")
table(d_clean$instruction, d_clean$stunting, useNA = "ifany")
round(prop.table(table(d_clean$instruction, d_clean$stunting), margin = 1) * 100, 1)

# Richesse
cat("\n--- RICHESSE ---\n")
table(d_clean$richesse_f, d_clean$stunting)
round(prop.table(table(d_clean$richesse_f, d_clean$stunting), margin = 1) * 100, 1)

# Eau amelioree
cat("\n--- EAU AMELIOREE ---\n")
table(d_clean$eau_amelioree, d_clean$stunting)
round(prop.table(table(d_clean$eau_amelioree, d_clean$stunting), margin = 1) * 100, 1)

# Vaccin binaire (remplace vaccin_dtc3)
cat("\n--- VACCINATION DTC3 ---\n")
table(d_clean$vaccin_bin, d_clean$stunting, useNA = "ifany")
round(prop.table(table(d_clean$vaccin_bin, d_clean$stunting), margin = 1) * 100, 1)

# Malnutrition mere
cat("\n--- MALNUTRITION MERE ---\n")
table(d_clean$mere_malnutrie, d_clean$stunting, useNA = "ifany")
round(prop.table(table(d_clean$mere_malnutrie, d_clean$stunting), margin = 1) * 100, 1)

# CPN adequat
cat("\n--- CPN ADEQUAT (>=4) ---\n")
table(d_clean$cpn_adequat, d_clean$stunting, useNA = "ifany")
round(prop.table(table(d_clean$cpn_adequat, d_clean$stunting), margin = 1) * 100, 1)

# Age de l'enfant
cat("\n--- AGE DE L'ENFANT (mois) ---\n")
d_clean %>%
  group_by(stunting) %>%
  summarise(
    n            = n(),
    moyenne_age  = round(mean(age_enfant, na.rm = TRUE), 1),
    sd_age       = round(sd(age_enfant,   na.rm = TRUE), 1),
    mediane_age  = round(median(age_enfant, na.rm = TRUE), 1)
  )

# Taille du menage
cat("\n--- TAILLE DU MENAGE ---\n")
d_clean %>%
  group_by(stunting) %>%
  summarise(
    moyenne = round(mean(taille_menage, na.rm = TRUE), 1),
    sd      = round(sd(taille_menage,   na.rm = TRUE), 1)
  )

# IMC de la mere
cat("\n--- IMC DE LA MERE ---\n")
d_clean %>%
  group_by(stunting) %>%
  summarise(
    moyenne_imc = round(mean(imc_mere, na.rm = TRUE), 2),
    sd_imc      = round(sd(imc_mere,   na.rm = TRUE), 2)
  )

# ================================================
# ETAPE 4 : TESTS DU CHI-DEUX
# ================================================

cat("\n=== TESTS DU CHI-DEUX ===\n")

vars_quali <- list(
  "Sexe"              = d_clean$sexe,
  "Milieu"            = d_clean$milieu,
  "Instruction"       = d_clean$instruction,
  "Richesse"          = d_clean$richesse_f,
  "Eau améliorée"     = d_clean$eau_amelioree,
  "Vaccin DTC3"       = d_clean$vaccin_bin,
  "Malnutrition mère" = d_clean$mere_malnutrie
)

for (nom in names(vars_quali)) {
  test <- chisq.test(vars_quali[[nom]], d_clean$stunting)
  cat(sprintf("%-22s : X²=%.3f, df=%d, p=%.4f %s\n",
              nom,
              test$statistic,
              test$parameter,
              test$p.value,
              ifelse(test$p.value < 0.05, "✅ SIGNIFICATIF", "❌ non significatif")))
}

# Test t pour variables continues
cat("\n=== TESTS T (variables continues) ===\n")
t_age <- t.test(age_enfant ~ stunting, data = d_clean)
t_imc <- t.test(imc_mere   ~ stunting, data = d_clean)
t_men <- t.test(taille_menage ~ stunting, data = d_clean)

cat(sprintf("Age enfant    : t=%.3f, p=%.4f %s\n",
    t_age$statistic, t_age$p.value,
    ifelse(t_age$p.value < 0.05, "✅ SIGNIFICATIF", "❌ non significatif")))
cat(sprintf("IMC mère      : t=%.3f, p=%.4f %s\n",
    t_imc$statistic, t_imc$p.value,
    ifelse(t_imc$p.value < 0.05, "✅ SIGNIFICATIF", "❌ non significatif")))
cat(sprintf("Taille ménage : t=%.3f, p=%.4f %s\n",
    t_men$statistic, t_men$p.value,
    ifelse(t_men$p.value < 0.05, "✅ SIGNIFICATIF", "❌ non significatif")))


# ================================================
# ETAPE 5 : MODELE LOGISTIQUE FINAL
# ================================================




# ================================================
# VALIDATION DU MODEL V2
# ================================================

library(pROC)

# Données complètes pour ROC
vars_v2 <- c("stunting","sexe","milieu","instruction",
             "richesse_f","age_enfant","mere_malnutrie")
d_v2 <- na.omit(d_clean[, vars_v2])
cat("Observations pour ROC :", nrow(d_v2), "\n")

# Modèle glm identique pour ROC (svyglm incompatible avec pROC)
model_v2_glm <- glm(
  stunting ~ sexe + milieu + instruction + richesse_f +
             age_enfant + mere_malnutrie,
  data = d_v2, family = binomial
)

# Courbe ROC
pred_v2 <- predict(model_v2_glm, type = "response")
roc_v2 <- roc(d_v2$stunting, pred_v2)
cat("AUC model_v2 :", round(auc(roc_v2), 4), "\n")

# Graphique ROC propre
plot(roc_v2,
     main  = "Courbe ROC — Modèle final des déterminants du stunting\nBurkina Faso, EDS 2021",
     col   = "#1F5C99",
     lwd   = 2,
     print.auc = TRUE,
     print.auc.x = 0.4,
     print.auc.y = 0.15)
abline(a=0, b=1, lty=2, col="gray60")

# Hosmer-Lemeshow
library(ResourceSelection)
hoslem.test(d_v2$stunting, fitted(model_v2_glm), g=10)

# ================================================
# EFFETS MARGINAUX
# ================================================

library(margins)
marg_v2 <- margins(model_v2_glm)
summary(marg_v2)




# ================================================
# AJOUT DES VARIABLES ALIMENTAIRES ET INFECTIONS
# ================================================

enfants <- enfants %>%
  mutate(
    # Durée allaitement en mois (déjà créée mais pas utilisée)
    allaitement_exclu = ifelse(m4 == 0, 1,  # jamais allaité
                        ifelse(m4 >= 95, NA, 0)),

    # Diversification alimentaire (score simplifié)
    # v414b = céréales, v414c = légumineuses, v414e = produits laitiers
    # v414f = viande/poisson, v414i = légumes verts, v414j = fruits
    div_alim = as.numeric(v414b == 1) +
               as.numeric(v414c == 1) +
               as.numeric(v414e == 1) +
               as.numeric(v414f == 1) +
               as.numeric(v414i == 1) +
               as.numeric(v414j == 1),

    # Diversification adéquate (>= 4 groupes sur 6)
    div_adequate = ifelse(div_alim >= 4, 1,
                   ifelse(is.na(div_alim), NA, 0)),

    # Diarrhée récente (2 dernières semaines)
    diarrhee = ifelse(h11 == 1 | h11 == 2, 1,
               ifelse(is.na(h11), NA, 0)),

    # Fièvre récente
    fievre = ifelse(h22 == 1, 1,
             ifelse(is.na(h22), NA, 0)),

    # Région (proxy sécuritaire)
    region = factor(v024),

    # Variable composite infections : diarrhée OU fièvre
    infection_recente = ifelse(diarrhee == 1 | fievre == 1, 1,
                        ifelse(is.na(diarrhee) & is.na(fievre), NA, 0))
  )

# Vérifier
cat("--- DIVERSIFICATION ALIMENTAIRE ---\n")
table(enfants$div_adequate, useNA = "ifany")

cat("--- DIARRHEE RECENTE ---\n")
table(enfants$diarrhee, useNA = "ifany")

cat("--- FIEVRE RECENTE ---\n")
table(enfants$fievre, useNA = "ifany")

cat("--- REGION ---\n")
table(enfants$region, useNA = "ifany")



d_clean <- enfants %>%
  filter(!is.na(stunting)) %>%
  mutate(
    stunting    = as.numeric(stunting),
    richesse_f  = factor(richesse, levels = 1:5,
                  labels = c("Très pauvre","Pauvre",
                             "Moyen","Riche","Très riche")),
    instruction = relevel(instruction, ref = "Aucune"),
    milieu      = relevel(milieu,      ref = "Urbain"),
    sexe        = relevel(sexe,        ref = "Masculin")
  )

design <- svydesign(
  id = ~v001, strata = ~v022,
  weights = ~I(v005/1000000),
  data = d_clean, nest = TRUE
)

# Modèle enrichi
model_v3 <- svyglm(
  stunting ~ sexe + milieu + instruction + richesse_f +
             age_enfant + mere_malnutrie +
             diarrhee + fievre + div_adequate + region,
  design = design,
  family = quasibinomial(link = "logit")
)

summary(model_v3)
exp(cbind(OR = coef(model_v3), confint(model_v3)))





# ================================================
# MODEL V4 — MODELE FINAL OPTIMAL
# ================================================

model_v4 <- svyglm(
  stunting ~ sexe + milieu + instruction + richesse_f +
             age_enfant + mere_malnutrie + region,
  design = design,
  family = quasibinomial(link = "logit")
)

summary(model_v4)
exp(cbind(OR = coef(model_v4), confint(model_v4)))

# ================================================
# VALIDATION MODEL V4
# ================================================

library(pROC)

vars_v4 <- c("stunting","sexe","milieu","instruction",
             "richesse_f","age_enfant","mere_malnutrie","region")
d_v4 <- na.omit(d_clean[, vars_v4])
cat("Observations pour validation :", nrow(d_v4), "\n")

model_v4_glm <- glm(
  stunting ~ sexe + milieu + instruction + richesse_f +
             age_enfant + mere_malnutrie + region,
  data = d_v4, family = binomial
)

pred_v4 <- predict(model_v4_glm, type = "response")
roc_v4  <- roc(d_v4$stunting, pred_v4)
cat("AUC model_v4 :", round(auc(roc_v4), 4), "\n")

plot(roc_v4,
     main = "Courbe ROC — Modèle final\nBurkina Faso, EDS 2021",
     col  = "#1F5C99", lwd = 2,
     print.auc = TRUE,
     print.auc.x = 0.4, print.auc.y = 0.15)
abline(a=0, b=1, lty=2, col="gray60")

# Hosmer-Lemeshow
library(ResourceSelection)
hoslem.test(d_v4$stunting, fitted(model_v4_glm), g=10)

# Effets marginaux
library(margins)
marg_v4 <- margins(model_v4_glm)
summary(marg_v4)

# Identifier les labels des régions
cat("\n--- LABELS DES REGIONS ---\n")
print(attr(enfants$v024, "labels"))






# ================================================
# CALCUL SENSIBILITE ET SPECIFICITE REELLES
# ================================================

# Probabilités prédites par le modèle
pred_v4 <- predict(model_v4_glm, type = "response")

# Seuil standard = 0.5
seuil <- 0.5
pred_classe <- ifelse(pred_v4 >= seuil, 1, 0)

# Matrice de confusion
mat <- table(Prédit = pred_classe, Réel = d_v4$stunting)
print(mat)

# Extraction des valeurs
VP <- mat[2,2]  # stunted prédit comme stunted
VN <- mat[1,1]  # non-stunted prédit comme non-stunted
FP <- mat[2,1]  # non-stunted prédit comme stunted
FN <- mat[1,2]  # stunted prédit comme non-stunted

# Calcul des métriques
sensibilite  <- VP / (VP + FN)
specificite  <- VN / (VN + FP)
precision    <- VP / (VP + FP)
exactitude   <- (VP + VN) / (VP + VN + FP + FN)

cat("\n=== MÉTRIQUES DU MODÈLE (seuil = 0.5) ===\n")
cat(sprintf("Sensibilité  : %.1f%%\n", sensibilite * 100))
cat(sprintf("Spécificité  : %.1f%%\n", specificite * 100))
cat(sprintf("Précision    : %.1f%%\n", precision   * 100))
cat(sprintf("Exactitude   : %.1f%%\n", exactitude  * 100))

cat("\n--- Détail ---\n")
cat(sprintf("VP (stunted bien détectés)     : %d\n", VP))
cat(sprintf("VN (non-stunted bien classés)  : %d\n", VN))
cat(sprintf("FP (non-stunted faussement     : %d\n", FP))
cat(sprintf("    classés stunted)           \n"))
cat(sprintf("FN (stunted manqués)           : %d\n", FN))

# Seuil optimal (maximise sensibilité + spécificité)
library(pROC)
roc_obj  <- roc(d_v4$stunting, pred_v4)
opt      <- coords(roc_obj, "best", ret = c("threshold","sensitivity","specificity"))
cat("\n=== SEUIL OPTIMAL ===\n")
print(round(opt, 3))



#### les pseudo-R² 
PseudoR2(model_v4_glm, which = c("McFadden", "Nagelkerke", "CoxSnell"))
AIC(model_v2_glm)
AIC(model_v4_glm)

### Test du rapport de vraisemblance (Likelihood Ratio Test)
# Comparer model_v2_glm et model_v4_glm
anova(model_v2_glm, model_v4_glm, test = "LRT")

### Critère AIC (Akaike Information Criterion, critère d'information d'Akaike) 
AIC(model_v2_glm)
AIC(model_v4_glm)

### Matrice de confusion
# Avec seuil de 0.5
pred_classe <- ifelse(pred_v4 > 0.5, 1, 0)
table(Prédit = pred_classe, Observé = d_v4$stunting)



#### Vue qu'avec leseuil de 0.5 on ne trouve aucune prediction d'enfant stunting

### Allons au seuil optimal de 0.2


# Seuil optimal = 0.20
seuil_opt <- 0.20
pred_opt  <- ifelse(pred_v4 >= seuil_opt, 1, 0)

# Matrice de confusion correcte
mat <- table(Prédit = pred_opt, Réel = d_v4$stunting)
print(mat)

# Métriques
VP <- mat["1","1"]
VN <- mat["0","0"]
FP <- mat["1","0"]
FN <- mat["0","1"]

cat(sprintf("\nSensibilité : %.1f%%\n", VP/(VP+FN)*100))
cat(sprintf("Spécificité : %.1f%%\n", VN/(VN+FP)*100))
cat(sprintf("Précision   : %.1f%%\n", VP/(VP+FP)*100))
cat(sprintf("Exactitude  : %.1f%%\n", (VP+VN)/(VP+VN+FP+FN)*100))

cat(sprintf("\nVP (stunted bien détectés)    : %d sur %d\n", VP, VP+FN))
cat(sprintf("VN (sains bien classés)       : %d sur %d\n", VN, VN+FP))
cat(sprintf("FP (sains classés stunted)    : %d\n", FP))
cat(sprintf("FN (stunted manqués)          : %d\n", FN))




# Variables supplémentaires à ajouter
enfants <- enfants %>%
  mutate(
    # Intervalle de naissance (en mois)
    intervalle_naiss = b11,
    intervalle_cat = case_when(
      b11 < 24              ~ "Moins de 24 mois",
      b11 >= 24 & b11 < 36  ~ "24 à 35 mois",
      b11 >= 36             ~ "36 mois et plus",
      is.na(b11)            ~ NA_character_
    ),
    
    # Présence de jumeaux
    jumeau = ifelse(b0 > 0, 1, 0),
    
    # Activité économique de la mère
    activite_mere = case_when(
      v717 == 0 ~ "Sans activité",
      v717 %in% c(1,2,3) ~ "Agriculture",
      v717 %in% c(4,5) ~ "Commerce",
      v717 %in% c(6,7,8,9) ~ "Autres activités",
      TRUE ~ NA_character_
    ),
    
    # Statut matrimonial de la mère
    statut_mat = case_when(
      v501 == 1 ~ "Mariée",
      v501 == 0 ~ "Célibataire/Seule",
      v501 %in% c(2,3,4,5) ~ "Divorcée/Veuve/Séparée",
      TRUE ~ NA_character_
    ),
    
    # Nombre d'enfants moins de 5 ans dans le ménage
    nb_enfants_5 = v137,
    nb_enfants_cat = case_when(
      v137 == 1 ~ "1 enfant",
      v137 == 2 ~ "2 enfants",
      v137 >= 3 ~ "3 enfants et plus",
      TRUE ~ NA_character_
    )
  )

# Vérifications
cat("--- INTERVALLE NAISSANCE ---\n")
table(enfants$intervalle_cat, useNA = "ifany")

cat("--- JUMEAUX ---\n")
table(enfants$jumeau, useNA = "ifany")

cat("--- ACTIVITE MERE ---\n")
table(enfants$activite_mere, useNA = "ifany")

cat("--- STATUT MATRIMONIAL ---\n")
table(enfants$statut_mat, useNA = "ifany")

cat("--- NB ENFANTS < 5 ANS ---\n")
table(enfants$nb_enfants_cat, useNA = "ifany")





# Couleurs officielles
col_normal  <- "#2E86C1"
col_stunted <- "#E74C3C"

# Reconstruire d_clean avec toutes les variables
d_clean <- enfants %>%
  filter(!is.na(stunting)) %>%
  mutate(
    stunting = as.numeric(stunting),
    stunting_label = ifelse(stunting == 1, "Retard de croissance", "Normal"),
    richesse_f = factor(richesse, levels = 1:5,
                 labels = c("Très pauvre","Pauvre","Moyen","Riche","Très riche")),
    instruction = relevel(instruction, ref = "Aucune"),
    milieu = relevel(milieu, ref = "Urbain"),
    sexe = relevel(sexe, ref = "Masculin"),
    region_label = factor(region, levels = 1:13,
                   labels = c("Boucle du Mouhoun","Cascades","Centre",
                              "Centre Est","Centre Nord","Centre Ouest",
                              "Centre Sud","Est","Hauts-Bassins",
                              "Nord","Plateau Central","Sahel","Sud-Ouest")),
    age_cat = case_when(
      age_enfant < 6   ~ "0-5 mois",
      age_enfant < 12  ~ "6-11 mois",
      age_enfant < 24  ~ "12-23 mois",
      age_enfant < 36  ~ "24-35 mois",
      age_enfant < 48  ~ "36-47 mois",
      age_enfant <= 59 ~ "48-59 mois",
      TRUE ~ NA_character_
    )
  )

# -----------------------------------------------
# GRAPHIQUE 1 — Prévalence globale (camembert)
# -----------------------------------------------
prev_data <- d_clean %>%
  count(stunting_label) %>%
  mutate(pct = round(n/sum(n)*100, 1),
         label = paste0(pct, "%"))

g1 <- ggplot(prev_data, aes(x="", y=n, fill=stunting_label)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y") +
  scale_fill_manual(values = c("Normal" = col_normal,
                               "Retard de croissance" = col_stunted)) +
  geom_text(aes(label=label), position=position_stack(vjust=0.5),
            size=5, fontface="bold", color="white") +
  labs(title = "Situation nutritionnelle des enfants de moins de 5 ans",
       subtitle = "Burkina Faso, EDS 2021",
       fill = "", x = NULL, y = NULL) +
  theme_void(base_size = 13) +
  theme(plot.title = element_text(hjust=0.5, face="bold", color="#1F5C99"),
        plot.subtitle = element_text(hjust=0.5, color="gray50"),
        legend.position = "bottom")

ggsave("04_Resultats/G1_prevalence_globale.png", g1,
       width=7, height=6, dpi=300, bg="white")
cat("G1 sauvegardé\n")

# -----------------------------------------------
# GRAPHIQUE 2 — Par tranche d'âge
# -----------------------------------------------
age_data <- d_clean %>%
  filter(!is.na(age_cat)) %>%
  group_by(age_cat) %>%
  summarise(prev = round(mean(stunting, na.rm=TRUE)*100, 1)) %>%
  mutate(age_cat = factor(age_cat, levels=c("0-5 mois","6-11 mois",
                                             "12-23 mois","24-35 mois",
                                             "36-47 mois","48-59 mois")))

g2 <- ggplot(age_data, aes(x=age_cat, y=prev, fill=prev)) +
  geom_bar(stat="identity", color="white", width=0.7) +
  geom_text(aes(label=paste0(prev,"%")), vjust=-0.5, fontface="bold", size=4) +
  scale_fill_gradient(low="#AED6F1", high=col_stunted) +
  labs(title = "Prévalence du retard de croissance selon la tranche d'âge",
       subtitle = "Burkina Faso, EDS 2021",
       x = "Tranche d'âge", y = "Prévalence (%)",
       fill = "Prévalence (%)") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face="bold", color="#1F5C99"),
        plot.subtitle = element_text(color="gray50"),
        axis.text.x = element_text(angle=30, hjust=1))

ggsave("04_Resultats/G2_prevalence_age.png", g2,
       width=9, height=6, dpi=300, bg="white")
cat("G2 sauvegardé\n")

# -----------------------------------------------
# GRAPHIQUE 3 — Par région (LE PLUS IMPACTANT)
# -----------------------------------------------
region_data <- d_clean %>%
  filter(!is.na(region_label)) %>%
  group_by(region_label) %>%
  summarise(prev = round(mean(stunting, na.rm=TRUE)*100, 1)) %>%
  arrange(desc(prev))

g3 <- ggplot(region_data, aes(x=reorder(region_label, prev), y=prev, fill=prev)) +
  geom_bar(stat="identity", color="white", width=0.75) +
  geom_text(aes(label=paste0(prev,"%")), hjust=-0.2, fontface="bold", size=3.8) +
  scale_fill_gradient(low="#AED6F1", high=col_stunted) +
  coord_flip() +
  geom_hline(yintercept=21.27, linetype="dashed", color="#1F5C99", linewidth=0.8) +
  annotate("text", x=1, y=22.5, label="Moyenne nationale\n21,27%",
           color="#1F5C99", size=3.2, hjust=0) +
  labs(title = "Prévalence du retard de croissance par région",
       subtitle = "Burkina Faso, EDS 2021",
       x = NULL, y = "Prévalence (%)",
       fill = "Prévalence (%)") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face="bold", color="#1F5C99"),
        plot.subtitle = element_text(color="gray50"))

ggsave("04_Resultats/G3_prevalence_region.png", g3,
       width=10, height=7, dpi=300, bg="white")
cat("G3 sauvegardé\n")

# -----------------------------------------------
# GRAPHIQUE 4 — Par milieu de résidence
# -----------------------------------------------
milieu_data <- d_clean %>%
  group_by(milieu) %>%
  summarise(prev = round(mean(stunting, na.rm=TRUE)*100, 1))

g4 <- ggplot(milieu_data, aes(x=milieu, y=prev, fill=milieu)) +
  geom_bar(stat="identity", color="white", width=0.5) +
  geom_text(aes(label=paste0(prev,"%")), vjust=-0.5, fontface="bold", size=5) +
  scale_fill_manual(values=c("Urbain"=col_normal, "Rural"=col_stunted)) +
  labs(title = "Prévalence du retard de croissance selon le milieu de résidence",
       subtitle = "Burkina Faso, EDS 2021",
       x = NULL, y = "Prévalence (%)", fill = NULL) +
  theme_minimal(base_size=13) +
  theme(plot.title = element_text(face="bold", color="#1F5C99"),
        plot.subtitle = element_text(color="gray50"),
        legend.position = "none")

ggsave("04_Resultats/G4_prevalence_milieu.png", g4,
       width=7, height=6, dpi=300, bg="white")
cat("G4 sauvegardé\n")

# -----------------------------------------------
# GRAPHIQUE 5 — Par niveau de richesse
# -----------------------------------------------
richesse_data <- d_clean %>%
  filter(!is.na(richesse_f)) %>%
  group_by(richesse_f) %>%
  summarise(prev = round(mean(stunting, na.rm=TRUE)*100, 1))

g5 <- ggplot(richesse_data, aes(x=richesse_f, y=prev, fill=prev)) +
  geom_bar(stat="identity", color="white", width=0.7) +
  geom_text(aes(label=paste0(prev,"%")), vjust=-0.5, fontface="bold", size=4) +
  scale_fill_gradient(low="#A9DFBF", high=col_stunted) +
  labs(title = "Prévalence du retard de croissance selon le niveau de richesse",
       subtitle = "Burkina Faso, EDS 2021",
       x = "Quintile de richesse", y = "Prévalence (%)", fill = "Prévalence (%)") +
  theme_minimal(base_size=13) +
  theme(plot.title = element_text(face="bold", color="#1F5C99"),
        plot.subtitle = element_text(color="gray50"))

ggsave("04_Resultats/G5_prevalence_richesse.png", g5,
       width=9, height=6, dpi=300, bg="white")
cat("G5 sauvegardé\n")

# -----------------------------------------------
# GRAPHIQUE 6 — Par instruction de la mère
# -----------------------------------------------
instruct_data <- d_clean %>%
  filter(!is.na(instruction)) %>%
  group_by(instruction) %>%
  summarise(prev = round(mean(stunting, na.rm=TRUE)*100, 1))

g6 <- ggplot(instruct_data, aes(x=instruction, y=prev, fill=instruction)) +
  geom_bar(stat="identity", color="white", width=0.55) +
  geom_text(aes(label=paste0(prev,"%")), vjust=-0.5, fontface="bold", size=4.5) +
  scale_fill_manual(values=c("Aucune"=col_stunted,
                             "Primaire"="#E67E22",
                             "Secondaire+"=col_normal)) +
  labs(title = "Prévalence du retard de croissance selon l'instruction de la mère",
       subtitle = "Burkina Faso, EDS 2021",
       x = "Niveau d'instruction", y = "Prévalence (%)", fill = NULL) +
  theme_minimal(base_size=13) +
  theme(plot.title = element_text(face="bold", color="#1F5C99"),
        plot.subtitle = element_text(color="gray50"),
        legend.position = "none")

ggsave("04_Resultats/G6_prevalence_instruction.png", g6,
       width=8, height=6, dpi=300, bg="white")
cat("G6 sauvegardé\n")

# -----------------------------------------------
# GRAPHIQUE 7 — Forest plot des odds-ratios
# -----------------------------------------------
library(tibble)

or_df <- tibble(
  Variable = c("Sexe féminin","Milieu rural","Instruction primaire",
               "Instruction secondaire+","Richesse pauvre","Richesse moyen",
               "Richesse riche","Richesse très riche","Âge enfant (mois)",
               "Malnutrition mère","Région Sahel"),
  OR    = c(0.732, 1.459, 0.729, 0.916, 1.018, 0.800, 0.791, 0.594, 1.006, 1.156, 1.771),
  lower = c(0.639, 1.097, 0.570, 0.719, 0.808, 0.629, 0.612, 0.398, 1.002, 0.898, 0.926),
  upper = c(0.839, 1.940, 0.932, 1.167, 1.283, 1.019, 1.023, 0.887, 1.010, 1.487, 3.388),
  Significatif = c("Oui","Oui","Oui","Non","Non","Non","Non","Oui","Oui","Non","Limite")
)

g7 <- ggplot(or_df, aes(x=OR, y=reorder(Variable, OR), color=Significatif)) +
  geom_point(size=4) +
  geom_errorbarh(aes(xmin=lower, xmax=upper), height=0.3, linewidth=0.8) +
  geom_vline(xintercept=1, linetype="dashed", color="gray40", linewidth=0.8) +
  scale_color_manual(values=c("Oui"="#1F5C99","Non"="gray60","Limite"="#E67E22")) +
  labs(title = "Odds-ratios ajustés — Déterminants du retard de croissance",
       subtitle = "Modèle logistique avec plan de sondage — Burkina Faso, EDS 2021",
       x = "Odds Ratio (IC 95%)", y = NULL,
       color = "Significatif (p<0,05)") +
  theme_minimal(base_size=13) +
  theme(plot.title = element_text(face="bold", color="#1F5C99"),
        plot.subtitle = element_text(color="gray50"))

ggsave("04_Resultats/G7_forest_plot_OR.png", g7,
       width=11, height=7, dpi=300, bg="white")
cat("G7 Forest plot sauvegardé\n")

cat("\n=== TOUS LES GRAPHIQUES PRODUITS AVEC SUCCES ===\n")



# ================================================
# ANALYSE DES NOUVELLES VARIABLES
# ================================================

# Ajouter les nouvelles variables dans d_clean
d_clean <- enfants %>%
  filter(!is.na(stunting)) %>%
  mutate(
    stunting = as.numeric(stunting),
    richesse_f = factor(richesse, levels = 1:5,
                 labels = c("Très pauvre","Pauvre","Moyen","Riche","Très riche")),
    instruction  = relevel(instruction, ref = "Aucune"),
    milieu       = relevel(milieu,       ref = "Urbain"),
    sexe         = relevel(sexe,         ref = "Masculin"),
    region_label = factor(region, levels = 1:13,
                   labels = c("Boucle du Mouhoun","Cascades","Centre",
                              "Centre Est","Centre Nord","Centre Ouest",
                              "Centre Sud","Est","Hauts-Bassins","Nord",
                              "Plateau Central","Sahel","Sud-Ouest")),

    # Nouvelles variables
    jumeau_f = factor(ifelse(b0 > 0, 1, 0),
               levels = c(0,1),
               labels = c("Non jumeau","Jumeau")),

    intervalle_f = factor(case_when(
      b11 < 24             ~ "Moins de 24 mois",
      b11 >= 24 & b11 < 36 ~ "24-35 mois",
      b11 >= 36            ~ "36 mois et plus",
      TRUE                 ~ NA_character_
    ), levels = c("36 mois et plus","24-35 mois","Moins de 24 mois")),

    activite_f = factor(case_when(
      v717 == 0            ~ "Sans activité",
      v717 %in% c(1,2,3)   ~ "Agriculture",
      v717 %in% c(4,5)     ~ "Commerce",
      v717 %in% c(6,7,8,9) ~ "Autres activités",
      TRUE                 ~ NA_character_
    )),

    statut_mat_f = factor(case_when(
      v501 == 1            ~ "Mariée",
      v501 == 0            ~ "Célibataire/Seule",
      v501 %in% c(2,3,4,5) ~ "Divorcée/Veuve/Séparée",
      TRUE                 ~ NA_character_
    )),

    nb_enf_f = factor(case_when(
      v137 == 1  ~ "1 enfant",
      v137 == 2  ~ "2 enfants",
      v137 >= 3  ~ "3 enfants et plus",
      TRUE       ~ NA_character_
    ), levels = c("1 enfant","2 enfants","3 enfants et plus"))
  )

# ================================================
# TABLEAUX DE PREVALENCE — nouvelles variables
# ================================================

cat("\n--- JUMEAUX ---\n")
tab_jum <- table(d_clean$jumeau_f, d_clean$stunting)
print(tab_jum)
round(prop.table(tab_jum, margin=1)*100, 1)

cat("\n--- INTERVALLE DE NAISSANCE ---\n")
tab_int <- table(d_clean$intervalle_f, d_clean$stunting)
print(tab_int)
round(prop.table(tab_int, margin=1)*100, 1)

cat("\n--- ACTIVITE ECONOMIQUE DE LA MERE ---\n")
tab_act <- table(d_clean$activite_f, d_clean$stunting)
print(tab_act)
round(prop.table(tab_act, margin=1)*100, 1)

cat("\n--- STATUT MATRIMONIAL ---\n")
tab_mat <- table(d_clean$statut_mat_f, d_clean$stunting)
print(tab_mat)
round(prop.table(tab_mat, margin=1)*100, 1)

cat("\n--- NOMBRE ENFANTS < 5 ANS ---\n")
tab_nb <- table(d_clean$nb_enf_f, d_clean$stunting)
print(tab_nb)
round(prop.table(tab_nb, margin=1)*100, 1)

# ================================================
# TESTS DU CHI-DEUX — nouvelles variables
# ================================================

cat("\n=== TESTS CHI-DEUX NOUVELLES VARIABLES ===\n")

nouvelles_vars <- list(
  "Jumeaux"              = d_clean$jumeau_f,
  "Intervalle naissance" = d_clean$intervalle_f,
  "Activité mère"        = d_clean$activite_f,
  "Statut matrimonial"   = d_clean$statut_mat_f,
  "Nb enfants < 5 ans"   = d_clean$nb_enf_f
)

for (nom in names(nouvelles_vars)) {
  test <- chisq.test(nouvelles_vars[[nom]], d_clean$stunting)
  cat(sprintf("%-25s : X²=%.3f, df=%d, p=%.4f %s\n",
              nom,
              test$statistic,
              test$parameter,
              test$p.value,
              ifelse(test$p.value < 0.05, "✅ SIGNIFICATIF", "❌ non significatif")))
}

# ================================================
# MODELE FINAL ENRICHI — avec nouvelles variables
# ================================================

# Redéclarer le design avec d_clean enrichi
design <- svydesign(
  id      = ~v001,
  strata  = ~v022,
  weights = ~I(v005 / 1000000),
  data    = d_clean,
  nest    = TRUE
)

# Modèle complet enrichi
model_v5 <- svyglm(
  stunting ~ sexe + milieu + instruction + richesse_f +
             age_enfant + mere_malnutrie + region +
             jumeau_f + intervalle_f + activite_f,
  design = design,
  family = quasibinomial(link = "logit")
)

summary(model_v5)
cat("\n=== ODDS-RATIOS MODEL V5 ===\n")
round(exp(cbind(OR = coef(model_v5), confint(model_v5))), 3)







# ================================================
# TEST DE NORMALITE — Variables continues
# ================================================

library(ggplot2)

# Les variables continues de ton analyse
vars_continues <- list(
  "Age de l'enfant (mois)" = d_clean$age_enfant,
  "IMC de la mère"         = d_clean$imc_mere,
  "Taille du ménage"       = d_clean$taille_menage
)

cat("=== TESTS DE NORMALITE (Kolmogorov-Smirnov) ===\n")
cat("Note : avec n > 5000, KS est plus adapté que Shapiro-Wilk\n\n")

for (nom in names(vars_continues)) {
  x <- vars_continues[[nom]]
  x <- x[!is.na(x)]
  
  # Test KS
  ks <- ks.test(x, "pnorm", mean=mean(x), sd=sd(x))
  
  # Asymétrie et aplatissement
  skew <- (sum((x - mean(x))^3) / length(x)) / (sd(x)^3)
  kurt <- (sum((x - mean(x))^4) / length(x)) / (sd(x)^4) - 3
  
  cat(sprintf("--- %s ---\n", nom))
  cat(sprintf("  N = %d | Moy = %.2f | SD = %.2f\n", length(x), mean(x), sd(x)))
  cat(sprintf("  Asymétrie = %.3f | Aplatissement = %.3f\n", skew, kurt))
  cat(sprintf("  KS : D = %.4f, p = %.6f → %s\n\n",
              ks$statistic, ks$p.value,
              ifelse(ks$p.value > 0.05, "NORMALE", "NON NORMALE")))
}

# ================================================
# QQ-PLOTS visuels
# ================================================

par(mfrow = c(1,3))

qqnorm(d_clean$age_enfant[!is.na(d_clean$age_enfant)],
       main = "QQ-Plot — Âge de l'enfant",
       col = "#1F5C99", pch = 16, cex = 0.3)
qqline(d_clean$age_enfant[!is.na(d_clean$age_enfant)],
       col = "red", lwd = 2)

qqnorm(d_clean$imc_mere[!is.na(d_clean$imc_mere)],
       main = "QQ-Plot — IMC de la mère",
       col = "#1F5C99", pch = 16, cex = 0.3)
qqline(d_clean$imc_mere[!is.na(d_clean$imc_mere)],
       col = "red", lwd = 2)

qqnorm(d_clean$taille_menage[!is.na(d_clean$taille_menage)],
       main = "QQ-Plot — Taille du ménage",
       col = "#1F5C99", pch = 16, cex = 0.3)
qqline(d_clean$taille_menage[!is.na(d_clean$taille_menage)],
       col = "red", lwd = 2)

par(mfrow = c(1,1))

# ================================================
# TESTS BIVARIES CORRIGES selon normalité
# ================================================

cat("\n=== TESTS BIVARIES CORRIGES ===\n")
cat("Si non normal → Wilcoxon (Mann-Whitney) au lieu du test t\n\n")

# Age enfant
w_age <- wilcox.test(age_enfant ~ stunting, data = d_clean)
t_age <- t.test(age_enfant ~ stunting, data = d_clean)
cat(sprintf("Age enfant — t-test : p = %.4f | Wilcoxon : p = %.4f\n",
            t_age$p.value, w_age$p.value))

# IMC mere
w_imc <- wilcox.test(imc_mere ~ stunting, data = d_clean, na.action = na.omit)
t_imc <- t.test(imc_mere ~ stunting, data = d_clean)
cat(sprintf("IMC mère   — t-test : p = %.4f | Wilcoxon : p = %.4f\n",
            t_imc$p.value, w_imc$p.value))

# Taille menage
w_men <- wilcox.test(taille_menage ~ stunting, data = d_clean)
t_men <- t.test(taille_menage ~ stunting, data = d_clean)
cat(sprintf("Taille mén — t-test : p = %.4f | Wilcoxon : p = %.4f\n",
            t_men$p.value, w_men$p.value))




# ================================================
# MODEL V6 — MODELE DEFINITIF
# Variables retenues : significatives + justifiées
# ================================================

model_v6 <- svyglm(
  stunting ~ sexe + milieu + instruction + richesse_f +
             age_enfant + mere_malnutrie +
             jumeau_f + intervalle_f,
  design = design,
  family = quasibinomial(link = "logit")
)

summary(model_v6)
round(exp(cbind(OR = coef(model_v6), confint(model_v6))), 3)

# Validation
vars_v6 <- c("stunting","sexe","milieu","instruction","richesse_f",
             "age_enfant","mere_malnutrie","jumeau_f","intervalle_f")
d_v6 <- na.omit(d_clean[, vars_v6])
cat("N observations model_v6 :", nrow(d_v6), "\n")

model_v6_glm <- glm(
  stunting ~ sexe + milieu + instruction + richesse_f +
             age_enfant + mere_malnutrie + jumeau_f + intervalle_f,
  data = d_v6, family = binomial
)

library(pROC)
pred_v6 <- predict(model_v6_glm, type = "response")
roc_v6 <- roc(d_v6$stunting, pred_v6)
cat("AUC model_v6 :", round(auc(roc_v6), 4), "\n")

library(ResourceSelection)
hoslem.test(d_v6$stunting, fitted(model_v6_glm), g=10)

library(DescTools)
PseudoR2(model_v6_glm, which = c("McFadden","Nagelkerke","CoxSnell"))

library(margins)
marg_v6 <- margins(model_v6_glm)
summary(marg_v6)




# ================================================
# SOLUTION : catégorie "Premier-né" pour l'intervalle
# ================================================

d_clean <- enfants %>%
  filter(!is.na(stunting)) %>%
  mutate(
    stunting   = as.numeric(stunting),
    richesse_f = factor(richesse, levels = 1:5,
                 labels = c("Très pauvre","Pauvre","Moyen",
                            "Riche","Très riche")),
    instruction  = relevel(instruction, ref = "Aucune"),
    milieu       = relevel(milieu,      ref = "Urbain"),
    sexe         = relevel(sexe,        ref = "Masculin"),
    region_label = factor(region, levels = 1:13,
                   labels = c("Boucle du Mouhoun","Cascades","Centre",
                              "Centre Est","Centre Nord","Centre Ouest",
                              "Centre Sud","Est","Hauts-Bassins","Nord",
                              "Plateau Central","Sahel","Sud-Ouest")),
    activite_f = factor(case_when(
      v717 == 0            ~ "Sans activité",
      v717 %in% c(1,2,3)   ~ "Agriculture",
      v717 %in% c(4,5)     ~ "Commerce",
      v717 %in% c(6,7,8,9) ~ "Autres activités",
      TRUE                 ~ NA_character_
    ), levels = c("Agriculture","Commerce",
                  "Sans activité","Autres activités")),

    jumeau_f = factor(ifelse(b0 > 0, 1, 0),
               levels = c(0,1),
               labels = c("Non jumeau","Jumeau")),

    # SOLUTION ELEGANTE : Premier-né = catégorie à part
    intervalle_f = factor(case_when(
      is.na(b11)           ~ "Premier-né",      # pas d'enfant précédent
      b11 < 24             ~ "Moins de 24 mois",
      b11 >= 24 & b11 < 36 ~ "24-35 mois",
      b11 >= 36            ~ "36 mois et plus",
      TRUE                 ~ NA_character_
    ), levels = c("36 mois et plus",   # référence
                  "Premier-né",
                  "24-35 mois",
                  "Moins de 24 mois"))
  )

# Vérification — plus aucun NA sur intervalle
cat("Distribution intervalle_f :\n")
table(d_clean$intervalle_f, useNA = "ifany")
cat("\nN total d_clean :", nrow(d_clean), "\n")

# ================================================
# DESIGN avec d_clean complet
# ================================================
design <- svydesign(
  id = ~v001, strata = ~v022,
  weights = ~I(v005/1000000),
  data = d_clean, nest = TRUE
)

# ================================================
# MODELE FINAL UNIQUE — toutes variables
# ================================================
model_FINAL <- svyglm(
  stunting ~ sexe + milieu + instruction + richesse_f +
             age_enfant + mere_malnutrie + region +
             jumeau_f + intervalle_f,
  design = design,
  family = quasibinomial(link = "logit")
)

summary(model_FINAL)

cat("\n=== ODDS-RATIOS MODELE FINAL ===\n")
round(exp(cbind(OR = coef(model_FINAL), confint(model_FINAL))), 3)

# ================================================
# VALIDATION COMPLETE — UN SEUL MODELE
# ================================================
vars_FINAL <- c("stunting","sexe","milieu","instruction","richesse_f",
                "age_enfant","mere_malnutrie","region",
                "jumeau_f","intervalle_f")
d_FINAL <- na.omit(d_clean[, vars_FINAL])
cat("\nN observations modele final :", nrow(d_FINAL), "\n")

model_FINAL_glm <- glm(
  stunting ~ sexe + milieu + instruction + richesse_f +
             age_enfant + mere_malnutrie + region +
             jumeau_f + intervalle_f,
  data = d_FINAL, family = binomial
)

# AUC et courbe ROC
library(pROC)
pred_FINAL <- predict(model_FINAL_glm, type = "response")
roc_FINAL  <- roc(d_FINAL$stunting, pred_FINAL)
cat("AUC modele final :", round(auc(roc_FINAL), 4), "\n")

# Graphique ROC final
plot(roc_FINAL,
     main = "Courbe ROC — Modèle final des déterminants du stunting\nBurkina Faso, EDS 2021",
     col  = "#1F5C99", lwd = 2,
     print.auc = TRUE,
     print.auc.x = 0.4, print.auc.y = 0.15)
abline(a=0, b=1, lty=2, col="gray60")

# Hosmer-Lemeshow
library(ResourceSelection)
hl <- hoslem.test(d_FINAL$stunting, fitted(model_FINAL_glm), g=10)
cat("Hosmer-Lemeshow p =", round(hl$p.value, 4), "\n")

# Pseudo-R²
library(DescTools)
cat("\nPseudo-R² :\n")
print(round(PseudoR2(model_FINAL_glm,
      which = c("McFadden","Nagelkerke","CoxSnell")), 4))

# Effets marginaux
library(margins)
cat("\n=== EFFETS MARGINAUX MODELE FINAL ===\n")
summary(margins(model_FINAL_glm))

# Seuil optimal et métriques
opt <- coords(roc_FINAL, "best",
              ret = c("threshold","sensitivity","specificity"))
cat("\nSeuil optimal :", round(opt$threshold, 3))
cat("\nSensibilité   :", round(opt$sensitivity*100, 1), "%")
cat("\nSpécificité   :", round(opt$specificity*100, 1), "%\n")


# ================================================
# AUDIT COMPLET — Variables non encore testées
# ================================================

enfants <- enfants %>%
  mutate(
    # Âge de la mère (en années)
    age_mere = v012,
    age_mere_cat = case_when(
      v012 < 20              ~ "Moins de 20 ans",
      v012 >= 20 & v012 < 35 ~ "20-34 ans",
      v012 >= 35             ~ "35 ans et plus",
      TRUE                   ~ NA_character_
    ),

    # Religion du chef de ménage
    religion_cm = case_when(
      v130 == 1 ~ "Islam",
      v130 == 2 ~ "Christianisme",
      v130 %in% c(3,4,5,6) ~ "Animisme/Autre",
      TRUE ~ NA_character_
    ),

    # Activité économique du chef de ménage
    activite_cm = case_when(
      v714 == 0 ~ "Sans activité",
      v714 %in% c(1,2,3) ~ "Agriculture",
      v714 %in% c(4,5)   ~ "Commerce",
      v714 %in% c(6,7,8,9) ~ "Autres",
      TRUE ~ NA_character_
    ),

    # Sexe du chef de ménage
    sexe_cm = factor(v151, levels = c(1,2),
               labels = c("Masculin","Féminin")),

    # Rang de naissance
    rang_f = factor(case_when(
      bord %in% c(1,2)   ~ "Rang 1-2",
      bord %in% c(3,4)   ~ "Rang 3-4",
      bord >= 5          ~ "Rang 5 et plus",
      TRUE               ~ NA_character_
    ), levels = c("Rang 1-2","Rang 3-4","Rang 5 et plus")),

    # Assainissement
    assainissement = case_when(
      v116 %in% c(11,12,13,21,22) ~ "Amélioré",
      v116 %in% c(23,31,42,43,96) ~ "Non amélioré",
      v116 == 97 ~ NA_character_,
      TRUE ~ NA_character_
    ),

    # Supplémentation vitamine A
    vitamine_a = case_when(
      h33 == 1 ~ "Oui",
      h33 == 0 ~ "Non",
      TRUE ~ NA_character_
    ),

    # Déparasitage
    deparasitage = case_when(
      h43 == 1 ~ "Oui",
      h43 == 0 ~ "Non",
      TRUE ~ NA_character_
    ),

    # Allaitement (recodé proprement)
    allaitement_f = case_when(
      m4 == 0  ~ "Jamais allaité",
      m4 < 6   ~ "Moins de 6 mois",
      m4 < 24  ~ "6-23 mois",
      m4 >= 24 ~ "24 mois et plus",
      TRUE     ~ NA_character_
    ),

    # Nb enfants < 5 ans (déjà fait mais pas dans modèle)
    nb_enf_f = factor(case_when(
      v137 == 1 ~ "1 enfant",
      v137 == 2 ~ "2 enfants",
      v137 >= 3 ~ "3 enfants et plus",
      TRUE ~ NA_character_
    ), levels = c("1 enfant","2 enfants","3 enfants et plus"))
  )

# Reconstruire d_clean
d_clean <- enfants %>% filter(!is.na(stunting)) %>%
  mutate(stunting = as.numeric(stunting))

# ================================================
# TABLEAUX DE PREVALENCE + TESTS CHI-DEUX
# ================================================

nouvelles <- list(
  "Âge mère cat"         = d_clean$age_mere_cat,
  "Religion CM"          = d_clean$religion_cm,
  "Activité CM"          = d_clean$activite_cm,
  "Sexe CM"              = d_clean$sexe_cm,
  "Rang naissance"       = d_clean$rang_f,
  "Assainissement"       = d_clean$assainissement,
  "Vitamine A"           = d_clean$vitamine_a,
  "Déparasitage"         = d_clean$deparasitage,
  "Allaitement"          = d_clean$allaitement_f,
  "Nb enfants < 5 ans"   = d_clean$nb_enf_f
)

cat("=== PREVALENCES ET TESTS CHI-DEUX ===\n\n")

for (nom in names(nouvelles)) {
  var <- nouvelles[[nom]]
  if (sum(!is.na(var)) < 10) next

  tab  <- table(var, d_clean$stunting)
  prev <- round(prop.table(tab, margin=1)*100, 1)
  test <- chisq.test(var, d_clean$stunting)

  cat(sprintf("--- %s --- p = %.4f %s\n", nom, test$p.value,
              ifelse(test$p.value < 0.05, "✅ SIG","❌ non sig")))
  print(prev)
  cat("\n")
}

# Test t — âge continu de la mère
t_age_mere <- t.test(age_mere ~ stunting, data=d_clean)
w_age_mere <- wilcox.test(age_mere ~ stunting, data=d_clean)
cat(sprintf("Âge mère (continu) — t: p=%.4f | Wilcoxon: p=%.4f %s\n",
            t_age_mere$p.value, w_age_mere$p.value,
            ifelse(w_age_mere$p.value < 0.05, "✅ SIG","❌ non sig")))


# ================================================
# MODELE DEFINITIF ENRICHI — Version finale
# ================================================

# Correction allaitement + rebuild d_clean complet
d_clean <- enfants %>%
  filter(!is.na(stunting)) %>%
  mutate(
    stunting   = as.numeric(stunting),
    richesse_f = factor(richesse, levels = 1:5,
                 labels = c("Très pauvre","Pauvre","Moyen",
                            "Riche","Très riche")),
    instruction  = relevel(instruction, ref = "Aucune"),
    milieu       = relevel(milieu,      ref = "Urbain"),
    sexe         = relevel(sexe,        ref = "Masculin"),

    jumeau_f = factor(ifelse(b0 > 0, 1, 0),
               levels = c(0,1),
               labels = c("Non jumeau","Jumeau")),

    intervalle_f = factor(case_when(
      is.na(b11)           ~ "Premier-né",
      b11 < 24             ~ "Moins de 24 mois",
      b11 >= 24 & b11 < 36 ~ "24-35 mois",
      b11 >= 36            ~ "36 mois et plus",
      TRUE                 ~ NA_character_
    ), levels = c("36 mois et plus","Premier-né",
                  "24-35 mois","Moins de 24 mois")),

    # Assainissement amélioré (OMS)
    assainis_f = factor(case_when(
      v116 %in% c(11,12,13,21,22) ~ "Amélioré",
      v116 %in% c(23,31,42,43,96) ~ "Non amélioré",
      TRUE                         ~ NA_character_
    ), levels = c("Amélioré","Non amélioré")),

    # Rang de naissance
    rang_f = factor(case_when(
      bord %in% c(1,2) ~ "Rang 1-2",
      bord %in% c(3,4) ~ "Rang 3-4",
      bord >= 5        ~ "Rang 5 et plus",
      TRUE             ~ NA_character_
    ), levels = c("Rang 1-2","Rang 3-4","Rang 5 et plus")),

    # Activité chef de ménage
    activite_cm_f = factor(case_when(
      v714 %in% c(1,2,3)   ~ "Agriculture",
      v714 %in% c(4,5)     ~ "Commerce/Autre",
      v714 == 0            ~ "Sans activité",
      v714 %in% c(6,7,8,9) ~ "Commerce/Autre",
      TRUE                  ~ NA_character_
    ), levels = c("Agriculture","Commerce/Autre","Sans activité")),

    # Allaitement — version corrigée
    allaitement_f = factor(case_when(
      is.na(m4)    ~ NA_character_,
      m4 == 0      ~ "Jamais allaité",
      m4 >= 95     ~ NA_character_,  # codes manquants EDS
      m4 < 6       ~ "Moins de 6 mois",
      m4 < 24      ~ "6-23 mois",
      m4 >= 24     ~ "24 mois et plus"
    ), levels = c("Jamais allaité","Moins de 6 mois",
                  "6-23 mois","24 mois et plus"))
  )

# Vérification des nouvelles variables
cat("--- ASSAINISSEMENT ---\n")
tab_ass <- table(d_clean$assainis_f, d_clean$stunting)
print(round(prop.table(tab_ass, margin=1)*100, 1))
cat("Chi-deux p =", round(chisq.test(d_clean$assainis_f,
                                      d_clean$stunting)$p.value, 4), "\n\n")

cat("--- RANG NAISSANCE ---\n")
tab_rang <- table(d_clean$rang_f, d_clean$stunting)
print(round(prop.table(tab_rang, margin=1)*100, 1))
cat("Chi-deux p =", round(chisq.test(d_clean$rang_f,
                                      d_clean$stunting)$p.value, 4), "\n\n")

cat("--- ACTIVITE CM ---\n")
tab_cm <- table(d_clean$activite_cm_f, d_clean$stunting)
print(round(prop.table(tab_cm, margin=1)*100, 1))
cat("Chi-deux p =", round(chisq.test(d_clean$activite_cm_f,
                                      d_clean$stunting)$p.value, 4), "\n\n")

cat("--- ALLAITEMENT ---\n")
tab_all <- table(d_clean$allaitement_f, d_clean$stunting)
print(round(prop.table(tab_all, margin=1)*100, 1))
cat("Chi-deux p =", round(chisq.test(d_clean$allaitement_f,
                                      d_clean$stunting)$p.value, 4), "\n\n")

# ================================================
# DESIGN
# ================================================
design <- svydesign(
  id = ~v001, strata = ~v022,
  weights = ~I(v005/1000000),
  data = d_clean, nest = TRUE
)

# ================================================
# MODELE DEFINITIF ENRICHI
# ================================================
model_DEF <- svyglm(
  stunting ~ sexe + milieu + instruction + richesse_f +
             age_enfant + mere_malnutrie + region +
             jumeau_f + intervalle_f +
             assainis_f + rang_f + activite_cm_f,
  design = design,
  family = quasibinomial(link = "logit")
)

summary(model_DEF)

cat("\n=== ODDS-RATIOS MODELE DEFINITIF ===\n")
round(exp(cbind(OR = coef(model_DEF), confint(model_DEF))), 3)

# ================================================
# VALIDATION MODELE DEFINITIF
# ================================================
vars_DEF <- c("stunting","sexe","milieu","instruction","richesse_f",
              "age_enfant","mere_malnutrie","region",
              "jumeau_f","intervalle_f",
              "assainis_f","rang_f","activite_cm_f")
d_DEF <- na.omit(d_clean[, vars_DEF])
cat("\nN observations modele definitif :", nrow(d_DEF), "\n")

model_DEF_glm <- glm(
  stunting ~ sexe + milieu + instruction + richesse_f +
             age_enfant + mere_malnutrie + region +
             jumeau_f + intervalle_f +
             assainis_f + rang_f + activite_cm_f,
  data = d_DEF, family = binomial
)

library(pROC)
pred_DEF <- predict(model_DEF_glm, type = "response")
roc_DEF  <- roc(d_DEF$stunting, pred_DEF)
cat("AUC modele definitif :", round(auc(roc_DEF), 4), "\n")

library(ResourceSelection)
hl_DEF <- hoslem.test(d_DEF$stunting, fitted(model_DEF_glm), g=10)
cat("Hosmer-Lemeshow p =", round(hl_DEF$p.value, 4), "\n")

library(DescTools)
cat("\nPseudo-R² :\n")
print(round(PseudoR2(model_DEF_glm,
            which = c("McFadden","Nagelkerke","CoxSnell")), 4))

library(margins)
cat("\n=== EFFETS MARGINAUX ===\n")
summary(margins(model_DEF_glm))

# Seuil optimal
opt <- coords(roc_DEF, "best",
              ret = c("threshold","sensitivity","specificity"))
cat("\nSeuil optimal :", round(opt$threshold, 3))
cat("\nSensibilité   :", round(opt$sensitivity*100, 1), "%")
cat("\nSpécificité   :", round(opt$specificity*100, 1), "%\n")





# ================================================
# ELEMENTS FINAUX POUR LA REDACTION
# ================================================

# Recharger le model_FINAL proprement
d_clean <- enfants %>%
  filter(!is.na(stunting)) %>%
  mutate(
    stunting   = as.numeric(stunting),
    richesse_f = factor(richesse, levels = 1:5,
                 labels = c("Très pauvre","Pauvre","Moyen",
                            "Riche","Très riche")),
    instruction  = relevel(instruction, ref = "Aucune"),
    milieu       = relevel(milieu,      ref = "Urbain"),
    sexe         = relevel(sexe,        ref = "Masculin"),
    jumeau_f = factor(ifelse(b0 > 0, 1, 0),
               levels = c(0,1),
               labels = c("Non jumeau","Jumeau")),
    intervalle_f = factor(case_when(
      is.na(b11)           ~ "Premier-né",
      b11 < 24             ~ "Moins de 24 mois",
      b11 >= 24 & b11 < 36 ~ "24-35 mois",
      b11 >= 36            ~ "36 mois et plus",
      TRUE                 ~ NA_character_
    ), levels = c("36 mois et plus","Premier-né",
                  "24-35 mois","Moins de 24 mois"))
  )

design <- svydesign(
  id = ~v001, strata = ~v022,
  weights = ~I(v005/1000000),
  data = d_clean, nest = TRUE
)

model_FINAL <- svyglm(
  stunting ~ sexe + milieu + instruction + richesse_f +
             age_enfant + mere_malnutrie + region +
             jumeau_f + intervalle_f,
  design = design,
  family = quasibinomial(link = "logit")
)

vars_FINAL <- c("stunting","sexe","milieu","instruction","richesse_f",
                "age_enfant","mere_malnutrie","region",
                "jumeau_f","intervalle_f")
d_FINAL <- na.omit(d_clean[, vars_FINAL])

model_FINAL_glm <- glm(
  stunting ~ sexe + milieu + instruction + richesse_f +
             age_enfant + mere_malnutrie + region +
             jumeau_f + intervalle_f,
  data = d_FINAL, family = binomial
)

# ================================================
# 1. EFFETS MARGINAUX CORRIGES
# ================================================
library(margins)
cat("=== EFFETS MARGINAUX FINAUX ===\n")
marg <- summary(margins(model_FINAL_glm))
print(marg)

# ================================================
# 2. FOREST PLOT FINAL AVEC BONS OR
# ================================================
library(ggplot2)
library(tibble)

or_final <- tibble(
  Variable = c(
    "Sexe féminin",
    "Milieu rural",
    "Instruction primaire",
    "Instruction secondaire+",
    "Richesse pauvre",
    "Richesse moyen",
    "Richesse riche",
    "Richesse très riche",
    "Âge enfant (mois)",
    "Malnutrition mère",
    "Jumeau",
    "Premier-né",
    "Intervalle 24-35 mois",
    "Intervalle < 24 mois",
    "Sahel (région 12)"
  ),
  OR    = c(0.732, 1.450, 0.711, 0.877, 1.006, 0.799, 0.797,
            0.615, 1.006, 1.177, 2.664, 1.304, 1.412, 1.570, 1.731),
  lower = c(0.639, 1.086, 0.552, 0.682, 0.796, 0.629, 0.618,
            0.409, 1.002, 0.916, 1.821, 1.057, 1.180, 1.203, 0.860),
  upper = c(0.839, 1.935, 0.915, 1.127, 1.272, 1.015, 1.026,
            0.924, 1.009, 1.511, 3.898, 1.608, 1.690, 2.049, 3.483),
  Sig   = c("Oui","Oui","Oui","Non","Non","Non","Non",
            "Oui","Oui","Non","Oui","Oui","Oui","Oui","Limite")
)

g_forest <- ggplot(or_final,
                   aes(x = OR,
                       y = reorder(Variable, OR),
                       color = Sig)) +
  geom_point(size = 4) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.3, linewidth = 0.8,
                orientation = "y") +
  geom_vline(xintercept = 1, linetype = "dashed",
             color = "gray40", linewidth = 0.8) +
  scale_color_manual(
    values = c("Oui"="#1F5C99","Non"="gray60","Limite"="#E67E22")
  ) +
  labs(
    title    = "Odds-ratios ajustés — Déterminants du retard de croissance",
    subtitle = "Modèle logistique avec plan de sondage — Burkina Faso, EDS 2021",
    x        = "Odds Ratio (IC 95%)",
    y        = NULL,
    color    = "Significatif (p < 0,05)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", color = "#1F5C99"),
    plot.subtitle = element_text(color = "gray50")
  )

ggsave("04_Resultats/G7_forest_plot_FINAL.png", g_forest,
       width = 12, height = 8, dpi = 300, bg = "white")
cat("Forest plot final sauvegardé\n")

# ================================================
# 3. COURBE ROC FINALE PROPRE
# ================================================
library(pROC)
pred_FINAL <- predict(model_FINAL_glm, type = "response")
roc_FINAL  <- roc(d_FINAL$stunting, pred_FINAL)

png("04_Resultats/G8_ROC_finale.png",
    width = 2400, height = 2000, res = 300)
plot(roc_FINAL,
     main = "Courbe ROC — Modèle final des déterminants\ndu retard de croissance, Burkina Faso EDS 2021",
     col  = "#1F5C99", lwd = 2.5,
     print.auc = TRUE,
     print.auc.x = 0.45, print.auc.y = 0.12,
     auc.polygon = TRUE,
     auc.polygon.col = "#D6E4F0",
     grid = TRUE)
abline(a = 0, b = 1, lty = 2, col = "gray60", lwd = 1.5)
dev.off()
cat("Courbe ROC finale sauvegardée\n")

# ================================================
# 4. TABLEAU RECAPITULATIF FINAL
# ================================================
cat("\n=== RECAPITULATIF COMPLET MODELE FINAL ===\n")
cat(sprintf("N observations      : %d\n", nrow(d_FINAL)))
cat(sprintf("AUC                 : %.4f\n", auc(roc_FINAL)))
cat(sprintf("Hosmer-Lemeshow p   : %.4f\n",
            hoslem.test(d_FINAL$stunting,
                        fitted(model_FINAL_glm), g=10)$p.value))
library(DescTools)
pr2 <- PseudoR2(model_FINAL_glm,
                which = c("McFadden","Nagelkerke","CoxSnell"))
cat(sprintf("McFadden R²         : %.4f\n", pr2["McFadden"]))
cat(sprintf("Nagelkerke R²       : %.4f\n", pr2["Nagelkerke"]))
opt <- coords(roc_FINAL, "best",
              ret = c("threshold","sensitivity","specificity"))
cat(sprintf("Seuil optimal       : %.3f\n", opt$threshold))
cat(sprintf("Sensibilité         : %.1f%%\n", opt$sensitivity*100))
cat(sprintf("Spécificité         : %.1f%%\n", opt$specificity*100))

cat("\n=== VARIABLES SIGNIFICATIVES ===\n")
or_sig <- or_final[or_final$Sig == "Oui",
                   c("Variable","OR","lower","upper")]
print(or_sig)




library(pROC)
library(dplyr)

# 1. Préparer une base complète avec les variables du modèle + v001 et v005
vars_modele <- c("stunting", "sexe", "milieu", "instruction", "richesse_f",
                 "age_enfant", "mere_malnutrie", "region", "jumeau_f",
                 "intervalle_f", "v001", "v005")

d_boot <- d_clean[complete.cases(d_clean[, vars_modele]), ]

# 2. Fonction pour un bootstrap sur les grappes
boot_auc_clusters <- function(data) {
  # Rééchantillonner les grappes avec remise
  clusters <- unique(data$v001)
  boot_clusters <- sample(clusters, size = length(clusters), replace = TRUE)
  
  # Construire l'échantillon bootstrap (on duplique les lignes des grappes choisies)
  boot_data <- data[data$v001 %in% boot_clusters, ]
  
  # Modèle logistique avec pondérations
  model_boot <- glm(stunting ~ sexe + milieu + instruction + richesse_f + 
                    age_enfant + mere_malnutrie + region + jumeau_f + intervalle_f,
                    data = boot_data,
                    family = binomial,
                    weights = v005 / 1000000)
  
  # Prédictions et AUC
  pred_boot <- predict(model_boot, type = "response")
  roc_boot <- roc(boot_data$stunting, pred_boot, quiet = TRUE)
  return(as.numeric(auc(roc_boot)))
}

# 3. Lancer le bootstrap (100 réplicats)
set.seed(123)
B <- 100
auc_boot <- replicate(B, boot_auc_clusters(d_boot))

# 4. Résultats
mean_auc <- mean(auc_boot)
ci_auc <- quantile(auc_boot, c(0.025, 0.975))

cat("AUC moyenne (bootstrap) :", round(mean_auc, 4), "\n")
cat("IC 95 % :", round(ci_auc[1], 4), "–", round(ci_auc[2], 4), "\n")




# Test final — nb_enf_f dans le modèle
d_clean <- d_clean %>%
  mutate(
    nb_enf_f = factor(case_when(
      v137 == 1 ~ "1 enfant",
      v137 == 2 ~ "2 enfants",
      v137 >= 3 ~ "3 enfants et plus",
      TRUE ~ NA_character_
    ), levels = c("1 enfant","2 enfants","3 enfants et plus"))
  )

design <- svydesign(
  id = ~v001, strata = ~v022,
  weights = ~I(v005/1000000),
  data = d_clean, nest = TRUE
)

model_TEST <- svyglm(
  stunting ~ sexe + milieu + instruction + richesse_f +
             age_enfant + mere_malnutrie + region +
             jumeau_f + intervalle_f + nb_enf_f,
  design = design,
  family = quasibinomial(link = "logit")
)

summary(model_TEST)


model_memoir_final <- svyglm(
	stunting ~ sexe + milieu + instruction + richesse_f +
             age_enfant + mere_malnutrie + region +
             jumeau_f + intervalle_f,
	design = design,
	family = quasibinomial (link = "logit")
)
summary(model_memoir_final)




# Ajouter les deux variables dans d_clean
d_clean <- d_clean %>%
  mutate(
    # Activité économique de la mère (v717)
    activite_mere_f = factor(case_when(
      v717 == 0            ~ "Sans activité",
      v717 %in% c(1,2,3)   ~ "Agriculture",
      v717 %in% c(4,5)     ~ "Commerce",
      v717 %in% c(6,7,8,9) ~ "Autres activités",
      TRUE                  ~ NA_character_
    ), levels = c("Agriculture","Commerce",
                  "Sans activité","Autres activités")),

    # Activité économique du chef de ménage (v714)
    activite_cm_f = factor(case_when(
      v714 %in% c(1,2,3)   ~ "Agriculture",
      v714 %in% c(4,5)     ~ "Commerce/Autre",
      v714 == 0            ~ "Sans activité",
      v714 %in% c(6,7,8,9) ~ "Commerce/Autre",
      TRUE                  ~ NA_character_
    ), levels = c("Agriculture","Commerce/Autre","Sans activité")),

    # Eau améliorée (déjà créée mais à vérifier)
    eau_bin = factor(case_when(
      eau_amelioree == 1 ~ "Améliorée",
      eau_amelioree == 0 ~ "Non améliorée",
      TRUE               ~ NA_character_
    ), levels = c("Améliorée","Non améliorée")),

    # Rang de naissance
    rang_f = factor(case_when(
      bord %in% c(1,2) ~ "Rang 1-2",
      bord %in% c(3,4) ~ "Rang 3-4",
      bord >= 5        ~ "Rang 5 et plus",
      TRUE             ~ NA_character_
    ), levels = c("Rang 1-2","Rang 3-4","Rang 5 et plus")),

    # Nombre d'enfants < 5 ans
    nb_enf_f = factor(case_when(
      v137 == 1 ~ "1 enfant",
      v137 == 2 ~ "2 enfants",
      v137 >= 3 ~ "3 enfants et plus",
      TRUE      ~ NA_character_
    ), levels = c("1 enfant","2 enfants","3 enfants et plus"))
  )

# Vérifications rapides
cat("Activité mère :\n")
print(round(prop.table(
  table(d_clean$activite_mere_f, d_clean$stunting),
  margin=1)*100, 1))

cat("\nActivité CM :\n")
print(round(prop.table(
  table(d_clean$activite_cm_f, d_clean$stunting),
  margin=1)*100, 1))

cat("\nEau :\n")
print(round(prop.table(
  table(d_clean$eau_bin, d_clean$stunting),
  margin=1)*100, 1))

cat("\nRang :\n")
print(round(prop.table(
  table(d_clean$rang_f, d_clean$stunting),
  margin=1)*100, 1))

cat("\nNb enfants :\n")
print(round(prop.table(
  table(d_clean$nb_enf_f, d_clean$stunting),
  margin=1)*100, 1))

# Graphiques
library(ggplot2)
col_normal  <- "#2E86C1"
col_stunted <- "#E74C3C"

# G — Activité mère
act_m <- d_clean %>%
  filter(!is.na(activite_mere_f)) %>%
  group_by(activite_mere_f) %>%
  summarise(prev = round(mean(stunting, na.rm=TRUE)*100, 1))

g_act_m <- ggplot(act_m,
    aes(x=reorder(activite_mere_f,-prev), y=prev,
        fill=activite_mere_f)) +
  geom_bar(stat="identity", color="white", width=0.6) +
  geom_text(aes(label=paste0(prev,"%")),
            vjust=-0.5, fontface="bold", size=4.5) +
  scale_fill_manual(values=c(
    "Agriculture"     ="#2E86C1",
    "Autres activités"="#5DADE2",
    "Commerce"        ="#E67E22",
    "Sans activité"   ="#E74C3C")) +
  labs(title="Prévalence du retard de croissance selon\nl'activité économique de la mère",
       subtitle="Burkina Faso, EDS 2021",
       x=NULL, y="Prévalence (%)", fill=NULL) +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(face="bold",color="#1F5C99"),
        plot.subtitle=element_text(color="gray50"),
        legend.position="none")

ggsave("04_Resultats/G_activite_mere.png", g_act_m,
       width=8, height=6, dpi=300, bg="white")

# G — Activité CM
act_cm <- d_clean %>%
  filter(!is.na(activite_cm_f)) %>%
  group_by(activite_cm_f) %>%
  summarise(prev = round(mean(stunting, na.rm=TRUE)*100, 1))

g_act_cm <- ggplot(act_cm,
    aes(x=reorder(activite_cm_f,-prev), y=prev,
        fill=activite_cm_f)) +
  geom_bar(stat="identity", color="white", width=0.6) +
  geom_text(aes(label=paste0(prev,"%")),
            vjust=-0.5, fontface="bold", size=4.5) +
  scale_fill_manual(values=c(
    "Agriculture"   ="#2E86C1",
    "Commerce/Autre"="#E67E22",
    "Sans activité" ="#E74C3C")) +
  labs(title="Prévalence du retard de croissance selon\nl'activité économique du chef de ménage",
       subtitle="Burkina Faso, EDS 2021",
       x=NULL, y="Prévalence (%)", fill=NULL) +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(face="bold",color="#1F5C99"),
        plot.subtitle=element_text(color="gray50"),
        legend.position="none")

ggsave("04_Resultats/G_activite_cm.png", g_act_cm,
       width=8, height=6, dpi=300, bg="white")

# G — Eau
eau_d <- d_clean %>%
  filter(!is.na(eau_bin)) %>%
  group_by(eau_bin) %>%
  summarise(prev = round(mean(stunting, na.rm=TRUE)*100, 1))

g_eau <- ggplot(eau_d, aes(x=eau_bin, y=prev, fill=eau_bin)) +
  geom_bar(stat="identity", color="white", width=0.5) +
  geom_text(aes(label=paste0(prev,"%")),
            vjust=-0.5, fontface="bold", size=5) +
  scale_fill_manual(values=c("Améliorée"=col_normal,
                             "Non améliorée"=col_stunted)) +
  labs(title="Prévalence du retard de croissance selon\nl'accès à l'eau potable",
       subtitle="Burkina Faso, EDS 2021",
       x=NULL, y="Prévalence (%)", fill=NULL) +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(face="bold",color="#1F5C99"),
        plot.subtitle=element_text(color="gray50"),
        legend.position="none")

ggsave("04_Resultats/G_eau_amelioree.png", g_eau,
       width=7, height=6, dpi=300, bg="white")

# G — Rang naissance
rang_d <- d_clean %>%
  filter(!is.na(rang_f)) %>%
  group_by(rang_f) %>%
  summarise(prev = round(mean(stunting, na.rm=TRUE)*100, 1))

g_rang <- ggplot(rang_d, aes(x=rang_f, y=prev, fill=prev)) +
  geom_bar(stat="identity", color="white", width=0.6) +
  geom_text(aes(label=paste0(prev,"%")),
            vjust=-0.5, fontface="bold", size=4.5) +
  scale_fill_gradient(low="#AED6F1", high=col_stunted) +
  labs(title="Prévalence du retard de croissance selon\nle rang de naissance",
       subtitle="Burkina Faso, EDS 2021",
       x="Rang de naissance", y="Prévalence (%)",
       fill="Prévalence (%)") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(face="bold",color="#1F5C99"),
        plot.subtitle=element_text(color="gray50"))

ggsave("04_Resultats/G_rang_naissance.png", g_rang,
       width=8, height=6, dpi=300, bg="white")

# G — Nb enfants
nb_d <- d_clean %>%
  filter(!is.na(nb_enf_f)) %>%
  group_by(nb_enf_f) %>%
  summarise(prev = round(mean(stunting, na.rm=TRUE)*100, 1))

g_nb <- ggplot(nb_d, aes(x=nb_enf_f, y=prev, fill=prev)) +
  geom_bar(stat="identity", color="white", width=0.6) +
  geom_text(aes(label=paste0(prev,"%")),
            vjust=-0.5, fontface="bold", size=4.5) +
  scale_fill_gradient(low="#A9DFBF", high=col_stunted) +
  labs(title="Prévalence selon le nombre d'enfants\nde moins de 5 ans dans le ménage",
       subtitle="Burkina Faso, EDS 2021",
       x="Nombre d'enfants < 5 ans",
       y="Prévalence (%)", fill="Prévalence (%)") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(face="bold",color="#1F5C99"),
        plot.subtitle=element_text(color="gray50"))

ggsave("04_Resultats/G_nb_enfants.png", g_nb,
       width=8, height=6, dpi=300, bg="white")

cat("\n=== 5 GRAPHIQUES PRODUITS ===\n")



# Coefficients exacts du modèle final
cat("=== COEFFICIENTS MODELE FINAL ===\n")
print(round(coef(model_FINAL), 6))
bs
cat("\n=== VERIFICATION INTERCEPT ===\n")
cat("Intercept :", round(coef(model_FINAL)[1], 6), "\n")



cat("=== OR AJUSTES FINAUX ===\n")
round(exp(cbind(OR = coef(model_FINAL),
               confint(model_FINAL))), 3)


