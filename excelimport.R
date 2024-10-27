# Charger les bibliothèques
library(readxl)
library(multcompView)
library(writexl)

# Charger les données
data <- read_excel("~/Essai/Macronutriments.xlsx")

# Calcul de l'écart-type pour chaque paramètre et chaque variété
calculate_sd <- function(essai1, essai2) {
  return(sd(c(essai1, essai2)))
}
# Appliquer la fonction sur chaque lentille et ajouter une colonne d'écart-type pour chaque paramètre
data$SD_K067 <- mapply(calculate_sd, data$Essai1_K067_Lentille_verte, data$Essai2_K067_Lentille_verte);
data$SD_K068 <- mapply(calculate_sd, data$Essai1_K068_Lentille_corail, data$Essai2_K068_Lentille_corail);
data$SD_K069 <- mapply(calculate_sd, data$Essai1_K069_Lentille_noire, data$Essai2_K069_Lentille_noire);

# Afficher le tableau avec moyenne et écart-type
data$Lentille_verte <- paste(round(data$Moyenne_K067_Lentille_verte,2), "±",round(data$SD_K067,2))
data$Lentille_corail <- paste(round(data$Moyenne_K068_Lentille_corail,2), "±", round(data$SD_K068,2))
data$Lentille_noire <- paste(round(data$Moyenne_K069_Lentille_noire,2), "±",round(data$SD_K069,2))

# Extraction des données et calculs pour chaque paramètre
Variete <- factor(rep(c("K067", "K068", "K069"), each = 2))

###################################
# Protéines
Proteines <- c(data$Essai1_K067_Lentille_verte[1], data$Essai2_K067_Lentille_verte[1],
               data$Essai1_K068_Lentille_corail[1], data$Essai2_K068_Lentille_corail[1],
               data$Essai1_K069_Lentille_noire[1], data$Essai2_K069_Lentille_noire[1])

dataaov <- data.frame(Variete, Proteines)

# ANOVA pour Protéines
myaov <- aov(Proteines ~ Variete, data = dataaov)
summary(myaov)

# Test post-hoc Tukey pour Protéines
tukey_result <- TukeyHSD(myaov)
print(tukey_result)

# Extraction des lettres pour Protéines
tukey_pvalues <- tukey_result$Variete[, "p adj"]
letters_proteines <- multcompLetters(tukey_pvalues, compare = "<", threshold = 0.05)$Letters
print(letters_proteines)

###################################
# Répéter le même processus pour les Lipides et les Cendres

# Lipides
Lipides <- c(data$Essai1_K067_Lentille_verte[2], data$Essai2_K067_Lentille_verte[2],
             data$Essai1_K068_Lentille_corail[2], data$Essai2_K068_Lentille_corail[2],
             data$Essai1_K069_Lentille_noire[2], data$Essai2_K069_Lentille_noire[2])

dataaov <- data.frame(Variete, Lipides)

# ANOVA pour Lipides
myaov <- aov(Lipides ~ Variete, data = dataaov)
summary(myaov)

# Test post-hoc Tukey pour Lipides
tukey_result <- TukeyHSD(myaov)
print(tukey_result)

# Extraction des lettres pour Lipides
tukey_pvalues <- tukey_result$Variete[, "p adj"]
letters_lipides <- multcompLetters(tukey_pvalues, compare = "<", threshold = 0.05)$Letters
print(letters_lipides)

###################################
# Cendres
Cendres <- c(data$Essai1_K067_Lentille_verte[3], data$Essai2_K067_Lentille_verte[3],
             data$Essai1_K068_Lentille_corail[3], data$Essai2_K068_Lentille_corail[3],
             data$Essai1_K069_Lentille_noire[3], data$Essai2_K069_Lentille_noire[3])

dataaov <- data.frame(Variete, Cendres)

# ANOVA pour Cendres
myaov <- aov(Cendres ~ Variete, data = dataaov)
summary(myaov)

# Test post-hoc Tukey pour Cendres
tukey_result <- TukeyHSD(myaov)
print(tukey_result)

# Extraction des lettres pour Cendres
tukey_pvalues <- tukey_result$Variete[, "p adj"]
letters_cendres <- multcompLetters(tukey_pvalues, compare = "<", threshold = 0.05)$Letters
print(letters_cendres)

###################################
Humidite <- c(data$Essai1_K067_Lentille_verte[4], data$Essai2_K067_Lentille_verte[4],
              data$Essai1_K068_Lentille_corail[4], data$Essai2_K068_Lentille_corail[4],
              data$Essai1_K069_Lentille_noire[4], data$Essai2_K069_Lentille_noire[4]
)
dataaov <- data.frame(Variete, Humidite)

# ANOVA
myaov <- aov(Humidite ~ Variete, data = dataaov)
summary(myaov)

# Test post-hoc Tukey HSD
tukey_result <- TukeyHSD(myaov)
print(tukey_result)
# Extraction des lettres pour Humidite
tukey_pvalues <- tukey_result$Variete[, "p adj"]
letters_humidite <- multcompLetters(tukey_pvalues, compare = "<", threshold = 0.05)$Letters
print(letters_humidite)

################################
Glucides <- c(data$Essai1_K067_Lentille_verte[5], data$Essai2_K067_Lentille_verte[5],
              data$Essai1_K068_Lentille_corail[5], data$Essai2_K068_Lentille_corail[5],
              data$Essai1_K069_Lentille_noire[5], data$Essai2_K069_Lentille_noire[5]
)

dataaov <- data.frame(Variete, Glucides)

# ANOVA
myaov <- aov(Glucides ~ Variete, data = dataaov)
summary(myaov)

# Test post-hoc Tukey HSD
tukey_result <- TukeyHSD(myaov)
print(tukey_result)

# Extraction des lettres pour Glucides
tukey_pvalues <- tukey_result$Variete[, "p adj"]
letters_glucides <- multcompLetters(tukey_pvalues, compare = "<", threshold = 0.05)$Letters
print(letters_glucides)

################################
Valeurs_energetique <- c(data$Essai1_K067_Lentille_verte[6], data$Essai2_K067_Lentille_verte[6],
                         data$Essai1_K068_Lentille_corail[6], data$Essai2_K068_Lentille_corail[6],
                         data$Essai1_K069_Lentille_noire[6], data$Essai2_K069_Lentille_noire[6]
)

dataaov <- data.frame(Variete, Valeurs_energetique)

# ANOVA
myaov <- aov(Valeurs_energetique ~ Variete, data = dataaov)
summary(myaov)

# Test post-hoc Tukey HSD
tukey_result <- TukeyHSD(myaov)
print(tukey_result)

# Extraction des lettres pour Glucides
tukey_pvalues <- tukey_result$Variete[, "p adj"]
letters_valeurs_energetique <- multcompLetters(tukey_pvalues, compare = "<", threshold = 0.05)$Letters
print(letters_valeurs_energetique)

print(letters_proteines)
print(letters_lipides)
print(letters_cendres)
print(letters_humidite)
print(letters_glucides)
print(letters_valeurs_energetique)

data$Lentille_verte[1] <- paste0(data$Lentille_verte[1],letters_proteines["K067"])
data$Lentille_corail[1] <- paste0(data$Lentille_corail[1],letters_proteines["K068"])
data$Lentille_noire[1] <- paste0(data$Lentille_noire[1],letters_proteines["K069"])

data$Lentille_verte[2] <- paste0(data$Lentille_verte[2],letters_lipides["K067"])
data$Lentille_corail[2] <- paste0(data$Lentille_corail[2],letters_lipides["K068"])
data$Lentille_noire[2] <- paste0(data$Lentille_noire[2],letters_lipides["K069"])

data$Lentille_verte[3] <- paste0(data$Lentille_verte[3],letters_cendres["K067"])
data$Lentille_corail[3] <- paste0(data$Lentille_corail[3],letters_cendres["K068"])
data$Lentille_noire[3] <- paste0(data$Lentille_noire[3],letters_cendres["K069"])

data$Lentille_verte[4] <- paste0(data$Lentille_verte[4],letters_humidite["K067"])
data$Lentille_corail[4] <- paste0(data$Lentille_corail[4],letters_humidite["K068"])
data$Lentille_noire[4] <- paste0(data$Lentille_noire[4],letters_humidite["K069"])

data$Lentille_verte[5] <- paste0(data$Lentille_verte[5],letters_glucides["K067"])
data$Lentille_corail[5] <- paste0(data$Lentille_corail[5],letters_glucides["K068"])
data$Lentille_noire[5] <- paste0(data$Lentille_noire[5],letters_glucides["K069"])

data$Lentille_verte[6] <- paste0(data$Lentille_verte[6],letters_valeurs_energetique["K067"])
data$Lentille_corail[6] <- paste0(data$Lentille_corail[6],letters_valeurs_energetique["K068"])
data$Lentille_noire[6] <- paste0(data$Lentille_noire[6],letters_valeurs_energetique["K069"])

# Afficher les données avec les écarts-types
data_final <- data[, c("PARAMETRES","Lentille_verte", "Lentille_corail", "Lentille_noire")]

#View(data_final)
# Exporter le tableau des résultats
write_xlsx(data_final, "resultats_macronutriments.xlsx")
