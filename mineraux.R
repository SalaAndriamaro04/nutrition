# Charger les bibliothèques
library(readxl)
library(multcompView)
library(writexl)

# Charger les données
data <- read_excel("~/Essai/Mineraux.xlsx")

# Calcul de l'écart-type pour chaque paramètre et chaque variété
calculate_sd <- function(essai1, essai2) {
  return(sd(c(essai1, essai2)))
}
# Appliquer la fonction sur chaque lentille et ajouter une colonne d'écart-type pour chaque paramètre
data$SD_K067 <- mapply(calculate_sd, data$Essai1_K067_Lentille_verte, data$Essai2_K067_Lentille_verte);
data$SD_K068 <- mapply(calculate_sd, data$Essai1_K068_Lentille_corail, data$Essai2_K068_Lentille_corail);
data$SD_K069 <- mapply(calculate_sd, data$Essai1_K069_Lentille_noire, data$Essai2_K069_Lentille_noire);

# Afficher le tableau avec moyenne et écart-type
data$Lentille_verte <- paste(round(data$Moyenne_K067_Lentille_verte,4), "±",round(data$SD_K067,2))
data$Lentille_corail <- paste(round(data$Moyenne_K068_Lentille_corail,4), "±", round(data$SD_K068,2))
data$Lentille_noire <- paste(round(data$Moyenne_K069_Lentille_noire,4), "±",round(data$SD_K069,2))

# Extraction des données et calculs pour chaque paramètre
Variete <- factor(rep(c("K067", "K068", "K069"), each = 2))

###################################
# Potassium
Potassium <- c(data$Essai1_K067_Lentille_verte[1], data$Essai2_K067_Lentille_verte[1],
               data$Essai1_K068_Lentille_corail[1], data$Essai2_K068_Lentille_corail[1],
               data$Essai1_K069_Lentille_noire[1], data$Essai2_K069_Lentille_noire[1])

dataaov <- data.frame(Variete, Potassium)

# ANOVA pour Potassium
myaov <- aov(Potassium ~ Variete, data = dataaov)
summary(myaov)

# Test post-hoc Tukey pour Potassium
tukey_result <- TukeyHSD(myaov)
print(tukey_result)

# Extraction des lettres pour Potassium
tukey_pvalues <- tukey_result$Variete[, "p adj"]
letters_potassium <- multcompLetters(tukey_pvalues, compare = "<", threshold = 0.05)$Letters
print(letters_potassium)

###################################

# Magnesium
Magnesium <- c(data$Essai1_K067_Lentille_verte[2], data$Essai2_K067_Lentille_verte[2],
             data$Essai1_K068_Lentille_corail[2], data$Essai2_K068_Lentille_corail[2],
             data$Essai1_K069_Lentille_noire[2], data$Essai2_K069_Lentille_noire[2])

dataaov <- data.frame(Variete, Magnesium)

# ANOVA pour Magnesium
myaov <- aov(Magnesium ~ Variete, data = dataaov)
summary(myaov)

# Test post-hoc Tukey pour magnesium
tukey_result <- TukeyHSD(myaov)
print(tukey_result)

# Extraction des lettres pour magnesium
tukey_pvalues <- tukey_result$Variete[, "p adj"]
letters_magnesium <- multcompLetters(tukey_pvalues, compare = "<", threshold = 0.05)$Letters
print(letters_magnesium)

###################################
# Fer
Fer <- c(data$Essai1_K067_Lentille_verte[3], data$Essai2_K067_Lentille_verte[3],
             data$Essai1_K068_Lentille_corail[3], data$Essai2_K068_Lentille_corail[3],
             data$Essai1_K069_Lentille_noire[3], data$Essai2_K069_Lentille_noire[3])

dataaov <- data.frame(Variete, Fer)

# ANOVA pour Fer
myaov <- aov(Fer ~ Variete, data = dataaov)
summary(myaov)

# Test post-hoc Tukey pour Fer
tukey_result <- TukeyHSD(myaov)
print(tukey_result)

# Extraction des lettres pour Fer
tukey_pvalues <- tukey_result$Variete[, "p adj"]
letters_fer <- multcompLetters(tukey_pvalues, compare = "<", threshold = 0.05)$Letters
print(letters_fer)

###################################
Zinc <- c(data$Essai1_K067_Lentille_verte[4], data$Essai2_K067_Lentille_verte[4],
              data$Essai1_K068_Lentille_corail[4], data$Essai2_K068_Lentille_corail[4],
              data$Essai1_K069_Lentille_noire[4], data$Essai2_K069_Lentille_noire[4]
)
dataaov <- data.frame(Variete, Zinc)

# ANOVA
myaov <- aov(Zinc ~ Variete, data = dataaov)
summary(myaov)

# Test post-hoc Tukey HSD
tukey_result <- TukeyHSD(myaov)
print(tukey_result)
# Extraction des lettres pour Zinc
tukey_pvalues <- tukey_result$Variete[, "p adj"]
letters_zinc <- multcompLetters(tukey_pvalues, compare = "<", threshold = 0.05)$Letters
print(letters_zinc)

################################
Calcium <- c(data$Essai1_K067_Lentille_verte[5], data$Essai2_K067_Lentille_verte[5],
              data$Essai1_K068_Lentille_corail[5], data$Essai2_K068_Lentille_corail[5],
              data$Essai1_K069_Lentille_noire[5], data$Essai2_K069_Lentille_noire[5]
)

dataaov <- data.frame(Variete, Calcium)

# ANOVA
myaov <- aov(Calcium ~ Variete, data = dataaov)
summary(myaov)

# Test post-hoc Tukey HSD
tukey_result <- TukeyHSD(myaov)
print(tukey_result)

# Extraction des lettres pour Calcium
tukey_pvalues <- tukey_result$Variete[, "p adj"]
letters_calcium <- multcompLetters(tukey_pvalues, compare = "<", threshold = 0.05)$Letters
print(letters_calcium)

################################
Phosphore <- c(data$Essai1_K067_Lentille_verte[6], data$Essai2_K067_Lentille_verte[6],
                         data$Essai1_K068_Lentille_corail[6], data$Essai2_K068_Lentille_corail[6],
                         data$Essai1_K069_Lentille_noire[6], data$Essai2_K069_Lentille_noire[6]
)

dataaov <- data.frame(Variete, Phosphore)

# ANOVA
myaov <- aov(Phosphore ~ Variete, data = dataaov)
summary(myaov)

# Test post-hoc Tukey HSD
tukey_result <- TukeyHSD(myaov)
print(tukey_result)

# Extraction des lettres pour Phosphore
tukey_pvalues <- tukey_result$Variete[, "p adj"]
letters_phosphore <- multcompLetters(tukey_pvalues, compare = "<", threshold = 0.05)$Letters
print(letters_phosphore)

print(letters_potassium)
print(letters_magnesium)
print(letters_fer)
print(letters_zinc)
print(letters_calcium)
print(letters_phosphore)

data$Lentille_verte[1] <- paste0(data$Lentille_verte[1],letters_potassium["K067"])
data$Lentille_corail[1] <- paste0(data$Lentille_corail[1],letters_potassium["K068"])
data$Lentille_noire[1] <- paste0(data$Lentille_noire[1],letters_potassium["K069"])

data$Lentille_verte[2] <- paste0(data$Lentille_verte[2],letters_magnesium["K067"])
data$Lentille_corail[2] <- paste0(data$Lentille_corail[2],letters_magnesium["K068"])
data$Lentille_noire[2] <- paste0(data$Lentille_noire[2],letters_magnesium["K069"])

data$Lentille_verte[3] <- paste0(data$Lentille_verte[3],letters_fer["K067"])
data$Lentille_corail[3] <- paste0(data$Lentille_corail[3],letters_fer["K068"])
data$Lentille_noire[3] <- paste0(data$Lentille_noire[3],letters_fer["K069"])

data$Lentille_verte[4] <- paste0(data$Lentille_verte[4],letters_zinc["K067"])
data$Lentille_corail[4] <- paste0(data$Lentille_corail[4],letters_zinc["K068"])
data$Lentille_noire[4] <- paste0(data$Lentille_noire[4],letters_zinc["K069"])

data$Lentille_verte[5] <- paste0(data$Lentille_verte[5],letters_calcium["K067"])
data$Lentille_corail[5] <- paste0(data$Lentille_corail[5],letters_calcium["K068"])
data$Lentille_noire[5] <- paste0(data$Lentille_noire[5],letters_calcium["K069"])

data$Lentille_verte[6] <- paste0(data$Lentille_verte[6],letters_phosphore["K067"])
data$Lentille_corail[6] <- paste0(data$Lentille_corail[6],letters_phosphore["K068"])
data$Lentille_noire[6] <- paste0(data$Lentille_noire[6],letters_phosphore["K069"])

# Afficher les données avec les écarts-types
data_final <- data[, c("PARAMETRES","Lentille_verte", "Lentille_corail", "Lentille_noire")]

View(data_final)
# Exporter le tableau des résultats
write_xlsx(data_final, "resultats_mineraux.xlsx")
