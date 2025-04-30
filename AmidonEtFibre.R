# Charger les bibliothèques
library(readxl)
library(multcompView)
library(writexl)

# Charger les données | nutrition path change en cas de changement de dossier
data <- read_excel("~/nutrition/src_excel/AmidonEtFibre.xlsx")
#View(data)

# Calcul de l'écart-type pour chaque paramètre et chaque variété
calculate_sd <- function(essai1, essai2) {
  return(sd(c(essai1, essai2)))
}

# Appliquer la fonction sur chaque lentille et ajouter une colonne d'écart-type pour chaque paramètre
data$SD_K067 <- mapply(calculate_sd, data$essai1_lentille_verte, data$essai2_lentille_verte);
data$SD_K068 <- mapply(calculate_sd, data$essai1_lentille_corail, data$essai2_lentille_corail);
data$SD_K069 <- mapply(calculate_sd, data$essai1_lentille_noire, data$essai2_lentille_noire);

# Afficher le tableau avec moyenne et écart-type
data$Lentille_verte <- paste(round(data$moyenne_lentille_verte,2), "±",round(data$SD_K067,2))
data$Lentille_corail <- paste(round(data$moyenne_lentille_corail,2), "±", round(data$SD_K068,2))
data$Lentille_noire <- paste(round(data$moyenne_lentille_noire,2), "±",round(data$SD_K069,2))

# Extraction des données et calculs pour chaque paramètre
Variete <- factor(rep(c("K067", "K068", "K069"), each = 2))

###################################
# Amidon
Amidon <- c(data$essai1_lentille_verte[1], data$essai2_lentille_verte[1],
               data$essai1_lentille_corail[1], data$essai2_lentille_corail[1],
               data$essai1_lentille_noire[1], data$essai2_lentille_noire[1])

dataaov <- data.frame(Variete, Amidon)


# ANOVA pour Amidon
myaov <- aov(Amidon ~ Variete, data = dataaov)
summary(myaov)

# Test post-hoc Tukey pour Amidon
tukey_result <- TukeyHSD(myaov)
print(tukey_result)

# Extraction des lettres pour Amidon
tukey_pvalues <- tukey_result$Variete[, "p adj"]
letters_amidon <- multcompLetters(tukey_pvalues, compare = "<", threshold = 0.05)$Letters
print(letters_amidon)

###################################
# Répéter le même processus pour le Fibre

# Fibre
Fibre <- c(data$essai1_lentille_verte[2], data$essai2_lentille_verte[2],
            data$essai1_lentille_corail[2], data$essai2_lentille_corail[2],
            data$essai1_lentille_noire[2], data$essai2_lentille_noire[2])

dataaov <- data.frame(Variete, Fibre)
# ANOVA pour Fibre
myaov <- aov(Fibre ~ Variete, data = dataaov)
summary(myaov)

# Test post-hoc Tukey pour Fibre
tukey_result <- TukeyHSD(myaov)
print(tukey_result)

# Extraction des lettres pour Fibre
tukey_pvalues <- tukey_result$Variete[, "p adj"]
letters_fibre <- multcompLetters(tukey_pvalues, compare = "<", threshold = 0.05)$Letters
print(letters_fibre)

print(letters_amidon)
print(letters_fibre)

data$Lentille_verte[1] <- paste0(data$Lentille_verte[1],letters_amidon["K067"])
data$Lentille_corail[1] <- paste0(data$Lentille_corail[1],letters_amidon["K068"])
data$Lentille_noire[1] <- paste0(data$Lentille_noire[1],letters_amidon["K069"])

data$Lentille_verte[2] <- paste0(data$Lentille_verte[2],letters_fibre["K067"])
data$Lentille_corail[2] <- paste0(data$Lentille_corail[2],letters_fibre["K068"])
data$Lentille_noire[2] <- paste0(data$Lentille_noire[2],letters_fibre["K069"])

# Afficher les données avec les écarts-types
data_final <- data[, c("Lentille","Lentille_verte", "Lentille_corail", "Lentille_noire")]

#View(data_final)
# Exporter le tableau des résultats | nutrition path change en cas de changement de dossier
write_xlsx(data_final, "~/nutrition/resultat/resultats_amidonEtfibre.xlsx")
