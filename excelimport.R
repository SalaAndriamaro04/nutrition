# Importer le fichier Excel
#install.packages("readxl")
library(readxl)
library(writexl)

data <- read_excel("~/Essai/Macronutriments.xlsx");

# Vérifier les noms de colonnes après modification
colnames(data)

calculate_sd <- function(essai1, essai2) {
  return(sd(c(essai1, essai2)))
}


# Appliquer la fonction sur chaque lentille et ajouter une colonne d'écart-type pour chaque paramètre
data$SD_K067 <- mapply(calculate_sd, data$Essai1_K067_Lentille_verte, data$Essai2_K067_Lentille_verte);
data$SD_K068 <- mapply(calculate_sd, data$Essai1_K068_Lentille_corail, data$Essai2_K068_Lentille_corail);
data$SD_K069 <- mapply(calculate_sd, data$Essai1_K069_Lentille_noire, data$Essai2_K069_Lentille_noire);

# Afficher le tableau avec moyenne et écart-type
data$Moyenne_K067 <- paste(round(data$Moyenne_K067_Lentille_verte, 2), "±", round(data$SD_K067, 2))
data$Moyenne_K068 <- paste(round(data$Moyenne_K068_Lentille_corail, 2), "±", round(data$SD_K068, 2))
data$Moyenne_K069 <- paste(round(data$Moyenne_K069_Lentille_noire, 2), "±", round(data$SD_K069, 2))

# Afficher les données avec les écarts-types
data_final <- data[, c("PARAMETRES LENTILLES", "UNITE","Moyenne_K067", "Moyenne_K068", "Moyenne_K069")]
#View(data_final)

#write.csv(data, "~/Essai/Macronutriments_Avec_Moyennes.csv", row.names = FALSE)
# Écriture des données dans un fichier Excel
write_xlsx(data_final, "~/Essai/Macronutriments_Ecarttype.xlsx")
