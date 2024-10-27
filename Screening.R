# Charger les bibliothèques nécessaires
library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)

# Charger les données
data <- read_excel("~/Essai/Phytochimique.xlsx")

# Transformation en format long
data_long <- data %>%
  select(PARAMETRES, contains("Essai")) %>%  # Sélectionner les colonnes pertinentes
  pivot_longer(cols = starts_with("Essai"), 
               names_to = c("Essai", "Type_Lentille"), 
               names_pattern = "Essai(\\d)_(.*)", 
               values_to = "Valeur") %>%
  mutate(Type_Lentille = factor(Type_Lentille))

# Création du boxplot
ggplot(data_long, aes(x = Type_Lentille, y = Valeur, fill = Type_Lentille)) + 
  geom_boxplot() +
  facet_wrap(~ PARAMETRES, scales = "free_y") +  # Un graphique pour chaque paramètre
  labs(title = "Boxplot des valeurs phytochemiques par type de lentille", 
       x = "Type de Lentille", 
       y = "Valeurs") +
  theme_minimal() +
  theme(legend.position = "none")
