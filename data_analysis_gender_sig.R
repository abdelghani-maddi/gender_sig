#####################################################################
rm(list = ls()) #supprimer tous les objets 

### Chargement des packages ----

library(tidyverse)
library(questionr)
library(RPostgres)
library(gtsummary)
library(openxlsx2)
library(gender)
library(GenderInfer)
library(openxlsx)
library(readxl)
library(data.table)  # Plus rapide que data.frame pour les gros datasets

### Data du genre selon prenom
saveRDS(gender_proba_3, "C:/Users/amaddi/Documents/Recherche/Gender Retraction signatures/gender.rds")
gender <- readRDS("C:/Users/amaddi/Documents/Recherche/Pubpeer Gender/Révisions QSS/gender.rds")

###
df_oa <- readRDS("C:/Users/amaddi/Documents/Recherche/Pubpeer Gender/Révisions QSS/df_oa.rds")
rtw <- read.csv("C:/Users/amaddi/Documents/Recherche/Gender Retraction signatures/retraction_watch.csv")

###
# library(openalexR)
# rtw_oax <- oa_fetch(
#   entity = "works",
#   doi = rtw$OriginalPaperDOI,
#   verbose = TRUE
# )
rtw_oax <- rtw_oax %>%
  mutate(extract_date = "21/04/2025")

rtw_oax <- saveRDS(rtw_oax, "C:/Users/amaddi/Documents/Recherche/Gender Retraction signatures/rtw_oax.rds")


# Données tout Echantillon aléatoire OAx : 887,589 publications
# row_data <- readRDS("~/Recherche/Pubpeer Gender/Révisions QSS/oa_metadata_allv2.rds")
row_data <- readRDS("C:/Users/amaddi/Documents/Recherche/Pubpeer Gender/Révisions QSS/oa_metadata_allv2.rds")


# Initialiser une liste pour stocker tous les petits dataframes
results_list <- vector("list", length = nrow(row_data))

# Parcourir toutes les lignes de row_data
for (i in 1:nrow(row_data)) {
  df <- row_data[[4]][[i]]  # Extraire le dataframe de la colonne 4, ligne i
  
  if (!is.null(df) && is.data.frame(df)) {  # Vérifier que ce n'est pas NULL
    df$id <- row_data$id[i]  # Ajouter l'ID correspondant
    results_list[[i]] <- df  # Ajouter ce dataframe à la liste
  }
}

# Combiner tous les dataframes extraits en un seul
final_df <- rbindlist(results_list, fill = TRUE)  # fill = TRUE pour éviter erreurs si colonnes varient

# Conversion de row_data en data.table pour un traitement rapide
row_data <- as.data.table(row_data)

# Fonction pour extraire le nom de famille d'un nom complet
get_last_name <- function(full_name) {
  # Extraire le dernier mot (le nom de famille)
  sapply(strsplit(full_name, " "), function(x) tail(x, 1))
}

##########################
final_df <- final_df %>%
  group_by(id) %>%
  mutate(
    last_name = get_last_name(au_display_name),  # Extraire le nom de famille
    is_sorted_alphabetically = all(order(last_name) == seq_along(last_name)),  # Vérifie l'ordre alphabétique
    num_authors = n()  # Nombre d'auteurs = nombre de lignes par ID
  ) %>%
  ungroup()  # Retirer les groupes
saveRDS(final_df, "C:/Users/amaddi/Documents/Recherche/Gender Retraction signatures/final_df.rds")


# Afficher les premières lignes pour vérifier

counts_alpha <- final_df %>%
  filter(num_authors>1) %>%
  select(id, is_sorted_alphabetically) %>%
  distinct() %>%  # Retirer les doublons
  group_by(is_sorted_alphabetically) %>%
  summarise(n = n(),
            share = (n / 466935) * 100) 

# W2790966113

####################

df_oax_alpha <- final_df %>%
  select(id, num_authors, is_sorted_alphabetically) %>%
  unique()

saveRDS(df_oax_alpha, "C:/Users/amaddi/Documents/Recherche/Gender Retraction signatures/df_oax_alpha.rds")
df_oax_alpha <- readRDS("C:/Users/amaddi/Documents/Recherche/Gender Retraction signatures/df_oax_alpha.rds")


sample_100 <- df_oax_alpha %>%
  filter(num_authors > 2) %>%
  slice_sample(n = 100)

sample_100 <- saveRDS(sample_100, "C:/Users/amaddi/Documents/Recherche/Gender Retraction signatures/sample_100.rds")
write.xlsx(sample_100, "C:/Users/amaddi/Documents/Recherche/Gender Retraction signatures/sample_100.xlsx")



#####

# Initialiser une liste pour stocker tous les petits dataframes
results_list <- vector("list", length = nrow(rtw_oax))

# Parcourir toutes les lignes de rtw_oax
for (i in 1:nrow(rtw_oax)) {
  df <- rtw_oax[[4]][[i]]  # Extraire le dataframe de la colonne 4, ligne i
  
  if (!is.null(df) && is.data.frame(df)) {  # Vérifier que ce n'est pas NULL
    df$id <- rtw_oax$id[i]  # Ajouter l'ID correspondant
    results_list[[i]] <- df  # Ajouter ce dataframe à la liste
  }
}

# Combiner tous les dataframes extraits en un seul
final_rtw <- rbindlist(results_list, fill = TRUE)  # fill = TRUE pour éviter erreurs si colonnes varient

# Conversion de row_data en data.table pour un traitement rapide
row_data_rtw <- as.data.table(rtw_oax)

# Fonction pour extraire le nom de famille d'un nom complet
get_last_name <- function(full_name) {
  # Extraire le dernier mot (le nom de famille)
  sapply(strsplit(full_name, " "), function(x) tail(x, 1))
}


##########################
final_df_rtw <- final_rtw %>%
  group_by(id) %>%
  mutate(
    last_name = get_last_name(au_display_name),  # Extraire le nom de famille
    is_sorted_alphabetically = all(order(last_name) == seq_along(last_name)),  # Vérifie l'ordre alphabétique
    num_authors = n()  # Nombre d'auteurs = nombre de lignes par ID
  ) %>%
  ungroup()  # Retirer les groupes

saveRDS(final_df_rtw, "C:/Users/amaddi/Documents/Recherche/Gender Retraction signatures/final_df_rtw.rds")


# Afficher les premières lignes pour vérifier

counts_alpha <- final_df_rtw %>%
  filter(num_authors>1) %>%
  select(id, is_sorted_alphabetically) %>%
  distinct() %>%  # Retirer les doublons
  group_by(is_sorted_alphabetically) %>%
  summarise(n = n(),
            share = (n / 45661) * 100) # 83% pas ordre alpha

