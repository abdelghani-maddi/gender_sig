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


### Chargement des données ----

# Données sur les prénoms
gender <- readRDS("gender.rds")

# La base RetractionWatch brute
rtw <- read.csv("retraction_watch.csv")

# La base RetractionWatch extraite d'OpenAlex
rtw_oax <- readRDS("rtw_oax.rds")
rtw_oax$bdd <- "retractionwatch"

# La base OpenAlex (échantillon aléatoire)
oa_metadata_all <- readRDS("C:/Users/amaddi/Documents/Recherche/Pubpeer Gender/Révisions QSS/oa_metadata_allv2.rds")
oa_metadata_all$extract_date <- "2024"
oa_metadata_all$bdd <- "openalex"

#===================================
# Table order flag alpha
#===================================
row_data <- rbind(rtw_oax, oa_metadata_all)

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

# extraire only les deux clonnes qui nous intéressent
id_flag_alph <- final_df %>%
  select(id, is_sorted_alphabetically, num_authors) %>%
  distinct()

# Ajouter les deux colonnes 
oax_rtw_alpha <- row_data %>%
  left_join(id_flag_alph, by = "id")

# oax_rtw_alpha_na <- oax_rtw_alpha %>%
#   filter(is.na(is_sorted_alphabetically))
# > describe(oax_rtw_alpha_na$bdd) 
# [74774 obs.] Métadonnées sur les auteurs non disponibles
#                    n     %
# openalex        73954  98.9
# retractionwatch   820   1.1
# Total           74774 100.0

oax_rtw_alpha <- oax_rtw_alpha %>%
  filter(!is.na(is_sorted_alphabetically))

# Tables inutiles désormais
# rm(final_df, oax_rtw_alpha_na)

saveRDS(oax_rtw_alpha, "oax_rtw_alpha.rds")
  
#===================================
# Table discipline + fractionnaire
#===================================

# Initialiser une liste pour stocker tous les petits dataframes
results_list <- vector("list", length = nrow(row_data))

# Parcourir toutes les lignes de row_data
for (i in 1:nrow(row_data)) {
  df <- row_data[[38]][[i]]  # Extraire le dataframe de la colonne 38, ligne i
  
  if (!is.null(df) && is.data.frame(df)) {  # Vérifier que ce n'est pas NULL
    df$id <- row_data$id[i]  # Ajouter l'ID correspondant
    results_list[[i]] <- df  # Ajouter ce dataframe à la liste
  }
}

# Combiner tous les dataframes extraits en un seul
final_df <- rbindlist(results_list, fill = TRUE)  # fill = TRUE pour éviter erreurs si colonnes varient

# # Conversion de row_data en data.table pour un traitement rapide
# row_data <- as.data.table(row_data)

##########################
# disc <- final_df %>%
#   select(name, display_name) %>%
#   unique() # choisir le niveau intermédiaire : field

# Se limiter aux grandes disciplines (environ 28 disciplines)
final_df_field <- final_df %>%
  select(-score, -i) %>%
  filter(name == "field") %>%
  distinct(id, display_name, .keep_all = TRUE) %>%  # éviter les doublons
  group_by(id) %>%
  mutate(frac_disc = 1 / n()) %>%
  ungroup() %>%
  rename(disc = display_name)

# vérification et test du fractionnement (la somme par id doit être = 1)
# final_df_field %>%
#   group_by(id) %>%
#   summarise(total = sum(frac_disc)) %>%
#   filter(abs(total - 1) > 1e-6)  # tout est ok!


# Ajouter les deux colonnes 
oax_rtw_alpha_disc <- oax_rtw_alpha %>%
  left_join(final_df_field, by = "id")

oax_rtw_alpha_disc_na <- oax_rtw_alpha_disc %>%
  filter(is.na(disc)) %>%
  select(id, bdd) %>%
  unique()
# > describe(oax_rtw_alpha_disc_na$bdd) 
# [109889 obs.]  Métadonnées sur les disciplines non disponibles
#                     n     %
# openalex        109525  99.7
# retractionwatch    364   0.3
# Total           109889 100.0

oax_rtw_alpha_disc <- oax_rtw_alpha_disc %>%
  filter(!is.na(disc))

# Tables inutiles désormais
# rm(final_df, oax_rtw_alpha_na)

saveRDS(oax_rtw_alpha_disc, "oax_rtw_alpha_disc.rds")


#===================================
# Table Gender et types de collab HF
#===================================







#===================================
# Table info corresp + First / Last
#===================================




######################################
######################################
# Abalyse des données -----
######################################
######################################


#===================================
# Pratiques de signature selon les disciplines
#===================================



#===================================
# Doubles ratio sur les rétractation
#===================================


#===================================
# Raisons de rétractation
#===================================


#===================================
# 
#===================================
