#####################################################################
rm(list = ls()) #supprimer tous les objets 

### Chargement des packages ----

library(tidyverse)
library(questionr)
library(gtsummary)
library(gender)
library(GenderInfer)
library(openxlsx)
library(readxl)
library(data.table)  # Plus rapide que data.frame pour les gros datasets
library(forcats)
library(scales)
library(stringi)      # Pour la translittération


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
# final_df <- final_df %>%
#   group_by(id) %>%
#   mutate(
#     last_name = get_last_name(au_display_name),  # Extraire le nom de famille
#     is_sorted_alphabetically = all(order(last_name) == seq_along(last_name)),  # Vérifie l'ordre alphabétique
#     num_authors = n()  # Nombre d'auteurs = nombre de lignes par ID
#   ) %>%
#   ungroup()  # Retirer les groupes

library(dplyr)

final_df <- final_df %>%
  group_by(id) %>%
  mutate(
    last_name = get_last_name(au_display_name),
    num_authors = n(),  # Nombre d'auteurs
    
    is_alpha_az = all(order(last_name) == seq_along(last_name)),
    is_alpha_za = all(order(last_name, decreasing = TRUE) == seq_along(last_name)),
    
    is_alpha_except_first = if (num_authors[1] >= 3) {
      all(order(last_name[-1]) == seq_along(last_name[-1]))
    } else {
      FALSE
    },
    
    is_alpha_except_last = if (num_authors[1] >= 3) {
      all(order(last_name[-num_authors[1]]) == seq_along(last_name[-num_authors[1]]))
    } else {
      FALSE
    },
    
    author_order_type = case_when(
      is_alpha_az ~ "alpha_a_to_z",
      is_alpha_za ~ "alpha_z_to_a",
      is_alpha_except_first ~ "alpha_except_first",
      is_alpha_except_last ~ "alpha_except_last",
      TRUE ~ "not_alpha"
    )
  ) %>%
  ungroup()



# extraire only les deux clonnes qui nous intéressent
id_flag_alph <- final_df %>%
  select(id, author_order_type, num_authors) %>%
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
  filter(!is.na(author_order_type))

# Tables inutiles désormais
# rm(final_df, oax_rtw_alpha_na)

saveRDS(oax_rtw_alpha, "oax_rtw_alphaV2.rds")
  
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

# saveRDS(oax_rtw_alpha_disc, "oax_rtw_alpha_disc.rds")
oax_rtw_alpha_disc <- readRDS("oax_rtw_alpha_disc.rds")

### Analyse ordre alpha selon les disciplines

alpha_disc <- oax_rtw_alpha_disc %>%
  filter(num_authors > 1) %>%
  select(id, disc, is_sorted_alphabetically) %>%
  distinct() %>%
  group_by(disc, is_sorted_alphabetically) %>%
  summarise(nb = n(), .groups = "drop_last") %>%
  mutate(part = nb / sum(nb))


# Reordonner les disciplines selon la part décroissante de TRUE
alpha_disc_ordered <- alpha_disc %>%
  mutate(disc = as.factor(disc)) %>%
  group_by(disc) %>%
  mutate(total_part_true = sum(part * (is_sorted_alphabetically == TRUE))) %>%
  ungroup() %>%
  mutate(disc = fct_reorder(disc, total_part_true, .desc = TRUE),
         part_label = paste0(round(part * 100), "%"))

# Graphique
ggplot(alpha_disc_ordered, aes(x = disc, y = part, fill = is_sorted_alphabetically)) +
  geom_col(position = "fill", width = 0.7) +
  geom_text(aes(label = ifelse(part > 0.05, part_label, "")),  # éviter le texte sur les très petites parts
            position = position_fill(vjust = 0.5),
            color = "white", size = 3.5, fontface = "bold") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Part des publications par ordre alphabétique selon les disciplines",
    x = NULL,
    y = "Part (%)",
    fill = "Ordre alphabétique"
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
  )

### Analyse ordre alpha selon les disciplines : par année

alpha_disc_year <- oax_rtw_alpha_disc %>%
  filter(num_authors > 1) %>%
  select(id, publication_year, disc, is_sorted_alphabetically) %>%
  distinct() %>%
  group_by(publication_year, disc, is_sorted_alphabetically) %>%
  summarise(nb = n(), .groups = "drop_last") %>%
  mutate(part = nb / sum(nb))



# Données préparées : garder uniquement TRUE pour afficher la part des pubs triées
# 1. Total par discipline
total_per_disc <- alpha_disc_year %>%
  group_by(disc) %>%
  summarise(total_pubs = sum(nb), .groups = "drop")

# 2. Données principales (TRUE seulement) + join avec total
alpha_disc_year_plot <- alpha_disc_year %>%
  filter(is_sorted_alphabetically == TRUE 
         & publication_year>1950
         & publication_year<2025) %>%
  left_join(total_per_disc, by = "disc")

# 3. Données pour les filigranes : une ligne par discipline
theme_gtsummary_language(language = "fr", decimal.mark = ",", big.mark = " ")

filigrane_labels <- total_per_disc %>%
  mutate(
    label = paste0("#", total_pubs),
    x = mean(range(alpha_disc_year_plot$publication_year, na.rm = TRUE)),  # centré sur l’axe X
    y = 0.7  # centré sur l’axe Y
  )

# 4. Le graphique
ggplot(alpha_disc_year_plot, aes(x = publication_year, y = part)) +
  # Filigrane
  geom_text(data = filigrane_labels, aes(x = x, y = y, label = label),
            inherit.aes = FALSE,
            color = "darkgray", size = 9, alpha = 0.3, fontface = "bold") +
  # Courbe principale
  geom_line(size = 1.2, color = "#2C3E50") +
  geom_point(size = 2, color = "#2C3E50") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red", size = 1) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(
    title = "Évolution de la part des publications par ordre alphabétique",
    subtitle = "Par discipline (≥ 2 auteurs), avec nombre total de publications en filigrane",
    x = "Année",
    y = "Part (%)"
  ) +
  facet_wrap(~ disc, scales = "free_y") +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12)
  )


#===================================
# Table Gender et types de collab HF
#===================================
oax_rtw_alpha_disc_gender

# Initialiser une liste pour stocker tous les petits dataframes
results_list <- vector("list", length = nrow(oax_rtw_alpha_disc))

# Parcourir toutes les lignes de oax_rtw_alpha_disc
for (i in 1:nrow(oax_rtw_alpha_disc)) {
  df <- oax_rtw_alpha_disc[[4]][[i]]  # Extraire le dataframe de la colonne 38, ligne i
  
  if (!is.null(df) && is.data.frame(df)) {  # Vérifier que ce n'est pas NULL
    df$id <- oax_rtw_alpha_disc$id[i]  # Ajouter l'ID correspondant
    results_list[[i]] <- df  # Ajouter ce dataframe à la liste
  }
}

# Combiner tous les dataframes extraits en un seul : on a 2,376,715 auteurs 
final_df <- rbindlist(results_list, fill = TRUE)  # fill = TRUE pour éviter erreurs si colonnes varient

final_df <- final_df %>%
  unique()

# Exemple : "Y. Nakamura", "I. S. Yahia", "Marie Curie", "张伟"

library(dplyr)
library(stringi)
library(stringr)


# Fonction vectorisée pour extraire le prénom
extract_first_name_vec <- function(names) {
  sapply(names, function(name) {
    name <- str_squish(name)
    words <- str_split(name, "\\s+")[[1]]
    
    if (length(words) == 0) return(NA)
    
    if (grepl("^[A-Z]\\.$", words[1])) {
      # Si le nom commence par une initiale, on tente d'aller plus loin
      if (length(words) >= 3 && grepl("^[A-Z]\\.$", words[2])) {
        return(words[3])
      } else if (length(words) >= 2) {
        return(words[2])
      } else {
        return(NA)
      }
    } else {
      return(words[1])
    }
  }, USE.NAMES = FALSE)
}

# Fonction pour translittérer seulement si nécessaire
smart_transliterate <- function(x) {
  need_conversion <- stri_detect_regex(x, "[^\\p{Latin}]")
  x[need_conversion] <- stri_trans_general(x[need_conversion], "Any-Latin")
  return(x)
}


# https://openalex.org/A5002127800 : test si la traduction fonctionne bien : parfait

# Application sur le dataframe
final_df <- final_df %>%
  mutate(
    prenom_orig = extract_first_name_vec(au_display_name),
    prenom_min = tolower(prenom_orig),
    prenom_latin = ifelse(
      is.na(prenom_min), NA_character_,
      stringi::stri_trans_general(prenom_min, "Any-Latin")
    )
  ) %>%
  left_join(gender, by = c("prenom_latin" = "given_name"))


###
config_df <- final_df %>%
  group_by(id) %>%
  summarise(
    n_authors = n(),
    n_males = sum(gender == "male", na.rm = TRUE),
    n_females = sum(gender == "female", na.rm = TRUE),
    first_gender = gender[author_position == "first"][1],
    last_gender = gender[author_position == "last"][1],
    gender_corresponding = gender[is_corresponding == TRUE][1],
    n_unknowns = sum(is.na(gender)),
    .groups = "drop"
  ) %>%
  mutate(
    config =
      case_when(
        n_authors == 1 & first_gender == "male" ~ "Homme seul",
        n_authors == 1 & first_gender == "female" ~ "Femme seule",
        n_males == n_authors ~ "Hommes entre eux",
        n_females == n_authors ~ "Femmes entre elles",
        n_males > 0 & n_females > 0 ~ case_when(
          first_gender == "female" | last_gender == "female" ~ "Mixte : femme en 1re ou dernière position",
          first_gender == "male" | last_gender == "male" ~ "Mixte : homme en 1re ou dernière position",
          TRUE ~ "Mixte (autre)"
        ),
        TRUE ~ "Configuration inconnue"
      )
  )


#####
oax_rtw_alpha_disc_gender <- oax_rtw_alpha_disc %>%
  left_join(config_df, by = "id")

# saveRDS(oax_rtw_alpha_disc_gender, "oax_rtw_alpha_disc_gender.rds")
oax_rtw_alpha_disc_gender <- readRDS("oax_rtw_alpha_disc_gender.rds")

###

#===================================
# Table info corresp + First / Last
#===================================

######################################
######################################
# Abalyse des données -----
######################################
######################################

library(dplyr)

table_analysis <- oax_rtw_alpha_disc_gender %>%
  group_by(config, bdd, disc, is_sorted_alphabetically) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(bdd, disc, is_sorted_alphabetically) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()


write.xlsx(table_analysis, "table_analysis.xlsx")

### toutes disciplines
# 1. Calculer les proportions dans chaque base (RW et OA)
config_props <- oax_rtw_alpha_disc_gender %>%
  group_by(config, bdd, is_sorted_alphabetically) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(bdd, is_sorted_alphabetically) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()
write.xlsx(config_props, "config_props.xlsx")


# 2. Reformater pour calculer le ratio RW/OA
config_ratios <- config_props %>%
  select(config, is_sorted_alphabetically, bdd, prop) %>%
  pivot_wider(names_from = bdd, values_from = prop) %>%
  mutate(
    ratio_rw_oa = retractionwatch / openalex
  )

write.xlsx(config_ratios, "config_ratios.xlsx")


### Par discipline
# 1. Calculer les proportions dans chaque base (RW et OA)
config_props <- oax_rtw_alpha_disc_gender %>%
  group_by(config, bdd, disc, is_sorted_alphabetically) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(bdd, disc, is_sorted_alphabetically) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

# 2. Reformater pour calculer le ratio RW/OA
config_ratios <- config_props %>%
  select(config, disc, is_sorted_alphabetically, bdd, prop) %>%
  pivot_wider(names_from = bdd, values_from = prop) %>%
  mutate(
    ratio_rw_oa = retractionwatch / openalex
  )

#========================================
#========================================


#========================================
#========================================


### toutes disciplines
# 1. Calculer les proportions dans chaque base (RW et OA)
gender_corresponding <- oax_rtw_alpha_disc_gender %>%
  filter(!is.na(gender_corresponding)) %>%
  group_by(gender_corresponding, bdd, is_sorted_alphabetically) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(bdd, is_sorted_alphabetically) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()
write.xlsx(gender_corresponding, "gender_corresponding.xlsx")


# 2. Reformater pour calculer le ratio RW/OA
corresp_ratios <- gender_corresponding %>%
  select(gender_corresponding, is_sorted_alphabetically, bdd, prop) %>%
  pivot_wider(names_from = bdd, values_from = prop) %>%
  mutate(
    ratio_rw_oa = retractionwatch / openalex
  )

write.xlsx(corresp_ratios, "corresp_ratios.xlsx")

#========================================
#========================================

# Raisons de rétractation

#========================================
#========================================

rtw_reasons_long <- rtw %>%
  select(Record.ID, OriginalPaperDOI, Reason) %>%
  # Séparer les raisons
  separate_rows(Reason, sep = ";") %>%
  # Nettoyer chaque raison
  mutate(
    Reason = str_remove_all(Reason, "[^[:alnum:] /&'-]"), # Garde lettres, chiffres, /, &, ', -
    Reason = str_squish(Reason) # Supprime les espaces inutiles
  ) %>%
  filter(Reason != "") %>% # Retirer les vides
  # Calculer nombre de raisons par Record.ID
  group_by(Record.ID) %>%
  mutate(
    n_reasons = n(),
    fraction = 1 / n_reasons
  ) %>%
  ungroup()


# matcher avec les autres infos 
rtw_reasons_long$doi <- paste0("https://doi.org/",rtw_reasons_long$OriginalPaperDOI)
  
rtw_reasons_long <- rtw_reasons_long %>%
  left_join(oax_rtw_alpha_disc_gender, by = "doi")

####

# Sélectionner les bonnes colonnes et rendre unique
rtw_reasons_analyse <- rtw_reasons_long %>%
  select(id, doi, Reason, n_reasons, 
         fraction, config, gender_corresponding,
         is_sorted_alphabetically) %>%
  unique()

# 1. Calculer la part de chaque raison dans chaque configuration + tri alphabétique
reason_by_config <- rtw_reasons_analyse %>%
  filter(!is.na(config), !is.na(Reason), !is.na(is_sorted_alphabetically)) %>%
  group_by(config, Reason, is_sorted_alphabetically) %>%
  summarise(fraction_config = sum(fraction), .groups = "drop")

# 2. Total des fractions par config + tri alphabétique
total_by_config <- rtw_reasons_analyse %>%
  filter(!is.na(config), !is.na(Reason), !is.na(is_sorted_alphabetically)) %>%
  group_by(config, is_sorted_alphabetically) %>%
  summarise(total_config = sum(fraction), .groups = "drop")

# 3. Part de chaque raison dans chaque config triée
reason_by_config <- reason_by_config %>%
  left_join(total_by_config, by = c("config", "is_sorted_alphabetically")) %>%
  mutate(part_config = fraction_config / total_config)

# 4. Part globale de chaque raison, tri alphabétique respecté
global_reason <- rtw_reasons_analyse %>%
  filter(!is.na(config), !is.na(Reason), !is.na(is_sorted_alphabetically)) %>%
  group_by(Reason, is_sorted_alphabetically) %>%
  summarise(fraction_global = sum(fraction), .groups = "drop") %>%
  group_by(is_sorted_alphabetically) %>% 
  mutate(part_global = fraction_global / sum(fraction_global)) %>%
  ungroup()

# 5. Fusion pour obtenir le ratio de concentration
final_concentration <- reason_by_config %>%
  left_join(global_reason, by = c("Reason", "is_sorted_alphabetically")) %>%
  mutate(ratio_concentration = part_config / part_global)

# 6. Tri final pour lecture facile
final_concentration <- final_concentration %>%
  arrange(desc(ratio_concentration))


write.xlsx(final_concentration, "raisons.xlsx")


#######################

config_alpha_order <- final_df %>%
  select(id, author_order_type, is_alpha_az,
         is_alpha_za, is_alpha_except_first,
         is_alpha_except_last) %>%
  unique()
saveRDS(config_alpha_order, "config_alpha_order.rds")
  
oax_rtw_alpha_disc_gender$class_auth <- ifelse(oax_rtw_alpha_disc_gender$num_authors >10, ">=11", oax_rtw_alpha_disc_gender$num_authors)  
  
oax_rtw_alpha_disc_gender_config <- oax_rtw_alpha_disc_gender %>%
  left_join(config_alpha_order, by = "id")


saveRDS(oax_rtw_alpha_disc_gender_config, "oax_rtw_alpha_disc_gender_config.rds")
oax_rtw_alpha_disc_gender_config <- readRDS("oax_rtw_alpha_disc_gender_config.rds")


config_alpha_order <- oax_rtw_alpha_disc_gender_config %>%
  select(id, author_order_type, class_auth, frac_disc, disc) %>%
  unique()
describe(config_alpha_order$author_order_type)

library(dplyr)
library(ggplot2)
library(gtsummary)

# 1. Préparer les données
config_alpha_order_plot <- config_alpha_order %>%
  group_by(disc, class_auth, author_order_type) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(disc, class_auth) %>%
  mutate(
    total = sum(n),
    pct = (n / total) * 100
  ) %>%
  ungroup()

## Réordonnancement de config_alpha_order_plot$class_auth
config_alpha_order_plot$class_auth <- config_alpha_order_plot$class_auth %>%
  fct_relevel(
    "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", ">=11"
  )

# 2. Plot : un graph par discipline
ggplot(config_alpha_order_plot, aes(x = factor(class_auth), y = pct, fill = author_order_type)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(
    x = "Taille de l'équipe (class_auth)",
    y = "% de configuration d'ordre",
    fill = "Type d'ordre des auteurs",
    title = "Répartition des configurations d'ordre selon la taille des équipes"
  ) +
  facet_wrap(~ disc, scales = "free_x") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# 1. Préparer les données
config_alpha_order %>%
  select(id, class_auth, author_order_type) %>%
  unique() %>%
  group_by(class_auth, author_order_type)
  summarise(n = n(), .groups = "drop") 
  
  
config_alpha_order_all <- oax_rtw_alpha_disc_gender_config %>%
    select(id, author_order_type, class_auth, frac_disc) %>%
    unique()  

## Réordonnancement de config_alpha_order_plot$class_auth
config_alpha_order_all$class_auth <- config_alpha_order_all$class_auth %>%
  fct_relevel(
    "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", ">=11"
  )
config_alpha_order_all %>%
  tbl_summary(
    include = c(author_order_type, class_auth)
  )

## Réordonnancement de config_alpha_order_plot$class_auth
config_alpha_order$class_auth <- config_alpha_order$class_auth %>%
  fct_relevel(
    "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", ">=11"
  )


## Réordonnancement de config_alpha_order$author_order_type
config_alpha_order$author_order_type <- config_alpha_order$author_order_type %>%
  fct_relevel(
    "alpha_a_to_z", "alpha_z_to_a", "alpha_except_first", "alpha_except_last",
    "not_alpha"
  )

config_alpha_order %>%
  tbl_summary(
    include = c(disc, author_order_type),
    by = class_auth
  )



config_alpha_order %>%
  subset(!class_auth %in% "1")  %>%
  tbl_summary(
    include = c(disc),
    by = author_order_type,
    percent = "row"
  ) %>%
  add_overall(last = TRUE)


config_alpha_order %>%
  subset(!class_auth %in% "1")  %>%
  tbl_summary(
    include = c(disc, author_order_type),
    by = class_auth,
    percent = "row"
  ) %>%
  add_overall()


library(dplyr)
library(tidyr)

# Supprimer class_auth == 1
df <- config_alpha_order %>%
  filter(class_auth != 1)

# 1. Nombre de publications par discipline et type d'ordre
counts_by_disc <- df %>%
  group_by(disc, author_order_type) %>%
  summarise(n = n(), .groups = "drop")

# 2. Total par discipline pour calculer les parts locales
parts_by_disc <- counts_by_disc %>%
  group_by(disc) %>%
  mutate(part_in_disc = n / sum(n)) %>%
  ungroup()

# 3. Part globale par type d'ordre
parts_global <- df %>%
  count(author_order_type, name = "n_global") %>%
  mutate(global_part = n_global / sum(n_global))

# 4. Joindre et calculer le double ratio
double_ratio_df <- parts_by_disc %>%
  left_join(parts_global, by = "author_order_type") %>%
  mutate(double_ratio = part_in_disc / global_part)

# 5. Optionnel : présentation sous forme large
double_ratio_wide <- double_ratio_df %>%
  select(disc, author_order_type, double_ratio) %>%
  pivot_wider(names_from = author_order_type, values_from = double_ratio)

double_ratio_wide

write.xlsx(double_ratio_wide, "double_ratio_wide.xlsx")

