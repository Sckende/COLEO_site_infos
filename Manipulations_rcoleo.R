setwd("C:/Users/HP_9470m/Desktop/PostDoc_COLEO/shiny_site_info/SITES_INFOS_tests/COLEO_site_infos")

library(tidyverse)
library(rcoleo)
library(dplyr)
library(purrr)

source("functions.R")

# Obtention des informations pour tous les sites
all_sites <- get_sites()[[1]][[1]][[1]]
#utils::View(all_sites)
names(all_sites)

# Obtention des coordonnées des sites d'échantillonnage
#utils::View(all_sites %>% select(
#  geom.coordinates))
all_sites$geom.coordinates[[1]]
k <- as.data.frame(do.call("rbind", all_sites$geom.coordinates))
k
all_sites <- all_sites %>% 
  mutate(long_site = do.call("rbind", all_sites$geom.coordinates)[,1],
         lat_site = do.call("rbind", all_sites$geom.coordinates)[,2])
names(all_sites)

# Association d'une couleur par habitat
all_sites$col <- c("#66CC00", "#000066", "#666600", "#003333", "#0066CC", "#FF9900", "#660000")[as.integer(as.factor(all_sites$type))]

# Creation de pop-ups 
all_sites <- all_sites %>% 
  mutate(popup_info = paste0("<b> id_cellule</b> ",
                             cell_id,
                             "<br/>",
                             "<b> nom_cellule</b> ",
                             cell.name,
                             "<br/>",
                             "<b> code_site</b> ",
                             site_code,
                             "<br/>",
                             "<b> type_habitat</b> ",
                             type,
                             "<br/>",
                             "<b> date_creation</b> ",
                             opened_at))

# Observations des especes
# Utilisation de la fonction de Andrew get_obs_df() - modifiée via retrait de la variable "media", qui est une liste et qui beug avec la fonction xtable(xtable) de la fonction renderTable(Shiny)

all_obs <-get_obs_df()
names(all_obs)
str(all_obs)
dim(all_obs)

# Vérification si tous les codes des sites pour les especes observees sont contenus dans la liste de codes de tous les sites existants
all(unique(all_obs$site_code) %in% unique(all_sites$site_code))


