setwd("C:/Users/HP_9470m/Desktop/PostDoc_COLEO/shiny_site_info/SITES_INFOS_tests/COLEO_site_infos")

library(tidyverse)
library(rcoleo)
library(dplyr)
library(purrr)

#source("functions.R")

# Obtention des informations pour tous les sites
j <- get_sites()[[1]][[1]][[1]]
#utils::View(j)
names(j)

# Obtention des coordonnées des sites d'échantillonnage
#utils::View(j %>% select(
#  geom.coordinates))
j$geom.coordinates[[1]]
k <- as.data.frame(do.call("rbind", j$geom.coordinates))
k
j <- j %>% 
  mutate(long_site = do.call("rbind", j$geom.coordinates)[,1],
         lat_site = do.call("rbind", j$geom.coordinates)[,2])
names(j)

# Association d'une couleur par habitat
j$col <- c("#66CC00", "#000066", "#666600", "#003333", "#0066CC", "#FF9900", "#660000")[as.integer(as.factor(j$type))]

# Creation de pop-ups 
j <- j %>% 
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
all_sp <- get_obs()
all_sp_df <- map_df(all_sp[[1]], 1, drop = FALSE)
dim(all_sp_df)
