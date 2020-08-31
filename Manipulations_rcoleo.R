setwd("C:/Users/HP_9470m/Desktop/PostDoc_COLEO/shiny_site_info")

library(tidyverse)
library(rcoleo)
library(dplyr)

#source("functions.R")

# Obtention des informations pour tous les sites
j <- get_sites()[[1]][[1]][[1]]
utils::View(j)
names(j)

# Obtention des coordonnées des sites d'échantillonnage
utils::View(j %>% select(
  geom.coordinates))
j$geom.coordinates[[1]]
k <- as.data.frame(do.call("rbind", j$geom.coordinates))
k
j <- j %>% 
  mutate(long_site = do.call("rbind", j$geom.coordinates)[,1],
         lat_site = do.call("rbind", j$geom.coordinates)[,2])
names(j)

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
                             "<b> date_création</b> ",
                             opened_at))


