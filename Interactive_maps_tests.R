setwd("C:/Users/HP_9470m/Desktop/PostDoc_COLEO/shiny_site_info/SITES_INFOS_tests/COLEO_site_infos")

# Interactive map

library(shiny)
library(rnaturalearth)
library(sf)
library(tidyverse)
library(rcoleo)
library(ggiraph)
library(leaflet)
library(dplyr)

source("functions.R")
source("Manipulation_rcoleo.R")

### ---------------LEAFLET TESTS ------------------------- ###


leaflet() %>% 
  addTiles() %>% # Affichage du fond de carte
  addCircleMarkers(lng = j$long_site, # Positionnement des sites avec les coordonnées long/lat 
                   lat = j$lat_site,
                   radius = 8, # taille du cercle
                   popup = j$popup_info, # Ajout de fenêtres pop-up
                   color = c("#66CC00", "#000066", "#666600", "#003333", "#0066CC", "#FF9900", "#660000")[as.integer(as.factor(j$type))])

