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
source("Manipulations_rcoleo.R")

### ---------------LEAFLET TESTS ------------------------- ###


leaflet() %>% 
  addTiles() %>% # Affichage du fond de carte
  addCircleMarkers(lng = j$long_site, # Positionnement des sites avec les coordonnées long/lat 
                   lat = j$lat_site,
                   radius = 8, # taille du cercle
                   popup = j$popup_info, # Ajout de fenêtres pop-up
                   color = c("#66CC00", "#000066", "#666600", "#003333", "#0066CC", "#FF9900", "#660000")[as.integer(as.factor(j$type))])

### -------------- INTERACTIVE WAFFLE PLOT TESTS ---------------- ###

# library(waffle)
# packageVersion("waffle")
# library(magrittr)
# library(hrbrthemes)
# library(ggplot2)
# library(dplyr)
# 
# xdf <- iris %>% count(Species)
# class(w)


storms %>% 
  filter(year == 2010) %>% 
  count(status)

all_obs %>%  filter(site_code == "142_111_F01") %>% count(type) -> xdf

library(hrbrthemes)
library(waffle)
library(tidyverse)

# tibble(
#   parts = factor(rep(month.abb[1:3], 3), levels=month.abb[1:3]),
#   values = c(10, 20, 30, 6, 14, 40, 30, 20, 10),
#   fct = c(rep("Thing 1", 3), rep("Thing 2", 3), rep("Thing 3", 3))
# ) -> xdf

ggplot(xdf, aes(fill=type, values=n)) +
  geom_waffle(color = "white", size=1.125, n_rows = 6) +
  #facet_wrap(~fct, ncol=1) +
 # scale_x_discrete(expand=c(0,0)) +
 # scale_y_discrete(expand=c(0,0)) +
 # ggthemes::scale_fill_tableau(name=NULL) +
  coord_equal() +
  labs(
    title = "Faceted Waffle Geoms"
  ) +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle()
