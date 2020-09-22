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

### TEST 1
#----------
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

# TEST 2
# ------

library(dplyr)
library(rvest)
library(ggplot2)
library(waffle)
library(rcdimple)

url14 <- html("http://en.wikipedia.org/wiki/Results_of_the_Indian_general_election,_2014")

tbls14 <- html_nodes(url14, "table")

df14 <- data.frame(html_table(tbls14[3], fill = TRUE))

df14 <- df14 %>%
  select(party = Party, seats = Seats) %>%
  filter(party != "Total", seats > 0, !is.na(party), !is.na(seats)) %>%
  mutate(party2 = ifelse(min_rank(desc(seats)) < 11, party, "Other")) %>%
  group_by(party2) %>%
  summarize(seats2 = sum(seats)) %>%
  mutate(party2 = gsub("All India Anna Dravida Munnetra Kazhagam", "All India ADMK", party2)) 


v14 <- c("Bharatiya Janata Party", "Indian National Congress", 
         "All India ADMK", "All India Trinamool Congress", 
         "Biju Janata Dal", "Shiv Sena", "Telugu Desam Party", "Telangana Rashtra Samithi", 
         "Communist Party of India (Marxist)", "YSR Congress Party", "Other")

df14$factor <- factor(df14$party2, levels = rev(v14), labels = rev(v14))

df14 <- arrange(df14, desc((factor)))

colors14 <- c("#FF8C00", "#00FFFF", "#000000", "#7CFC00", "#006400", 
              "#FFA500", "#FFFF00", "#FFC0CB", "#FF0000", "#0000FF", "#696969")

vec14 <- structure(df14$seats2, names = df14$party2)


# Interactive waffle chart!

waffle(vec14, rows = 15, title = "2014 Lok Sabha", colors = colors14) %>%
  as_rcdimple(height = 375, width = 1000 ) %>%
  add_legend( x = "0%", width = "100%", orderRule = "vec14")


### --------- OVERLAP BETWEEN BARPLOT/HISTOGRAM & LINE PLOT ----------------- ###

g <- CO2 %>% sample_n(12) 

#barplot(g$conc)
plot(g$conc, type = "h")
par(new = TRUE)
plot(g$uptake, type = "l")

