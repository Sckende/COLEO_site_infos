setwd("C:/Users/HP_9470m/Desktop/PostDoc_COLEO/shiny_site_info/SITES_INFOS_tests/COLEO_site_infos")

# Interactive map

library(shiny)
#library(rnaturalearth)
#library(sf)
library(tidyverse)
library(rcoleo)
#library(ggiraph)
library(leaflet)
library(dplyr)

source("functions.R")
source("Manipulations_rcoleo.R")

#### ---------------LEAFLET TESTS ------------------------- ####


leaflet() %>% 
  addTiles() %>% # Affichage du fond de carte
  addCircleMarkers(lng = j$long_site, # Positionnement des sites avec les coordonnées long/lat 
                   lat = j$lat_site,
                   radius = 8, # taille du cercle
                   popup = j$popup_info, # Ajout de fenêtres pop-up
                   color = c("#66CC00", "#000066", "#666600", "#003333", "#0066CC", "#FF9900", "#660000")[as.integer(as.factor(j$type))])

#### -------------- INTERACTIVE WAFFLE PLOT TESTS ---------------- ####

#### TEST 1 - Proportion des différentes catégories d'indicateurs dans toute la BDD ####
# ----------- #

library(hrbrthemes)
library(waffle)
library(tidyverse)
library(RColorBrewer)
if(!exists("all_obs")) source("Manipulations_rcoleo.R")

# xdf <- count(as.character(all_obs$category))
# xdf$prop <- round((xdf$freq * 100)/sum(xdf$freq), digits = 0)
# names(xdf)[1] <- "category"
# xdf <- dplyr::left_join(xdf, coul, by = "category")
# 
# ggplot(xdf, aes(fill=category, values=prop)) +
#   geom_waffle(color = "white", size = 1.125, n_rows = 10) +
#   coord_equal() +
#   scale_fill_brewer(palette = "Dark2") +
#   labs(title = "Proportion d'indicateurs", fill = "Catégories") +
#   theme_ipsum_rc(grid="") + # retrait du contour des pixels
#   theme_enhance_waffle() # retrait des valeurs en axis et ordonnées

ggplot(indic_count, aes(fill=category, values=prop)) +
  geom_waffle(color = "white", size = 1.125, n_rows = 10) +
  coord_equal() +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "Proportion d'indicateurs", fill = "Catégories") +
  theme_ipsum_rc(grid="") + # retrait du contour des pixels
  theme_enhance_waffle() # retrait des valeurs en axis et ordonnées



#### TEST 1 - Représentation des catégories présentes sur un site par rapport au reste de la BDD ####

# EXAMPLE #
# https://stackoverflow.com/questions/52741666/creating-a-waffle-plot-together-with-facets-in-ggplot2

data("mtcars")
source("waffle_functions.R")

mtcars$gear_vs <- paste(mtcars$gear, mtcars$vs, sep = "-")
mtcars$carb <- factor(mtcars$carb)
x <- mtcars %>% group_by(carb) %>% dplyr::summarise(value = sum(hp))

waffle_chart(x, fill = "carb", value = "value")

x1 <- mtcars %>% group_by(gear_vs, carb) %>% dplyr::summarise(value = sum(hp))

waffle_chart(x1, fill = "carb", facet = "gear_vs", value = "value")

## You can also scale the waffles to a maximum hp in gear_vs

y <- x1 %>% group_by(gear_vs) %>% dplyr::summarise(value = sum(value))

waffle_chart(x1, fill = "carb", facet = "gear_vs", value = "value", composition = FALSE, max_value = max(y$value))

# ----------- #
# Exemple pour le site 135_104_F01 / 136_116_H01 #
# ----------------- #
source("waffle_functions.R")

#site_count <- count(as.character(all_obs$category[all_obs$site_code == "135_104_F01"]))
site_count <- count(as.character(all_obs$category[all_obs$site_code == "136_116_H01"]))
names(site_count)[1] <- "category"
site_count <- dplyr::left_join(site_count, indic_count, by = "category")
names(site_count)[1:4] <- c("category", "freq_site", "freq_tot", "prop_tot")
site_count$prop_siteVStot <- round(site_count$freq_site/site_count$freq_tot*site_count$prop_tot, digits = 1)


waffle_chart(data = site_count, fill = "category", value = "freq_site", fill_colors = RColorBrewer::brewer.pal(n = length(site_count$category), name = 'Dark2'))

waffle_chart(data = site_count, fill = "category", value = "freq_site", fill_colors = as.character(site_count$cat_coul))

# ---------- #

newDF <- as.data.frame(rbind(as.matrix(site_count[, c(1, 2,5)]), as.matrix(site_count[, c(1, 3, 5)])))
names(newDF)[2] <- "freq"
newDF$data <- c(rep("site", length(unique(site_count$category))), rep("totale", length(unique(site_count$category))))
newDF$freq <- as.numeric(as.character(newDF$freq))
summary(newDF)

d <- newDF[newDF$category == "plantes",]
waffle_chart(data = d, fill = "data", value = "freq")


waffle_chart(data = newDF, fill = "data", facet = "category", value = "freq", composition = TRUE, max_value = NULL, fill_colors = RColorBrewer::brewer.pal(n = length(site_count$category), name = 'Dark2'))

x11()
waffle_chart(data = newDF, fill = "data", facet = "category", value = "freq", composition = FALSE, max_value = sum(newDF$freq[newDF$data == "totale"]), fill_colors =  c(as.character(unique(site_count$cat_coul)), "grey"))

# x11()
# par(mfrow = c(2,3))
for (i in as.character(indic_count$category)){
  d <- newDF[newDF$category == i,]
  print(waffle_chart(data = d, fill = "data", facet = "category", value = "freq", composition = FALSE, max_value = sum(newDF$freq[newDF$data == "totale"]), fill_colors = c(as.character(coul$cat_coul[coul$category == i]), "grey")))
  #print(waffle_chart(data = d, fill = "data", value = "freq", fill_colors = c(as.character(coul$cat_coul[coul$category == i]), "grey")))
}


#### TEST 2 ####
# ----------- #

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


#### --------- OVERLAP BETWEEN BARPLOT/HISTOGRAM & LINE PLOT ----------------- ####

g <- CO2 %>% sample_n(12) 

#barplot(g$conc)
plot(g$conc, type = "h")
par(new = TRUE)
plot(g$uptake, type = "l")

