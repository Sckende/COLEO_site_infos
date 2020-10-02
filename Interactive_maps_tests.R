setwd("/home/claire/PostDoc_COLEO/shiny_site_info/SITES_INFOS_tests/COLEO_site_infos")

# Interactive map

library(shiny)
#library(rnaturalearth)
#library(sf)
library(tidyverse)
library(rcoleo)
#library(ggiraph)
library(leaflet)
library(dplyr)
library(plotly)
library(ggplot2)

source("functions.R")
source("Manipulations_rcoleo.R")
source("waffle_functions.R")

#### ---------------LEAFLET TESTS ------------------------- ####


leaflet() %>% 
  addTiles() %>% # Affichage du fond de carte
  addCircleMarkers(lng = j$long_site, # Positionnement des sites avec les coordonnées long/lat 
                   lat = j$lat_site,
                   radius = 8, # taille du cercle
                   popup = j$popup_info, # Ajout de fenêtres pop-up
                   color = c("#66CC00", "#000066", "#666600", "#003333", "#0066CC", "#FF9900", "#660000")[as.integer(as.factor(j$type))])

#### -------------- INTERACTIVE WAFFLE PLOT TESTS ---------------- ####

#### TEST 1 - Proportion des différentes catégories d'indicateurs dans toute la BDD - ggplot2 ####

library(hrbrthemes)
library(waffle)
library(tidyverse)
library(RColorBrewer)
if(!exists("all_obs")) source("Manipulations_rcoleo.R")

ggplot(indic_count, aes(fill=category, values=prop)) +
  geom_waffle(color = "white", size = 1.125, n_rows = 10) +
  coord_equal() +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "Proportion d'indicateurs", fill = "Catégories") +
  theme_ipsum_rc(grid="") + # retrait du contour des pixels
  theme_enhance_waffle() # retrait des valeurs en axis et ordonnées


#### TEST 2 - Proportion des différentes catégories d'indicateurs dans toute la BDD - wffle_functions + ggplot 2 ####

# ---------- #
# EXAMPLE #
# https://stackoverflow.com/questions/52741666/creating-a-waffle-plot-together-with-facets-in-ggplot2
# ---------- #

data("mtcars")


mtcars$gear_vs <- paste(mtcars$gear, mtcars$vs, sep = "-")
mtcars$carb <- factor(mtcars$carb)
x <- mtcars %>% group_by(carb) %>% dplyr::summarise(value = sum(hp))

waffle_chart(x, fill = "carb", value = "value")

x1 <- mtcars %>% group_by(gear_vs, carb) %>% dplyr::summarise(value = sum(hp))

waffle_chart(x1, fill = "carb", facet = "gear_vs", value = "value")

## You can also scale the waffles to a maximum hp in gear_vs

y <- x1 %>% group_by(gear_vs) %>% dplyr::summarise(value = sum(value))

waffle_chart(x1, fill = "carb", facet = "gear_vs", value = "value", composition = FALSE, max_value = max(y$value))

# ---------- #
# Pour la BDD COLEO #
# ---------- #
waffle_chart(data = indic_count,
             fill = "category",
             value = "freq",
             plot.title = "ESSAI DE TITRE",
             base_size = 20,
             fill_title = "Indicateurs", #character giving the title for the color legend
             fill_colors = as.character(indic_count$cat_coul), #named character vector giving the colors for \code{fill} levels. See \code{\link[ggplot2]{scale_fill_manual}}
             legend.position = "right"#character specifying the position of the legend. See \code{\link[ggplot2]{ggtheme}}
             )


#### TEST 3 - Représentation des catégories présentes sur un site par rapport au reste de la BDD ####

# ----------- #
# Exemple pour le site 135_104_F01 / 136_116_H01 #
# ----------------- #

site_count <- count(as.character(all_obs$category[all_obs$site_code == "136_116_H01"]))
#site_count <- count(as.character(all_obs$category[all_obs$site_code == "136_116_H01"]))
names(site_count)[1] <- "category"
site_count <- dplyr::left_join(site_count, indic_count, by = "category")
names(site_count)[1:4] <- c("category", "freq_site", "freq_tot", "prop_tot")
#site_count$prop_siteVStot <- round(site_count$freq_site/site_count$freq_tot*site_count$prop_tot, digits = 1)

# waffle plot unique pour le site
waffle_chart(data = site_count, fill = "category", value = "freq_site", fill_colors = as.character(site_count$cat_coul))


# ---------- #
# Manipulation des données pour obtenir un plot par indicateur
# --------- #
site_count <- as.data.frame(rbind(as.matrix(site_count[, c(1, 2,5)]), as.matrix(site_count[, c(1, 3, 5)])))
names(site_count)[2] <- "freq"
site_count$data <- c(rep("site", length(unique(site_count$category))), rep("totale", length(unique(site_count$category))))
site_count$freq <- as.numeric(as.character(site_count$freq))
summary(site_count)

# grille avec 1 waffle plot par indicateurs du site
# *** WARNING *** - Problème de couleur
waffle_chart(data = site_count,
             fill = "data",
             facet = "category",
             value = "freq",
             composition = FALSE,
             max_value = sum(site_count$freq[site_count$data == "totale"]),
             base_size = 20,
             plot.title = paste("Proportion des indicateurs sur le site", "135_104_F01", sep = " "),
             fill_title = "Indicateurs",
             fill_colors =  c(as.character(unique(site_count$cat_coul))[1], "grey"))

# Tentative de réglage de beug avec la couleur
d <- site_count[site_count$category == "plantes",]
waffle_chart(data = d, fill = "data", value = "freq")


waffle_chart(data = site_count, fill = "data", facet = "category", value = "freq", composition = TRUE, max_value = NULL, fill_colors = RColorBrewer::brewer.pal(n = length(site_count$category), name = 'Dark2'))


waffle_chart(data = newDF, fill = "data", facet = "category", value = "freq", composition = FALSE, max_value = sum(newDF$freq[newDF$data == "totale"]), fill_colors =  c(as.character(unique(site_count$cat_coul)), "grey"))

# --------------- #
# Plusieurs plot sur une même page
# Multiplot function - http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# --------------- #

# x11()
# par(mfrow = c(2,3))
source("multiplot_functions.R")

# Préparation de la liste avec tous les plots
plot_list <- list()

for (i in 1: length(unique(site_count$category))){
  d <- site_count[site_count$category == unique(site_count$category)[i],]
  plot_list[[i]] <- waffle_chart(data = d,
                     fill = "data",
                     facet = "category",
                     value = "freq",
                     composition = FALSE,
                     max_value = sum(site_count$freq[site_count$data == "totale"]),
                     base_size = 20,
                     fill_colors = c(as.character(unique(d$cat_coul)), "grey"))
}

multiplot(plotlist = plot_list, cols = ceiling(length(plot_list)/2))

#### TEST 4 - Interactive waffle chart  ####
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


#### --------- Interactive pop-up om plot - ggplot + plotly ----------------- ####
library(plotly)

# For temperatures

datn <- meteoCELLSdf[meteoCELLSdf$cell_id == 446 & meteoCELLSdf$indic_meteo == "Temp",]

datn$Month <- factor(datn$Month, levels = datn$Month) # Avoiding ggplot sorts factor levels

#---#
p <- ggplot(data=datn, aes(x=Month, y=Value)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, colour = "grey") +
  geom_area()
  #geom_ribbon(aes(x=Month, ymax=Value, ymin=0), fill="pink", alpha=.5)
  #geom_density(alpha = 0.2, colour = "green")

fig <- ggplotly(p)

fig

#---#
fig <- plot_ly(x = ~datn$Month, y = ~datn$Value, type = 'scatter', mode = 'lines', fill = 'tozeroy')
fig <- fig %>% layout(xaxis = list(title = 'Mois'),
                      yaxis = list(title = 'Température moyenne'))

fig

#---#

fig1 <- plot_ly(x = datn$Month,
                y = datn$Value,
                type = "scatter",
                mode = "lines+markers",
                marker = list(symbol = "circle",
                    color = 'darkorange'),
                line = list(color = "darkorange"),
                fill = "tozeroy",
                fillcolor = "rgba(255,126,0,0.4)" )
# fig1 <- fig1 %>% add_trace(
#   x = datn$Month,
#   y = datn$Value,
#   type = 'scatter',
#   fill = 'tozeroy',
#   color = "darkorange",
#   hoveron = 'points',
#   marker = list(
#     color = 'darkorange'
#   ),
#   line = list(
#     color = 'darkorange'
#   ),
#   text = "Points",
#   hoverinfo = 'x+y'
# ) %>% 
fig1 <- fig1 %>% layout(yaxis = list(title = "Températures moyennes (1989 - 2019)")) %>% 
                        layout(plot_bgcolor = "rgba(254, 247, 234, 0)") %>% 
                        layout(paper_bgcolor = "rgba(254, 247, 234, 0)")
fig1

# For precipitations
datp <- meteoCELLSdf[meteoCELLSdf$cell_id == 446 & meteoCELLSdf$indic_meteo == "Prec",]
datp$Month <- factor(datp$Month, datp$Month)

fig2 <- plot_ly(
  x = datp$Month,
  y = datp$Value,
  yaxis = list(title = "Precip"),
  name = "Précipitations",
  type = "bar",
  opacity = 0.5,
  orientation = "v",
  marker = list(color = "green")
)# %>% layout(yaxis = list(title = "Précipitations cumulées (1989 - 2019)"))
fig2
FIG <- subplot(fig1, fig2, nrows = 2)
FIG
