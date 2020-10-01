setwd("/home/claire/PostDoc_COLEO/shiny_site_info/SITES_INFOS_tests/COLEO_site_infos")
rm(list = ls())
library(tidyverse)
library(rcoleo)
library(plyr)
library(dplyr)
library(purrr)
library(jsonlite)
library(geojsonio)
library(sf)

source("functions.R")


# ----------------------------------------------------- #
#### Obtention des informations pour tous les sites ####
# --------------------------------------------------- #


all_sites <- get_sites()[[1]][[1]][[1]]
#utils::View(all_sites)
names(all_sites)

# ----------------------------------------------------------- #
#### Obtention des coordonnées des sites d'échantillonnage ####
# ----------------------------------------------------------- #

#utils::View(all_sites %>% select(
#  geom.coordinates))
all_sites$geom.coordinates[[1]]
k <- as.data.frame(do.call("rbind", all_sites$geom.coordinates))
k
all_sites <- all_sites %>% 
  mutate(long_site = do.call("rbind", all_sites$geom.coordinates)[,1],
         lat_site = do.call("rbind", all_sites$geom.coordinates)[,2])
names(all_sites)

# Obtention de l'année d'ouverture des sites
all_sites$open_year <- do.call("rbind",strsplit(all_sites$opened_at, "-"))[,1]

# Association d'une couleur par type d'échantillonnage
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
                             "<b> type_echantillonnage</b> ",
                             type,
                             "<br/>",
                             "<b> annee_creation</b> ",
                             open_year))

# ------------------------------- #
#### Observations des especes ####
# ----------------------------- #

# via get_gen() directement - Plus rapide
observations <- rcoleo::get_gen("/observations")
observations <- do.call("rbind.fill", observations[[1]])
observations <- observations[, c(1, 2, 4, 12, 20:22)]

# Association du type de campagne pour chaque observations
  # Préparation du DF campaigns
campaigns <- rcoleo::get_gen("/campaigns") # Identique à rcoleo::get_campaigns()
campaigns <- do.call("rbind.fill", campaigns[[1]])
campaigns <- campaigns[, c(1:3, 5, 6)]
names(campaigns)[c(1, 3:5)] <- c("campaign_id", "campaign_type", "campaign_opened_at", "campaign_closed_at")

  # Tables joining
obsCamp <- dplyr::left_join(observations, campaigns, by = "campaign_id")
summary(obsCamp)

obsCamp <-cbind(obsCamp[,c(1, 4, 7, 8)],(apply(obsCamp[, c(2, 3, 5, 6, 9:11)], 2, as.factor)))
summary(obsCamp)

# Association avec le type d'indicateurs (= species category)
getSpecies <- rcoleo::get_species() 
getSpecies <- do.call("rbind.fill", getSpecies[[1]])

#all(unique(obsCamp$obs_species.taxa_name) %in% unique(getSpecies$name))

names(obsCamp)[7] <- "name"
obsCampCat <- dplyr::left_join(obsCamp, getSpecies[, 1:4], by = "name")
obsCampCat <- cbind(obsCampCat[, c(1:6, 8:11)], (apply(obsCampCat[, c(7, 12:14)], 2, as.factor)))

# Association du nom du site où l'observation a eu lieu
names(all_sites)[1] <- "site_id"
obsCampCat <- left_join(obsCampCat, all_sites[, c(1, 4:6, 13:15, 20:24)], by = "site_id")

all_obs <- cbind(obsCampCat[, c(1:14, 18, 21, 22, 24, 25)], (apply(obsCampCat[, c(15:17, 19, 20, 23)], 2, as.factor)))

# Nettoyage du DF
  # Noms de variables
names(all_obs)[c(3, 7, 15, 21:25)] <- c("species_value", "species_value_type", "cell_id", "hab_type", "site_open_at", "cell_name", "cell_code", "site_open_year")
all_obs$hab_type <- as.character(all_obs$hab_type)
  
  # Année d'observations

all_obs$obs_year <- as.factor(do.call("rbind",strsplit(as.character(all_obs$date_obs), "-"))[,1])


  # Couleur par type d'indicateur
cat_unik <- as.character(unique(all_obs$category))
cat_unik <- cat_unik[!is.na(cat_unik)] # Retrait des NA

RColorBrewer::display.brewer.pal(n = length(cat_unik), name = 'Dark2')
RColorBrewer::display.brewer.pal(n = length(cat_unik), name = 'Spectral')

    # Spécification de couleur hexadécimale 


RColorBrewer::brewer.pal(n = length(cat_unik), name = "Dark2")

coul <- data.frame(RColorBrewer::brewer.pal(n = length(cat_unik), name = "Dark2"), sort(cat_unik))
names(coul) <- c("cat_coul", "category")
all_obs <-  dplyr::left_join(all_obs, coul, by = "category")

# ------------------------------------------------ #
#### Compte des différents types d'indicateurs ####
# ---------------------------------------------- #

indic_count <- plyr::count(as.character(all_obs$category))
indic_count$prop <- round((indic_count$freq * 100)/sum(indic_count$freq), digits = 0)
names(indic_count)[1] <- "category"
indic_count <- na.omit(indic_count)
indic_count <- dplyr::left_join(indic_count, coul, by = "category")

# ------------------------------------------------ #
#### Formatage des coord. des cellules --> geoJson pour extraction des données météo ####
# Via earthmap.org - https://earthmap.org/
# ---------------------------------------------- #

  # Obtention de toutes les cellules
cells <- rcoleo::get_gen("/cells")
cells <- do.call("rbind.fill", cells$body)
str(cells)
names(cells)
  # Conversion des polygones en spatial feature
geom <- rcoleo::cl_to_sf(cells[,c(1, 7, 8)])
names(geom)[1] <- "name"
  # Conversion en format JSON
coordGEOJSON <- geojson_json(geom)
#geojson_write(coordGEOJSON, file = "cellsCOORD.geojson")# Writing geojson file in the current directory

  # Obtention des cellules uniquement présentes dans le prototype de la shiny app
#cells : 134 198 151 165 119 149 160 157 169 186 141 446 495
uniq_cells <- unique(all_obs$cell_id)
coord_app_geojson <- geom[geom$name %in% uniq_cells,]

coord_app_geojson <- geojson_json(coord_app_geojson)
#geojson_write(coord_app_geojson, file = "ShinycellsCOORD.geojson") # Writing geojson file in the current directory

# ------------------------------ #
#### Environmental data uniquement pour les cellules dans le prototype du TdeB ####
# ---------------------------- #

meteoCELLS <- list.files(path = "/home/claire/PostDoc_COLEO/shiny_site_info/SITES_INFOS_tests/COLEO_site_infos/data_meteo_cells/", full.names = TRUE)
nchar("/home/claire/PostDoc_COLEO/shiny_site_info/SITES_INFOS_tests/COLEO_site_infos/data_meteo_cells//") # For counting the nimber of characters in the path = 96
lapply(funRcoleo, source)

meteoCELLSdf <- data.frame()
for(i in 1:length(meteoCELLS)){
  cell_id <- substr(meteoCELLS[[i]], start = 97, stop = 99)
  indic_meteo <- substr(meteoCELLS[[i]], start = 101, stop = 104)
  tab <- readr::read_csv(meteoCELLS[[i]], col_names = FALSE, skip = 2)
  tab$cell_id <- cell_id
  tab$indic_meteo <- indic_meteo
  
  meteoCELLSdf <- rbind(meteoCELLSdf, tab)
}

names(meteoCELLSdf)[c(1, 2)] <- c("Month", "Value")
meteoCELLSdf$indic_meteo[meteoCELLSdf$indic_meteo == "Mean"] <- "Temp"

meteoCELLSdf$Month <- as.factor(meteoCELLSdf$Month)
meteoCELLSdf$cell_id <- as.numeric(meteoCELLSdf$cell_id)
meteoCELLSdf$indic_meteo <- as.factor(meteoCELLSdf$indic_meteo)

summary(meteoCELLSdf)
# ---------------- #
###   BROUILLON ###
# -------------- #

# ------------------------------ #
#### Fake environmental data ####
# ---------------------------- #
# For each cell
# unique(all_sites$cell_id) = 28, donc création de 28 conditions environnementales différentes

### Fake temperatures - aspect sinusoidal
# variables
# n <- 100 # number of data points
# t <- seq(from = 0, to = 4*pi, length.out = 100)
# a <- 3
# b <- 0.5
# 
# FakeTemp <- NULL
# for(i in 1:28){
#   c.unif <- runif(n)
#   amp <- round(runif(1, 1, 10))
#   col <- i + 1
#   y2 <- a*sin(b*t)+c.unif*amp # uniform error
#   
#   cell_id <- unique(all_sites$cell_id)[i]
#   
#   j <- data.frame(cell_id, Temp = y2)
#   
#   FakeTemp <- rbind(FakeTemp, j)
#   
# }
# 
# ### Fake precipitations
# 
# FakePrec <- NULL
# for(i in 1:28){
#   
#   p <- runif(12, 0, 120)
#   cell_id <- unique(all_sites$cell_id)[i]
#   
#   k <- data.frame(cell_id, Prec = p)
#   
#   FakePrec <- rbind(FakePrec, k)
#   
# }
# 
# for(i in unique(FakePrec$cell_id)){
#   barplot(FakePrec$Prec[FakePrec$cell_id == i])
# }
# 
