setwd("/home/claire/PostDoc_COLEO/shiny_site_info/SITES_INFOS_tests/COLEO_site_infos")
rm(list = ls())
library(tidyverse)
library(rcoleo)
library(plyr)
library(dplyr)
library(purrr)

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
# ------------------------------ #
#### Fake environmental data ####
# ---------------------------- #
# For each cell
# unique(all_sites$cell_id) = 28, donc création de 28 conditions environnementales différentes

### Fake temperatures - aspect sinusoidal
# variables
n <- 100 # number of data points
t <- seq(from = 0, to = 4*pi, length.out = 100)
a <- 3
b <- 0.5

FakeTemp <- NULL
for(i in 1:28){
  c.unif <- runif(n)
  amp <- round(runif(1, 1, 10))
  col <- i + 1
  y2 <- a*sin(b*t)+c.unif*amp # uniform error
  
  cell_id <- unique(all_sites$cell_id)[i]
  
  j <- data.frame(cell_id, Temp = y2)
  
  FakeTemp <- rbind(FakeTemp, j)

}

### Fake precipitations

FakePrec <- NULL
for(i in 1:28){
  
  p <- runif(12, 0, 120)
  cell_id <- unique(all_sites$cell_id)[i]
  
  k <- data.frame(cell_id, Prec = p)
  
  FakePrec <- rbind(FakePrec, k)
  
}

for(i in unique(FakePrec$cell_id)){
  barplot(FakePrec$Prec[FakePrec$cell_id == i])
}

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
  
  # Année d'observations

all_obs$obs_year <- as.factor(do.call("rbind",strsplit(as.character(all_obs$date_obs), "-"))[,1])


# ---------------- #
###   BROUILLON ###
# -------------- #
