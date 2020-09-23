setwd("/home/claire/PostDoc_COLEO/shiny_site_info/SITES_INFOS_tests/COLEO_site_infos")

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

# Modification de la variable "created_at" pour obtenir l'année de création des sites
all_sites <- all_sites %>% 
  separate(created_at, c("Y_creation", "M_creation", "others_creation"), sep = "-")

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
                             Y_creation))
# ------------------------------- #
#### Observations des especes ####
# ----------------------------- #

# Utilisation de la fonction de Andrew get_obs_df() - modifiée via retrait de la variable "media", qui est une liste et qui beug avec la fonction xtable(xtable) de la fonction renderTable(Shiny)

all_obs <-get_obs_df()
names(all_obs)
str(all_obs)
dim(all_obs)

# Vérification si tous les codes des sites pour les especes observees sont contenus dans la liste de codes de tous les sites existants
all(unique(all_obs$site_code) %in% unique(all_sites$site_code))

# Modification de la variable "date_obs" pour obtenir l'année des observations effectuées
all_obs <- all_obs %>% 
  separate(date_obs, c("Y_obs", "M_obs", "DAY_obs"), sep = "-")

# Récupération des "cell_id" pour chaque "site_code"
all_obs <- dplyr::left_join(all_obs, all_sites[, c(2, 4)], by = "site_code")


# Récupération des lat/long, infos pop-up & type d'échantillonnage de chaque site dans le DF des observations

j <- all_sites %>% select(site_code, lat_site, long_site, popup_info, type_ech = type, col)
all_obs <- left_join(all_obs, j, by = "site_code")

# Vérification si tous les ensembles de codes des sites, lat & long pour les observations sont contenus dans les ensembles de codes de tous les sites, lat et long par sites
x1 <- NULL
for(i in unique(all_obs$site_code)){
  l <- paste(i, unique(all_obs$long_site[all_obs$site_code == i]), unique(all_obs$lat_site[all_obs$site_code == i]))
  x1 <- c(x1, l)
}

x2 <- NULL
for(i in all_sites$site_code){
  l <- paste(i, unique(all_sites$long_site[all_sites$site_code == i]), unique(all_sites$lat_site[all_sites$site_code == i]))
  x2 <- c(x2, l)
}

all(x1 %in% x2) # All is fine !

#### Fake environmental data ####
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

#### Indicateurs (= species category) ####

# Association du type de campagne pour chaque observations
observations <- rcoleo::get_gen("/observations")
observations <- do.call("rbind.fill", observations[[1]])
observations <- observations[, c(1, 2, 4, 12, 20:22)]

getCampaigns <- rcoleo::get_campaigns()
getCampaigns <- do.call("rbind.fill", getCampaigns[[1]])

names(getCampaigns)[1] <- "campaign_id"
obsCamp <- dplyr::left_join(observations, getCampaigns, by = "campaign_id")
summary(as.factor(obsCamp$type))

# Association du nom du site où l'observation a eu lieu
# Association de la catégorie de l'espèce observée

# Nettoyage du DF


