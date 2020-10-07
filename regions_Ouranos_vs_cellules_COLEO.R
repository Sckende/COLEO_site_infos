# Travvaux sur objets spatiaux #
# Extraction pour chaque cellule de COLEO, la région administrative (modifiées par Ouranos) #
# https://www.ouranos.ca/portraits-climatiques/#/ #

library(sf)
library(raster)
library(geojsonio)
library(rcoleo)
library(dplyr)


setwd("/home/claire/PostDoc_COLEO/shiny_site_info/SITES_INFOS_tests/COLEO_site_infos/data_ouranos")

# sf objects
reg <- geojson_sf("/home/claire/PostDoc_COLEO/shiny_site_info/SITES_INFOS_tests/COLEO_site_infos/data_ouranos/regions_simplified_Ouranos.geojson") # régions du Québec modifiées par Ouranos
cellJSON <- geojsonio::geojson_sf("/home/claire/PostDoc_COLEO/shiny_site_info/SITES_INFOS_tests/COLEO_site_infos/cellsCOORD.geojson") # toutes les cellules existantes dans COLEO
cellSHINY <- geojsonio::geojson_sf("/home/claire/PostDoc_COLEO/shiny_site_info/SITES_INFOS_tests/COLEO_site_infos/ShinycellsCOORD.geojson") # Cellules actuellement utilisées dans le TdeB "description des sites"


plot(st_geometry(reg), col = "white", border = "grey")
plot(st_geometry(cellSHINY), border = "darkorange" , add = TRUE)
# 
# class(reg)
# class(cellJSON)
# class(cellSHINY)

st_is_valid(reg)
#st_intersects(reg, cellJSON)
#j <- st_intersects(reg, cellSHINY)


st_centroid(cellSHINY)

plot(st_geometry(st_centroid(cellSHINY)),col = "red" , add = TRUE)

cent <- st_intersects(reg, st_centroid(cellSHINY))

RegCellsShiny <- data.frame()
for (i in 1:length(cent)){
  if(length(cent[[i]]) != 0){
    c <- data.frame(id = rep(i, length(cent[[i]])), cell_num = cent[[i]])
    RegCellsShiny <- rbind(RegCellsShiny, c)
  }
}

RegCellsShiny <- dplyr::left_join(RegCellsShiny, reg[, c(2, 4)], by = "id")
RegCellsShiny <- RegCellsShiny[,-4]

#cellSHINY <- as.data.frame(cellSHINY)

cellSHINY$cell_num <- 1:length(cellSHINY$name)
RegCellsShiny <- dplyr::left_join(RegCellsShiny, cellSHINY[, -2], by = "cell_num")
RegCellsShiny <- RegCellsShiny[, -5]
names(RegCellsShiny)[4] <- "cell_id"


#### ------ Récupération des scénarios climatiques dépendemment des régions ----- ####
scenario_meteo <- data.frame()
for (i in unique(RegCellsShiny$Region)){
  files <- list.files("/home/claire/PostDoc_COLEO/shiny_site_info/SITES_INFOS_tests/COLEO_site_infos/data_ouranos",
                  pattern = i,
                  full.names = TRUE)
  for (j in files){
    data <- read.csv(j, header = TRUE)
    if(stringr::str_detect(j, "précipitations")){
      param <- "prec"
    }else{
      param <- "temp"
    }
    data$param_met <- param
    data$Region <- i
    scenario_meteo <- rbind(scenario_meteo, data)
  }

}

head(scenario_meteo)
summary(scenario_meteo)

