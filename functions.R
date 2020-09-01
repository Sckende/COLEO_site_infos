get_can_ne <- function() {
  ne_qc <- ne_states("Canada", returnclass = "sf")
  return(ne_qc)
}


# get observations as a df

get_obs_df <- function(){
  obs <- rcoleo::get_obs()
  obs_df <- obs[[1]] %>% map("body") %>% map_df(~ select(.x, -c(closed_at, media)))
  return(obs_df)
}

get_observed_cells <- function(.cells, .obs_df){
  .cells %>%
    semi_join(.obs_df, by = "cell_code")
}

filter_cells_are_observed <- function(.cells){


  .cells %>% filter(cell_code %in% c("135_104", "148_101", "137_111", "141_108", "132_116", "137_107",
    "139_111", "139_103", "142_111", "145_102", "136_116", "48_181",
    "80_175"))
}


plot_map_cells <- function(.ne, .cells, zoom = "all"){
  if(zoom == "montreal") {
    .xlim = c(-74.82,-70.00)
    .ylim = c(45.21,47.5)
  } else {
    .ylim = c(43,62)
    .xlim = c(-80,-53)
  }
  .ne %>%
    ggplot() +
    geom_sf() +
    geom_sf_interactive(aes(tooltip = name, data_id = cell_code), data = .cells, fill = "lightblue", lwd = 1.2) +
    coord_sf(crs = st_crs(.cells), ylim = .ylim, xlim = .xlim) +
    theme_void()
}
