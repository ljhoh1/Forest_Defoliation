require(sp)
require(RColorBrewer)
require(raster)
require(rgdal)
require(giscoR)
require(tmap)
require(ggplot)
require(sf)
require(RColorBrewer)
require(forcats)
require(dplyr)
require(ggplot2)
require(reshape2)

raw_map = read.table("foresthealth.txt", sep = ";", header = T, stringsAsFactors = T)
data_map = raw_map[,!names(raw_map) %in% "X" ]
GER <- gisco_get_nuts(country = "Germany", nuts_level = 1)
BW <- GER[16,]

tree_species_list <- data_map %>%
  group_split(tree_sp_eu)

tree_species = data_map %>%
  distinct(tree_sp_eu)

tree_data = list(tree_species_list, tree_species)

for (spec in 1:dim(tree_species)[1]) {
  tree_data[[spec]] <- tree_species_list[[spec]] %>%
    dplyr::select(x_utm, y_utm, year, tree_age, nbv_ratio, n_trees) %>%
    group_by(year, x_utm, y_utm) %>%
    summarize(tree_age_mean = mean(tree_age, na.rm=TRUE),
              nbv_ratio = mean(nbv_ratio, na.rm=TRUE),
              n_trees = mean(n_trees, na.rm=TRUE)) %>%
    distinct()
}


species <- data_map %>%
  dplyr::select(tree_sp_eu) %>%
  arrange(tree_sp_eu) %>%
  unique() %>%
  pull(tree_sp_eu)

years <- data_map %>%
  dplyr::select(year) %>%
  unique() %>%
  arrange(year) %>%
  pull (year)

for (spec in 1:length(tree_species_list)) {
  n_year = tree_data[[spec]] %>%
    ungroup() %>%
    dplyr::select(year) %>%
    unique() %>%
    pull(year)

  for (year_no in n_year[1: length(n_year) - 1]) {

    dat <- tree_data[[spec]] %>%
      dplyr::select(x_utm, y_utm, year, nbv_ratio) %>%
      mutate(ints = cut(nbv_ratio, breaks = c(0, 0.25, 0.45, 1))) %>%
      filter(year == year_no) %>%
      dplyr::select(-c(nbv_ratio, year))

    dat_3 <- st_as_sf(dat, coords = c('x_utm', 'y_utm'), crs=25832)

    bw_map <- tm_shape(BW) +
      tm_polygons(contrast=.7, id="name", col = "azure") +
      tm_shape(dat_3) +
      tm_dots("ints", palette=c("darkgreen", "darkorange", "deeppink"), scale = 3, title=paste0(year_no), legend.show = FALSE)

    name <- paste0(species[spec], year_no, ".png")
    setwd("C:/Users/ljhoh/OneDrive - University of Edinburgh/03_Dissertation/Forest_Defoliation/Maps")
    tmap_save(bw_map, filename = name)

  }
  dat <- tree_data[[spec]] %>%
    dplyr::select(x_utm, y_utm, year, nbv_ratio) %>%
    mutate(ints = cut(nbv_ratio, breaks = c(0, 0.25, 0.45, 1))) %>%
    filter(year == sapply(list(n_year), tail, 1)) %>%
    dplyr::select(-c(nbv_ratio, year))

  dat_3 <- st_as_sf(dat, coords = c('x_utm', 'y_utm'), crs=25832)

  bw_map <- tm_shape(BW) +
    tm_polygons(contrast=.7, id="name", col = "azure") +
    tm_shape(dat_3) +
    tm_dots("ints", palette=c("darkgreen", "darkorange", "deeppink"), scale = 3,
            title=paste(sapply(list(n_year), tail, 1), "Defoliation", species[[spec]]))

  name <- paste0(species[spec], sapply(list(n_year), tail, 1), ".png")
  setwd("C:/Users/ljhoh/OneDrive - University of Edinburgh/03_Dissertation/Forest_Defoliation/Maps")
  tmap_save(bw_map, filename = name)
}


