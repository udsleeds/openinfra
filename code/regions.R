# Aim: create maps showing UK transport regions as of 2022

library(tmap)
tmap_mode("view")
# regions_dft_2020 = readRDS("~/cyipt/tempCycleways/regions_dft.Rds")
tm_shape(regions_dft_2020) + tm_polygons("Level")
nrow(regions_dft_2020) # 79
sf::write_sf(regions_dft_2020, "regions_dft_2020.geojson")
# piggyback::pb_upload("regions_dft_2020.geojson")


