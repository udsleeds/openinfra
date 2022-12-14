library(tmap)
tmap_mode("view")

u1 = "Bristol_Central_openinfra_OSM_combined_layer.zip"
u2 = "Hackney_Central_openinfra_OSM_combined_layer.zip"
u3 = "Leeds_Central_openinfra_OSM_combined_layer.zip"
u4 = "Leicester_Central_openinfra_OSM_combined_layer.zip"

u1 = paste0("https://github.com/udsleeds/openinfra/releases/download/0.5/", u1)
f1 = basename(u1)
download.file(u1, f1)
unzip(f1)
combined = sf::read_sf("Bristol_Central_openinfra_OSM_combined_layer.geojson")

sum(sf::st_length(combined))
tm_shape(combined) +
  tm_lines()


