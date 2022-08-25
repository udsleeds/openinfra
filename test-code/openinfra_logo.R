#Generate the OpenInfra hex logo. 


# Load libraries --------------------------------------------------------------
library(hexSticker)
library(openinfra)
library(ggplot2)
library(sf)

# Load package data. ----------------------------------------------------------
logo_data = openinfra::sotm_data

# Try see if you can change the spatial buffer to be a Hexagon centred at LCC so that it fits nicely in the hexSticker. 
leeds_centre_point = sf::st_sfc(sf::st_point(c(-1.549, 53.801)), crs = "EPSG:4326")
leeds_centre_buffer = sf::st_buffer(leeds_centre_point, dist = 1750) # radius in m

# Full or buffered data. ------------------------------------------------------
#logo_data_buffed = logo_data
logo_data_buffed = logo_data[leeds_centre_buffer, op = st_within]

# Create ggplot of data. ------------------------------------------------------
logo_geom = logo_data_buffed$geometry 

logo_plt = ggplot2::ggplot(data = logo_geom) + 
  ggplot2::geom_sf(mapping = aes(size = .15)) + # Specify linestring size
  scale_size_identity()


logo_plt = logo_plt + theme_void() # Remove lat long axes 
logo_plt

# Create hexSticker  --=--------------------------------------------------------
s = sticker( logo_plt,
             package="openinfra", p_color = "black", p_x =1, p_y =2, p_size=20, s_x=1, s_y=1, s_width=1.8, s_height=1.8, dpi = 900,
             white_around_sticker = TRUE,
             filename="~openinfra_hex_logo_fullmap.png")
plot(s)
