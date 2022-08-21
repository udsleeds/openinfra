# Generate the OpenInfra hex logo. 

library(hexSticker)
library(openinfra)
library(tmap)
library(ggplot2)
library(sf)

logo_data = openinfra::sotm_data

geom_only_logo_plot = plot(logo_data$geometry)


logo_plot = tmap::tm_shape(logo_data) + 
  tmap::tm_lines()
#___________________

logo_geom <- logo_data$geometry 
logo_geom = logo_geom %>% st_crop(logo_geom)

logo_plt <- ggplot2::ggplot(data = logo_geom) + 
  ggplot2::geom_sf()

logo_plt <- logo_plt + theme_void() + theme_transparent()


ggplot(data = logo_data) 

# hexSticker example ------------------------------------------------------
s = sticker( logo_plt,
             package="openinfra", p_color = "black", p_x =1, p_y =2, p_size=20, s_x=1, s_y=1, s_width=1.8, s_height=1.8,
             filename="~openinfra_hex_logo_fullmap.png")
plot(s)


#______________________________________________








imgurl = system.file('/home/james/Desktop/openinfra_logo.png', package="openinfra")

imgurl = "https://user-images.githubusercontent.com/1825120/185739316-4ba87157-aab3-4d92-bf4c-5960e509e0c8.png"

sticker(imgurl, package="openinfra", p_size=20, s_x=1, s_y=.75, s_width=.6,
        filename="/home/james/Desktop/openinfra_hex_logo.png")



s <- sticker(subview = logi,
             package="hexSticker", p_size=20, s_x=.8, s_y=.6, s_width=1.4, s_height=1.2,
             filename="inst/figures/baseplot.png")
plot(s)
