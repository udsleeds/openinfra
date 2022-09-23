# LIDA Case Study Plots

library(tmap)
library(dplyr)

tmap::tmap_mode("plot")
data = example_data
dpi = 1000

fig_dir = '/home/james/Desktop/LIDA_CS_figs/'

data = data %>% dplyr::filter(! is.na(highway)) 


# Figure 1 plot -----------------------------------------------------------

figure_1 = tmap::tm_shape(data) + 
  tmap::tm_lines(col = "highway") +
  tmap::tm_layout(main.title = "Default OSM highway values",
                  main.title.position = "centre",
                  main.title.size = 1,
                  legend.outside = TRUE,
                  frame = FALSE,
                  legend.frame = TRUE)

tmap::tmap_save(figure_1, paste0(fig_dir, "Figure 1 Default OSM highway values, 2 km circular buffer around Leeds City Centre.jpg"),
                dpi = dpi)

# Figure 2 plot -----------------------------------------------------------

library(openinfra)
output = openinfra::oi_recode_road_class(data, del = TRUE)

figure_2 = tmap::tm_shape(output) + 
  tmap::tm_lines(col = "openinfra_road_desc") + 
  tmap::tm_add_legend(border.col = "black",
                  border.lwd = 1) + 
  tmap::tm_layout(main.title = "Openinfra recategorised OSM highway values",
                  main.title.position = "centre",
                  main.title.size = 1,
                  legend.outside = TRUE,
                  frame = FALSE,
                  legend.frame = TRUE)

tmap::tmap_save(figure_2, paste0(fig_dir, "Figure 2 Openinfra recategorised OSM highway values, 2 km circular buffer around Leeds City Centre.jpg"),
                dpi = dpi)


# oi_cycle_separation plot ------------------------------------------------

output = openinfra::oi_cycle_separation(data, remove = TRUE)

presentation_figure = tmap::tm_shape(output) + 
  tmap::tm_lines(col = "openinfra_cycle_infra") + 
  tmap::tm_add_legend(border.col = "black",
                      border.lwd = 1) + 
  tmap::tm_layout(main.title = "Openinfra recategorised cycling infrastructure ",
                  main.title.position = "centre",
                  main.title.size = 1,
                  legend.outside = TRUE,
                  frame = FALSE,
                  legend.frame = TRUE)

tmap::tmap_save(presentation_figure, paste0(fig_dir, "Cycling_infra_leeds.jpg"),
                dpi = dpi)
