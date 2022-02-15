# AIMS:
# 1. Explore collideoscope data
# 2. Find out where collisions happened in Leeds
# 3. Find out if the collisions happened where cycling infrastructure is present.

library(tidyverse)
library(sf)
library(osmextract)
library(pct)
library(tmap)

collid = read.csv("/Users/gretatimaite/Desktop/collideoscope-data.csv") # change the path as needed

collid %>% glimpse()

# let's check how data is coded
collid$Category[1:10]
collid$Title[1:10]
collid$Participants[1:10]
# case 8 looks interesting.
collid$Title[8]
#> [1] "Near miss involving a bicycle and a motor vehicle"
collid$Participants[8]
#> [1] "other"
collid$Participants %>% table()
# what does this "other" category capture? There's a category for cars, HGV, and motorcycles.
# Is it public transport, such as buses?

# We have coordinates, so let's turn plot the locations of accidents
collid_sf = sf::st_as_sf(collid,
                         coords = c("Longitude", "Latitude"),
                         crs = st_crs(4326))

tmap_mode("view")
collid_sf %>%
  tmap::qtm()

# There are quite a few accidents reported in West Yorkshire and Leeds in particular. Let's figure out how many 

# first we need to have a geography of Leeds defined.
wy_pct = pct::get_pct_zones(region = "west-yorkshire")

# Now we can subset Leeds
wy_pct %>% select(lad_name) %>% plot
leeds_pct = wy_pct %>% 
  filter(lad_name == "Leeds")
# Let's check
leeds_pct$geometry %>% plot

# susbet collisions that happened in Leeds
collid_leeds = collid_sf[leeds_pct, op = sf::st_within]
collid_leeds %>% qtm()

collid_leeds %>% nrow()
#> [1] 60

# So we have 60 collisions that were self-reported in Leeds over the duration of the collideoscope project.
# Let find out in which year the highest number of collisiosn was reported.

collid_leeds = collid_leeds %>% 
  mutate(year = lubridate::year(Confirmed))
# a quick check
collid_leeds$year[1:5]
collid_leeds$Confirmed[1:5]

# grouping
collid_leeds_grouped = collid_leeds %>% 
  group_by(year) %>% 
  summarize(n_year = n()) %>% 
  arrange(desc(n_year))

# in 2018 there was the highest number of self-reported collisions
# interesting that there are no data for 2020 or 2021 (when the project was closed)

ggplot2::ggplot(collid_leeds_grouped,
                aes(x = year,
                    y = n_year))+
  geom_col()

# Finally, let's have a look if these accidents happened where cycling lanes and/or tracks are present.
# For this we'll need OSM data. To reduce the need for computational power, we'll focus only on central Leeds.

url = "https://github.com/udsleeds/openinfra/releases/download/v0.1/leeds_central_15-02-2022.RDS"
download.file(url, 
              destfile = "leeds_central_15-02-2022.RDS") # downloading the file to the working directory; if you do not know where your working directory is, type `getwd()` to your console.

leeds_central = readRDS("leeds_central_15-02-2022.RDS") # reading the file

# overlapping two datasets
sf::st_geometry(leeds_central) %>% plot()
collid_leeds$geometry %>% plot(col = "red", add = TRUE)

leeds_central_cycle = leeds_central %>% 
  filter(highway == "cycleway") 

tm_shape(leeds_central_cycle)+
  tm_lines("highway")

# plot
tm_shape(collid_leeds)+
  tm_dots()+
  tm_shape(leeds_central_cycle)+
  tm_lines("highway")

# we'll need a buffer around cycleways
leeds_central_cycle_buffer = sf::st_buffer(leeds_central_cycle,
                                           dist = 6)
tm_shape(collid_leeds)+
  tm_dots()+
  tm_shape(leeds_central_cycle_buffer)+
  tm_polygons("highway")


collid_lc_cycle = collid_leeds[leeds_central_cycle_buffer, op=sf::st_within]
collid_lc_cycle %>% nrow()
#> 2

collid_lc_cycle %>% sf::st_drop_geometry() 
# We have two accidents on cycleways: one near miss involving a car and one slight incident with a pedestrian (both in 2015).

# plot
tm_shape(leeds_central_cycle_buffer)+
  tm_polygons("highway")+
tm_shape(collid_lc_cycle)+
  tm_dots(col = "red")


