# load up libraries
library(tidyverse)
library(sf)
library(tmap)
library(piggyback)

# download datasets
piggyback::pb_download("leeds_walking_ped.RDS")
leeds_walking_ped = readRDS("leeds_walking_ped.RDS") # subset of wy_walking. see code below
# leeds_walking_ped = wy_walking[leeds_pct, op = st_intersects] %>%  # subsetting highways that intersect with Leeds area
#   recode_pedestrian() # creating a new `pedestrian_friendly` 

piggyback::pb_download("leeds_wheelchair_ped_yes.RDS")
leeds_wheelchair_ped_yes = readRDS("leeds_wheelchair_ped_yes.RDS")

piggyback::pb_download("leeds_pct.Rds")
leeds_pct = readRDS("leeds_pct.Rds")

# new datasets
leeds_walking_ped_yes = leeds_walking_ped %>%
  filter(pedestrian_friendly == "yes") %>%
  select(pedestrian_friendly) # an sf object with pedestrian_friendly highways

leeds_walking_ped_maybe = leeds_walking_ped %>% 
  filter(pedestrian_friendly == "maybe") %>% 
  select(pedestrian_friendly) # an sf object with potentially pedestrian_friendly highways

leeds_walking_ped_no = leeds_walking_ped %>% 
  filter(pedestrian_friendly == "no") %>% 
  select(pedestrian_friendly) # an sf object with not pedestrian_friendly highways

# creating tmap objects 
lp_yes = tm_shape(leeds_pct)+
  tm_polygons(col = "foot") + # number of people who commute on foot
  tm_shape(leeds_walking_ped_yes) + # adding pedestrian network
  tm_lines(col = "#084A92") + 
  tm_shape(leeds_wheelchair_ped_yes) + # adding wheelchair=yes tags
  tm_dots(col = "red",
          scale = 3) + # scaling tags up so they are easier to see
  tm_layout(title = "Pedestrian friendly highways") # a tmap object of pedestrian friendly network

lp_maybe = tm_shape(leeds_pct) +
  tm_polygons(col = "foot") +
  tm_shape(leeds_walking_ped_maybe) +
  tm_lines(col = "#006428") +
  tm_layout(title = "Potentially pedestrian friendly highways")

lp_no = tm_shape(leeds_pct)+
  tm_polygons(col = "foot")+
  tm_shape(leeds_walking_ped_no)+
  tm_lines(col = "#A80000") +
  tm_layout(title = "Not pedestrian friendly highways")

# arranging tmap objects
# trying out different layout options
lp_arrange = tmap_arrange(lp_yes, lp_maybe, lp_no,
                          nrow = 2)
# tmap_save(lp_arrange,
#           filename = "lp_arrange.png")

lp_arrange1 = tmap_arrange(lp_yes, lp_maybe)
# tmap_save(lp_arrange1,
#           filename = "lp_arrange1.png")

lp_arrange2 = tmap_arrange(lp_yes, lp_maybe,
                           nrow = 1)
# tmap_save(lp_arrange2,
#           filename = "lp_arrange2.png")

# interactive map
tmap_mode("view")
lp_arrange2_interactive = tmap_arrange(lp_yes, lp_maybe,
                           nrow = 1)
# tmap_save(lp_arrange2_interactive,
#           filename = "lp_arrange2_interactive.png") # using in-built "Export" funcitonality might be preferred
