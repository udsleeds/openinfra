library(tidyverse)
library(sf)
library(mapview)
library(tmap)
library(osmextract)
library(pct)

# Getting data =====
## Greater London =====

gl = readRDS("gl.Rds")

## West Yorkshire ====

# osmextract::oe_match_pattern("west yorkshire")
# region_wy = "West Yorkshire"
# tags_needed = c("cycleway",
#                 "bicycle",
#                 "wheelchair",
#                 "kerb",
#                 "disabled",
#                 "mobility_scooter",
#                 "handicap",
#                 "foot",
#                 "lit",
#                 "access",
#                 "sidewalk",
#                 "footway",
#                 "incline",
#                 "smoothness",
#                 "est_width",
#                 "width",
#                 "ramp",
#                 "sidewalk_left",
#                 "sidewalk_right",
#                 "ramp_wheelchair",
#                 "footway_left",
#                 "footway_right",
#                 "footway_surface",
#                 "priority",
#                 "sidewalk_both_surface",
#                 "path",
#                 "pedestrian",
#                 "capacity_disabled",
#                 "sidewalk_left_width",
#                 "sidewalk_right_surface",
#                 "maxspeed",
#                 "surface"
# )
#  
# wy <- osmextract::oe_get(place = region_wy,
#                          layer = "lines",
#                          force_download = TRUE,
#                          force_vectortranslate = TRUE,
#                          extra_tags = tags_needed)

wy = readRDS("wy.Rds")

## Greater Manchester ====

# osmextract::oe_match_pattern("manchester")
# region_gm = "Greater Manchester"
# 
# gm <- osmextract::oe_get(place = region_gm,
#                          layer = "lines",
#                          force_download = TRUE,
#                          force_vectortranslate = TRUE,
#                          extra_tags = tags_needed)

gm = readRDS("gm.Rds")

## Merseyside ====

# osmextract::oe_match_pattern("merseyside")
# region_mers = "Merseyside"
# 
# mers <- osmextract::oe_get(place = region_mers,
#                          layer = "lines",
#                          force_download = TRUE,
#                          force_vectortranslate = TRUE,
#                          extra_tags = tags_needed)

mers = readRDS("mers.Rds")

# EDA =====================
## WY ========
wy_df = wy %>% 
  sf::st_drop_geometry() %>% 
  filter(!is.na(highway) & highway != "motorway" & highway != "motorway_link") 

wy_df %>% select(surface) %>% table

wy_df_cat = wy_df %>% 
  mutate(
    bicycle_cat = case_when(
      str_detect(bicycle, "designated|yes")  ~ "yes/designated",
      str_detect(bicycle, "no|dismount")  ~ "no",
      str_detect(bicycle, "destin|permis|priva|unknown")~ "maybe"),
    
    wheelchair_cat = case_when(
      str_detect(wheelchair, "bad|limit|permis") ~ "maybe",
      str_detect(wheelchair, "desig|yes") ~ "yes",
      str_detect(wheelchair, "no") ~ "no"
    ),
    foot_cat = case_when(
      str_detect(foot, "designated|yes")  ~ "yes/designated",
      str_detect(foot, "no|disc")  ~ "no",
      str_detect(foot, "cust|deli|dest|emerg|limit|permis|permit|priv|unknown")  ~ "maybe"
    ),
    footway_cat = case_when(
      str_detect(footway, "access|alley|left|link|traffic") ~ "other",
      str_detect(footway, "no") ~ "no",
      str_detect(footway, "yes|sidewalk") ~ "yes/sidewalk",
    ),
    lit_cat = case_when(
      str_detect(lit, "af|auto|dis|interval|sep|suns|sunr")~ "other",
      str_detect(lit, "limit")~ "limited",
      str_detect(lit, "yes")~ "yes",
      str_detect(lit, "no")~ "no"
    ),
    maxspeed_cl = maxspeed %>% 
      parse_number(),
    maxspeed_cat = case_when(
      maxspeed_cl > 1 & maxspeed_cl <= 20 ~ "1-20 mph",
      maxspeed_cl > 20 & maxspeed_cl <= 40~ "21-40 mph",
      maxspeed_cl > 40 & maxspeed_cl <= 60~ "41-60 mph",
      maxspeed_cl > 60 ~ "61 mph <"
    ),
    cycleway_cat = case_when(
      str_detect(cycleway, "buff|lane|segr|sep|track") ~ "separate",
      str_detect(cycleway, "no") ~ "no",
      str_detect(cycleway, "share") ~ "shared",
      str_detect(cycleway, "cross|cyclestreet|left|opp|path|side|yes|unm") ~ "other"
    ),
    kerb_cat = case_when(
      str_detect(kerb, "fl")~ "flush",
      str_detect(kerb, "low")~ "lowered",
      str_detect(kerb, "raise")~ "raised",
      str_detect(kerb, "yes|no")~ "other"
    ),
    width_cl = width %>% parse_number(),
    width_cat = case_when(
      width_cl > 0 & width_cl < 1.5 ~ "< 1.5m",
      width_cl <= 1.5 & width_cl <= 2 ~ "1.5m - 2m",
      width_cl > 2 ~ "2m <"
    ),
    sidewalk_cat = case_when(
      str_detect(sidewalk, "both")~ "both",
      str_detect(sidewalk, "left")~ "left",
      str_detect(sidewalk, "right")~ "right",
      str_detect(sidewalk, "no|none")~ "no",
      str_detect(sidewalk, "separate")~ "separated",
      str_detect(sidewalk, "yes|mapped")~ "yes",
      str_detect(sidewalk, "cross")~ "other"),
    surface_cat = case_when(
      str_detect(surface, "asphalt")~ "asphalt",
      str_detect(surface, "pave")~ "paved",
      str_detect(surface, "concrete")~ "concrete",
      str_detect(surface, "dirt|mud|ground")~ "dirt",
      str_detect(surface, "grass")~ "grass",
      str_detect(surface, "gravel")~ "gravel",
      str_detect(surface, "stone")~ "stoned",
      !is.na(surface) & TRUE ~ "other",
    ),
    incline_cl = incline %>% parse_number(),
    incline_cat = case_when(
      str_detect(incline, "up")~ "up",
      str_detect(incline, "down")~ "down",
      incline_cl > 0 & incline_cl < 10 ~ "0% - 10%", 
      incline_cl > 10 ~ " 10% <", 
      incline_cl < 0 & incline_cl > -10 ~ "-10% - 0%",
      !is.na(incline) & TRUE ~ "other"
    )
  )


wy_tagged = rbind(
  wy_df_cat %>% filter(!is.na(wheelchair_cat)) %>% mutate(key = "wheelchair", value = wheelchair_cat, name = "wy"),
  wy_df_cat %>% filter(!is.na(foot_cat)) %>% mutate(key = "foot", value = foot_cat, name = "wy"),
  wy_df_cat %>% filter(!is.na(footway_cat)) %>% mutate(key = "footway", value = footway_cat, name = "wy"),
  wy_df_cat %>% filter(!is.na(lit_cat)) %>% mutate(key = "lit", value = lit_cat, name = "wy"),
  wy_df_cat %>% filter(str_detect(highway, "footway|living_street|path|pedestrian|steps|cycle")) %>%
    mutate(key = "highway", value = highway, name = "wy"),
  wy_df_cat %>% filter(!is.na(maxspeed_cat)) %>% mutate(key = "maxspeed", value = maxspeed_cat, name = "wy"),
  wy_df_cat %>% filter(!is.na(bicycle_cat)) %>% mutate(key = "bicycle", value = bicycle_cat, name = "wy"),
  wy_df_cat %>% filter(!is.na(cycleway_cat)) %>% mutate(key = "cycleway", value = cycleway_cat, name = "wy"),
  wy_df_cat %>% filter(!is.na(kerb_cat)) %>% mutate(key = "kerb", value = kerb_cat, name = "wy"),
  wy_df_cat %>% filter(!is.na(width_cat)) %>% mutate(key = "width", value = width_cat, name = "wy"),
  wy_df_cat %>% filter(!is.na(sidewalk_cat)) %>% mutate(key = "sidewalk", value = sidewalk_cat, name = "wy"),
  wy_df_cat %>% filter(!is.na(surface_cat)) %>% mutate(key = "surface", value = surface_cat, name = "wy"),
  wy_df_cat %>% filter(!is.na(incline_cat)) %>% mutate(key = "incline", value = incline_cat, name = "wy")
) 

wy_tagged_grouped = wy_tagged %>% 
  group_by(key, value) %>% 
  mutate(n = n()) %>% 
  filter(n > 10) %>% 
  ungroup()


## plot 1 ====
wy_tagged_grouped_prop = wy_tagged_grouped %>% 
  group_by(highway) %>% 
  summarize(n = n(),
            Proportion = n / nrow(wy_df))

wy_plot1 = wy_tagged_grouped_prop %>%  filter(str_detect(highway, "foot|cycle|ped|steps|living"))  %>% 
  ggplot( aes(x = highway,
              y = Proportion)) +
  geom_bar(stat = "identity") 

## plot 2 ====
wy_tagged_grouped_prop2 = wy_tagged_grouped %>% 
  group_by(value, key) %>% 
  summarize(n = n(),
            Proportion = n / nrow(wy_df))

wy_plot2 = wy_tagged_grouped_prop2 %>% filter(key %in% tags_needed) %>% select(key, Proportion) %>% 
  ggplot( aes(x = value,
              y = Proportion)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~key, scales = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1)) 

## plot 3 ====
tags_needed2 = c("maxspeed", "lit", "kerb", "width")
wy_plot3 = wy_tagged_grouped_prop2 %>% filter(key %in% tags_needed2) %>% select(key, Proportion) %>% 
  ggplot( aes(x = value,
              y = Proportion)) +
  scale_y_continuous(labels = scales::percent) +
  geom_bar(stat = "identity") +
  facet_wrap(~key, scales = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1)) 


## GM ============
gm_df = gm %>% 
  sf::st_drop_geometry() %>% 
  filter(!is.na(highway) & highway != "motorway" & highway != "motorway_link") 

gm_df_cat = gm_df %>% 
  mutate(
    bicycle_cat = case_when(
      str_detect(bicycle, "designated|yes")  ~ "yes/designated",
      str_detect(bicycle, "no|dismount")  ~ "no",
      str_detect(bicycle, "destin|permis|priva|unknown")~ "maybe"),
    
    wheelchair_cat = case_when(
      str_detect(wheelchair, "bad|limit|permis") ~ ",aybe",
      str_detect(wheelchair, "desig|yes") ~ "yes",
      str_detect(wheelchair, "no") ~ "no"
    ),
    foot_cat = case_when(
      str_detect(foot, "designated|yes")  ~ "yes/designated",
      str_detect(foot, "no|disc")  ~ "no",
      str_detect(foot, "cust|deli|dest|emerg|limit|permis|permit|priv|unknown")  ~ "maybe"
    ),
    footway_cat = case_when(
      str_detect(footway, "access|alley|left|link|traffic") ~ "other",
      str_detect(footway, "no") ~ "no",
      str_detect(footway, "yes|sidewalk") ~ "yes/sidewalk",
    ),
    lit_cat = case_when(
      str_detect(lit, "af|auto|dis|interval|sep|suns|sunr")~ "other",
      str_detect(lit, "limit")~ "limited",
      str_detect(lit, "yes")~ "yes",
      str_detect(lit, "no")~ "no"
    ),
    maxspeed_cl = maxspeed %>% 
      parse_number(),
    maxspeed_cat = case_when(
      maxspeed_cl > 1 & maxspeed_cl <= 20 ~ "1-20 mph",
      maxspeed_cl > 20 & maxspeed_cl <= 40~ "21-40 mph",
      maxspeed_cl > 40 & maxspeed_cl <= 60~ "41-60 mph",
      maxspeed_cl > 60 ~ "61 mph <"
    ),
    cycleway_cat = case_when(
      str_detect(cycleway, "buff|lane|segr|sep|track") ~ "separate",
      str_detect(cycleway, "no") ~ "no",
      str_detect(cycleway, "share") ~ "shared",
      str_detect(cycleway, "cross|cyclestreet|left|opp|path|side|yes|unm") ~ "other"
    ),
    kerb_cat = case_when(
      str_detect(kerb, "fl")~ "flush",
      str_detect(kerb, "low")~ "lowered",
      str_detect(kerb, "raise")~ "raised",
      str_detect(kerb, "yes|no")~ "other"
    ),
    width_cl = width %>% parse_number(),
    width_cat = case_when(
      width_cl > 0 & width_cl < 1.5 ~ "< 1.5m",
      width_cl <= 1.5 & width_cl <= 2 ~ "1.5m - 2m",
      width_cl > 2 ~ "2m <"
    ),
    sidewalk_cat = case_when(
      str_detect(sidewalk, "both")~ "both",
      str_detect(sidewalk, "left")~ "left",
      str_detect(sidewalk, "right")~ "right",
      str_detect(sidewalk, "no|none")~ "no",
      str_detect(sidewalk, "separate")~ "separated",
      str_detect(sidewalk, "yes|mapped")~ "yes",
      str_detect(sidewalk, "cross")~ "other"),
    surface_cat = case_when(
      str_detect(surface, "asphalt")~ "asphalt",
      str_detect(surface, "pave")~ "paved",
      str_detect(surface, "concrete")~ "concrete",
      str_detect(surface, "dirt|mud|ground")~ "dirt",
      str_detect(surface, "grass")~ "grass",
      str_detect(surface, "gravel")~ "gravel",
      str_detect(surface, "stone")~ "stoned",
      !is.na(surface) & TRUE ~ "other",
    ),
    incline_cl = incline %>% parse_number(),
    incline_cat = case_when(
      str_detect(incline, "up")~ "up",
      str_detect(incline, "down")~ "down",
      incline_cl > 0 & incline_cl < 10 ~ "0% - 10%", 
      incline_cl > 10 ~ " 10% <", 
      incline_cl < 0 & incline_cl > -10 ~ "-10% - 0%",
      !is.na(incline) & TRUE ~ "other"
    )
  )


gm_tagged = rbind(
  gm_df_cat %>% filter(!is.na(wheelchair_cat)) %>% mutate(key = "wheelchair", value = wheelchair_cat, name = "gm"),
  gm_df_cat %>% filter(!is.na(foot_cat)) %>% mutate(key = "foot", value = foot_cat, name = "gm"),
  gm_df_cat %>% filter(!is.na(footway_cat)) %>% mutate(key = "footway", value = footway_cat, name = "gm"),
  gm_df_cat %>% filter(!is.na(lit_cat)) %>% mutate(key = "lit", value = lit_cat, name = "gm"),
  gm_df_cat %>% filter(str_detect(highway, "footway|living_street|path|pedestrian|steps|cycle")) %>%
    mutate(key = "highway", value = highway, name = "gm"),
  gm_df_cat %>% filter(!is.na(maxspeed_cat)) %>% mutate(key = "maxspeed", value = maxspeed_cat, name = "gm"),
  gm_df_cat %>% filter(!is.na(bicycle_cat)) %>% mutate(key = "bicycle", value = bicycle_cat, name = "gm"),
  gm_df_cat %>% filter(!is.na(cycleway_cat)) %>% mutate(key = "cycleway", value = cycleway_cat, name = "gm"),
  gm_df_cat %>% filter(!is.na(kerb_cat)) %>% mutate(key = "kerb", value = kerb_cat, name = "gm"),
  gm_df_cat %>% filter(!is.na(width_cat)) %>% mutate(key = "width", value = width_cat, name = "gm"),
  gm_df_cat %>% filter(!is.na(sidewalk_cat)) %>% mutate(key = "sidewalk", value = sidewalk_cat, name = "gm"),
  gm_df_cat %>% filter(!is.na(surface_cat)) %>% mutate(key = "surface", value = surface_cat, name = "gm"),
  gm_df_cat %>% filter(!is.na(incline_cat)) %>% mutate(key = "incline", value = incline_cat, name = "gm")
) 

gm_tagged_grouped = gm_tagged %>% 
  group_by(key, value) %>% 
  mutate(n = n()) %>% 
  filter(n > 10) %>% 
  ungroup() 

### plot 1 ====
gm_tagged_grouped_prop = gm_tagged_grouped %>% 
  group_by(highway) %>% 
  summarize(n = n(),
            Proportion = n / nrow(gm_df))

gm_plot1 = gm_tagged_grouped_prop %>%  filter(str_detect(highway, "foot|cycle|ped|steps|living"))   %>% 
  ggplot( aes(x = highway,
              y = Proportion)) +
  scale_y_continuous(labels = scales::percent) +
  geom_bar(stat = "identity") 

### plot 2 ====

gm_tagged_grouped_prop2 = gm_tagged_grouped %>% 
  group_by(value, key) %>% 
  summarize(n = n(),
            Proportion = n / nrow(gm_df))
gm_plot2 = gm_tagged_grouped_prop2 %>% filter(key %in% tags_needed) %>% select(key, Proportion) %>% 
  ggplot( aes(x = value,
              y = Proportion)) +
  geom_bar(stat = "identity") +
  facet_wrap(~key, scales = "free_x") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1)) 

### plot 3 ====
tags_needed2 = c("maxspeed", "lit", "kerb", "width")
gm_plot3 = gm_tagged_grouped_prop2 %>% filter(key %in% tags_needed2) %>% select(key, Proportion) %>% 
  ggplot( aes(x = value,
              y = Proportion)) +
  scale_y_continuous(labels = scales::percent) +
  geom_bar(stat = "identity") +
  facet_wrap(~key, scales = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1)) 

## MERSEYSIDE ===========

mers_df = mers %>% 
  sf::st_drop_geometry() %>% 
  filter(!is.na(highway) & highway != "motorway" & highway != "motorway_link") 

mers_df_cat = mers_df %>% 
  mutate(
    bicycle_cat = case_when(
      str_detect(bicycle, "designated|yes")  ~ "yes/designated",
      str_detect(bicycle, "no|dismount")  ~ "no",
      str_detect(bicycle, "destin|permis|priva|unknown")~ "maybe"),
    
    wheelchair_cat = case_when(
      str_detect(wheelchair, "bad|limit|permis") ~ ",aybe",
      str_detect(wheelchair, "desig|yes") ~ "yes",
      str_detect(wheelchair, "no") ~ "no"
    ),
    foot_cat = case_when(
      str_detect(foot, "designated|yes")  ~ "yes/designated",
      str_detect(foot, "no|disc")  ~ "no",
      str_detect(foot, "cust|deli|dest|emerg|limit|permis|permit|priv|unknown")  ~ "maybe"
    ),
    footway_cat = case_when(
      str_detect(footway, "access|alley|left|link|traffic") ~ "other",
      str_detect(footway, "no") ~ "no",
      str_detect(footway, "yes|sidewalk") ~ "yes/sidewalk",
    ),
    lit_cat = case_when(
      str_detect(lit, "af|auto|dis|interval|sep|suns|sunr")~ "other",
      str_detect(lit, "limit")~ "limited",
      str_detect(lit, "yes")~ "yes",
      str_detect(lit, "no")~ "no"
    ),
    maxspeed_cl = maxspeed %>% 
      parse_number(),
    maxspeed_cat = case_when(
      maxspeed_cl > 1 & maxspeed_cl <= 20 ~ "1-20 mph",
      maxspeed_cl > 20 & maxspeed_cl <= 40~ "21-40 mph",
      maxspeed_cl > 40 & maxspeed_cl <= 60~ "41-60 mph",
      maxspeed_cl > 60 ~ "61 mph <"
    ),
    cycleway_cat = case_when(
      str_detect(cycleway, "buff|lane|segr|sep|track") ~ "separate",
      str_detect(cycleway, "no") ~ "no",
      str_detect(cycleway, "share") ~ "shared",
      str_detect(cycleway, "cross|cyclestreet|left|opp|path|side|yes|unm") ~ "other"
    ),
    kerb_cat = case_when(
      str_detect(kerb, "fl")~ "flush",
      str_detect(kerb, "low")~ "lowered",
      str_detect(kerb, "raise")~ "raised",
      str_detect(kerb, "yes|no")~ "other"
    ),
    width_cl = width %>% parse_number(),
    width_cat = case_when(
      width_cl > 0 & width_cl < 1.5 ~ "< 1.5m",
      width_cl <= 1.5 & width_cl <= 2 ~ "1.5m - 2m",
      width_cl > 2 ~ "2m <"
    ),
    sidewalk_cat = case_when(
      str_detect(sidewalk, "both")~ "both",
      str_detect(sidewalk, "left")~ "left",
      str_detect(sidewalk, "right")~ "right",
      str_detect(sidewalk, "no|none")~ "no",
      str_detect(sidewalk, "separate")~ "separated",
      str_detect(sidewalk, "yes|mapped")~ "yes",
      str_detect(sidewalk, "cross")~ "other"),
    surface_cat = case_when(
      str_detect(surface, "asphalt")~ "asphalt",
      str_detect(surface, "pave")~ "paved",
      str_detect(surface, "concrete")~ "concrete",
      str_detect(surface, "dirt|mud|ground")~ "dirt",
      str_detect(surface, "grass")~ "grass",
      str_detect(surface, "gravel")~ "gravel",
      str_detect(surface, "stone")~ "stoned",
      !is.na(surface) & TRUE ~ "other",
    ),
    incline_cl = incline %>% parse_number(),
    incline_cat = case_when(
      str_detect(incline, "up")~ "up",
      str_detect(incline, "down")~ "down",
      incline_cl > 0 & incline_cl < 10 ~ "0% - 10%", 
      incline_cl > 10 ~ " 10% <", 
      incline_cl < 0 & incline_cl > -10 ~ "-10% - 0%",
      !is.na(incline) & TRUE ~ "other"
    )
  )


mers_tagged = rbind(
  mers_df_cat %>% filter(!is.na(wheelchair_cat)) %>% mutate(key = "wheelchair", value = wheelchair_cat, name = "mers"),
  mers_df_cat %>% filter(!is.na(foot_cat)) %>% mutate(key = "foot", value = foot_cat, name = "mers"),
  mers_df_cat %>% filter(!is.na(footway_cat)) %>% mutate(key = "footway", value = footway_cat, name = "mers"),
  mers_df_cat %>% filter(!is.na(lit_cat)) %>% mutate(key = "lit", value = lit_cat, name = "mers"),
  mers_df_cat %>% filter(str_detect(highway, "footway|living_street|path|pedestrian|steps|cycle")) %>%
    mutate(key = "highway", value = highway, name = "mers"),
  mers_df_cat %>% filter(!is.na(maxspeed_cat)) %>% mutate(key = "maxspeed", value = maxspeed_cat, name = "mers"),
  mers_df_cat %>% filter(!is.na(bicycle_cat)) %>% mutate(key = "bicycle", value = bicycle_cat, name = "mers"),
  mers_df_cat %>% filter(!is.na(cycleway_cat)) %>% mutate(key = "cycleway", value = cycleway_cat, name = "mers"),
  mers_df_cat %>% filter(!is.na(kerb_cat)) %>% mutate(key = "kerb", value = kerb_cat, name = "mers"),
  mers_df_cat %>% filter(!is.na(width_cat)) %>% mutate(key = "width", value = width_cat, name = "mers"),
  mers_df_cat %>% filter(!is.na(sidewalk_cat)) %>% mutate(key = "sidewalk", value = sidewalk_cat, name = "mers"),
  mers_df_cat %>% filter(!is.na(surface_cat)) %>% mutate(key = "surface", value = surface_cat, name = "mers")
) 

mers_tagged_grouped = mers_tagged %>% 
  group_by(key, value) %>% 
  mutate(n = n()) %>% 
  filter(n > 10) %>% 
  ungroup() 

### plot 1 ====

mers_tagged_grouped_prop = mers_tagged_grouped %>% 
  group_by(highway) %>% 
  summarize(n = n(),
            Proportion = n / nrow(mers_df))

mers_plot1 = mers_tagged_grouped_prop %>%  filter(str_detect(highway, "foot|cycle|ped|steps|living"))   %>% 
  ggplot( aes(x = highway,
              y = Proportion)) +
  scale_y_continuous(labels = scales::percent) +
  geom_bar(stat = "identity") 

### plot 2 ====

mers_tagged_grouped_prop2 = mers_tagged_grouped %>% 
  group_by(value, key) %>% 
  summarize(n = n(),
            Proportion = n / nrow(mers_df))
mers_plot2 = mers_tagged_grouped_prop2 %>% filter(key %in% tags_needed) %>% select(key, Proportion) %>% 
  ggplot( aes(x = value,
              y = Proportion)) +
  geom_bar(stat = "identity") +
  facet_wrap(~key, scales = "free_x") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1)) 

### plot 3 =====

tags_needed2 = c("maxspeed", "lit", "kerb", "width")
mers_plot3 = mers_tagged_grouped_prop2 %>% filter(key %in% tags_needed2) %>% select(key, Proportion) %>% 
  ggplot( aes(x = value,
              y = Proportion)) +
  geom_bar(stat = "identity") +
  facet_wrap(~key, scales = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1)) 

## Greater London ====
gl_df = gl %>% 
  sf::st_drop_geometry() %>% 
  filter(!is.na(highway) & highway != "motorway" & highway != "motorway_link")

gl_df_cat = gl_df %>% 
  mutate(
    bicycle_cat = case_when(
      str_detect(bicycle, "designated|yes")  ~ "yes/designated",
      str_detect(bicycle, "no|dismount")  ~ "no",
      str_detect(bicycle, "destin|permis|priva|unknown")~ "maybe"),
    
    wheelchair_cat = case_when(
      str_detect(wheelchair, "bad|limit|permis") ~ "maybe",
      str_detect(wheelchair, "desig|yes") ~ "yes",
      str_detect(wheelchair, "no") ~ "no"
    ),
    foot_cat = case_when(
      str_detect(foot, "designated|yes")  ~ "yes/designated",
      str_detect(foot, "no|disc")  ~ "no",
      str_detect(foot, "cust|deli|dest|emerg|limit|permis|permit|priv|unknown")  ~ "maybe"
    ),
    footway_cat = case_when(
      str_detect(footway, "access|alley|left|link|traffic") ~ "other",
      str_detect(footway, "no") ~ "no",
      str_detect(footway, "yes|sidewalk") ~ "yes/sidewalk",
    ),
    lit_cat = case_when(
      str_detect(lit, "af|auto|dis|interval|sep|suns|sunr")~ "other",
      str_detect(lit, "limit")~ "limited",
      str_detect(lit, "yes")~ "yes",
      str_detect(lit, "no")~ "no"
    ),
    maxspeed_cl = maxspeed %>% 
      parse_number(),
    maxspeed_cat = case_when(
      maxspeed_cl > 1 & maxspeed_cl <= 20 ~ "1-20 mph",
      maxspeed_cl > 20 & maxspeed_cl <= 40~ "21-40 mph",
      maxspeed_cl > 40 & maxspeed_cl <= 60~ "41-60 mph",
      maxspeed_cl > 60 ~ "61 mph <"
    ),
    cycleway_cat = case_when(
      str_detect(cycleway, "buff|lane|segr|sep|track") ~ "separate",
      str_detect(cycleway, "no") ~ "no",
      str_detect(cycleway, "share") ~ "shared",
      str_detect(cycleway, "cross|cyclestreet|left|opp|path|side|yes|unm") ~ "other"
    ),
    kerb_cat = case_when(
      str_detect(kerb, "fl")~ "flush",
      str_detect(kerb, "low")~ "lowered",
      str_detect(kerb, "raise")~ "raised",
      str_detect(kerb, "yes|no")~ "other"
    ),
    width_cl = width %>% parse_number(),
    width_cat = case_when(
      width_cl > 0 & width_cl < 1.5 ~ "< 1.5m",
      width_cl <= 1.5 & width_cl <= 2 ~ "1.5m - 2m",
      width_cl > 2 ~ "2m <"
    ),
    sidewalk_cat = case_when(
      str_detect(sidewalk, "both")~ "both",
      str_detect(sidewalk, "left")~ "left",
      str_detect(sidewalk, "right")~ "right",
      str_detect(sidewalk, "no|none")~ "no",
      str_detect(sidewalk, "separate")~ "separated",
      str_detect(sidewalk, "yes|mapped")~ "yes",
      str_detect(sidewalk, "cross")~ "other")
  )

gl_tagged = rbind(
  gl_df_cat %>% filter(!is.na(wheelchair_cat)) %>% mutate(key = "wheelchair", value = wheelchair_cat, name = "gl"),
  gl_df_cat %>% filter(!is.na(foot_cat)) %>% mutate(key = "foot", value = foot_cat, name = "gl"),
  gl_df_cat %>% filter(!is.na(footway_cat)) %>% mutate(key = "footway", value = footway_cat, name = "gl"),
  gl_df_cat %>% filter(!is.na(lit_cat)) %>% mutate(key = "lit", value = lit_cat, name = "gl"),
  gl_df_cat %>% filter(str_detect(highway, "footway|living_street|path|pedestrian|steps|cycle")) %>%
    mutate(key = "highway", value = highway, name = "gl"),
  gl_df_cat %>% filter(!is.na(maxspeed_cat)) %>% mutate(key = "maxspeed", value = maxspeed_cat, name = "gl"),
  gl_df_cat %>% filter(!is.na(bicycle_cat)) %>% mutate(key = "bicycle", value = bicycle_cat, name = "gl"),
  gl_df_cat %>% filter(!is.na(cycleway_cat)) %>% mutate(key = "cycleway", value = cycleway_cat, name = "gl"),
  gl_df_cat %>% filter(!is.na(kerb_cat)) %>% mutate(key = "kerb", value = kerb_cat, name = "gl"),
  gl_df_cat %>% filter(!is.na(width_cat)) %>% mutate(key = "width", value = width_cat, name = "gl"),
  gl_df_cat %>% filter(!is.na(sidewalk_cat)) %>% mutate(key = "sidewalk", value = sidewalk_cat, name = "gl")
)

gl_tagged_grouped = gl_tagged %>% 
  group_by(key, value) %>% 
  mutate(n = n()) %>% 
  filter(n > 10) %>% 
  ungroup()

## plot 1 ====
gl_tagged_grouped_prop = gl_tagged_grouped %>% 
  # select(highway) %>% 
  group_by(highway) %>% 
  summarize(n = n(),
            Proportion = n / nrow(gl_df))

gl_plot1 = gl_tagged_grouped_prop %>%  filter(str_detect(highway, "foot|cycle|ped|steps|living"))  %>% 
  ggplot( aes(x = highway,
              y = Proportion)) +
  geom_text(aes(label=round(Proportion * 100)), 
            color="black", size=3,
            position = position_dodge(1), vjust = 0.02)+
  scale_y_continuous(labels = scales::percent, name = "Percentage")+
  geom_bar(stat = "identity") 
gl_plot1

## plot 2 ====
gl_tagged_grouped_prop2 = gl_tagged_grouped %>% 
  group_by(value, key) %>% 
  summarize(n = n(),
            Proportion = n / nrow(gl_df))

tags_cf = c("bicycle", "foot", "cycleway","footway", "value")
gl_plot2 = gl_tagged_grouped_prop2 %>% filter(key %in% tags_cf) %>% select(key, Proportion) %>% 
  ggplot( aes(x = value,
              y = Proportion)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=round(Proportion * 100)), 
            color="black", size=3,
            position = position_dodge(1), vjust = 0.02)+
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~key, scales = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=.5)) 
gl_plot2

## plot 3 =====

tags_add = c("maxspeed", "lit", "kerb", "width")
gl_plot3 = gl_tagged_grouped_prop2 %>% filter(key %in% tags_add) %>% select(key, Proportion) %>% 
  ggplot( aes(x = value,
              y = Proportion)) +
  geom_text(aes(label=round(Proportion * 100)), 
            color="black", size=3,
            position = position_dodge(1), vjust = 0.02)+
  scale_y_continuous(labels = scales::percent) +
  geom_bar(stat = "identity") +
  facet_wrap(~key, scales = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1)) 
gl_plot3



## Joined =======
all_tagged = rbind(wy_tagged,
                   gm_tagged,
                   mers_tagged,
                   gl_tagged)

all_tagged_grouped = all_tagged %>% 
  group_by(key, value, name) %>% 
  mutate(n = n()) %>% 
  filter(n > 10) %>% 
  ungroup() 



### plot 1 ======

gm_tagged_grouped_prop_name = gm_tagged_grouped_prop %>% mutate(name = "gm")
wy_tagged_grouped_prop_name = wy_tagged_grouped_prop %>% mutate(name = "wy")
mers_tagged_grouped_prop_name = mers_tagged_grouped_prop %>% mutate(name = "mers")
gl_tagged_grouped_prop_name = gl_tagged_grouped_prop %>% mutate(name = "gl")

joined = rbind(gm_tagged_grouped_prop_name,
               wy_tagged_grouped_prop_name,
               mers_tagged_grouped_prop_name,
               gl_tagged_grouped_prop_name)

joined_plot1 = joined %>%  filter(str_detect(highway, "foot|cycle|ped|steps|living"))   %>% 
  ggplot(aes(x = highway,
             y = Proportion,
             fill = name)) +
  geom_bar(stat = "identity",
           position=position_dodge() ) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5)) +
  geom_text(aes(label=round(Proportion * 100)), 
            color="black", size=3,
            position = position_dodge(1), vjust = 0.02)+
  scale_y_continuous(labels = scales::percent, name = "Percentage") +
  scale_fill_discrete(name = "Metropolitan counties", labels = c("Greater London", "Greater Manchester", "Merseyside", "West Workshire"))+
  xlab("Type of highway")+
  ylab("Proportion")+
  theme(legend.position = "top",
        legend.direction = "horizontal")
joined_plot1
# saveRDS(joined_plot1, "joined_plot1.Rds")
# ggsave("joined_plot1.png",
#        plot = joined_plot1,
#        dpi = 700)

image_read("~/openinfra/joined_plot1.png")

### plot 2 ====

gm_tagged_grouped_prop_name2 = gm_tagged_grouped_prop2 %>% mutate(name = "gm")
wy_tagged_grouped_prop_name2 = wy_tagged_grouped_prop2 %>% mutate(name = "wy")
mers_tagged_grouped_prop_name2 = mers_tagged_grouped_prop2 %>% mutate(name = "mers")
gl_tagged_grouped_prop_name2 = gl_tagged_grouped_prop2 %>% mutate(name = "gl")

joined2 = rbind(gm_tagged_grouped_prop_name2,
                wy_tagged_grouped_prop_name2,
                mers_tagged_grouped_prop_name2,
                gl_tagged_grouped_prop_name2)

joined_plot2 = joined2  %>% filter (key %in% tags_cf) %>% 
  ggplot(aes(x = value,
             y = Proportion,
             fill = name)) +
  geom_bar(stat = "identity",
           position=position_dodge() ) + 
  theme_bw() +
  facet_wrap(~key, scales = "free_x") +
  geom_text(aes(label=round(Proportion * 100)), 
            color="black", size=3,
            position = position_dodge(1), vjust = 0.02)+
  scale_y_continuous(labels = scales::percent, name = "Percentage") +
  scale_fill_discrete(name = "Metropolitan counties", labels = c("Greater London", "Greater Manchester", "Merseyside", "West Workshire"))+
  xlab("Tag type")+
  ylab("Proportion")+
  theme(legend.position = "top",
        legend.direction = "horizontal")
joined_plot2
# saveRDS(joined_plot2, "joined_plot2.Rds")
# ggsave("joined_plot2.png",
#        plot = joined_plot2,
#        dpi = 700)

### plot 3 =====
tags_needed3 = c("kerb", "sidewalk", "width", "lit")
joined_plot3 = joined2  %>% filter (key %in% tags_needed3) %>% 
  ggplot(aes(x = value,
             y = Proportion,
             fill = name)) +
  geom_bar(stat = "identity",
           position=position_dodge() ) + 
  theme_bw() +
  facet_wrap(~key, scales = "free_x") +
  geom_text(aes(label=round(Proportion * 100)), 
            color="black", size=3,
            position = position_dodge(1), vjust = 0.02)+
  scale_y_continuous(labels = scales::percent, name = "Percentage") +
  scale_fill_discrete(name = "Metropolitan counties", labels = c("Greater London", "Greater Manchester", "Merseyside", "West Workshire"))+
  xlab("Tag type")+
  ylab("Proportion")+
  theme(legend.position = "top",
        legend.direction = "horizontal")
joined_plot3
# saveRDS(joined_plot3, "joined_plot3.Rds")

# ggsave("joined_plot3.png",
#        plot = joined_plot3,
#        dpi = 700)


### plot 4 ====

tags_needed4 = c("maxspeed", "surface", "incline", "wheelchair")
joined_plot4 = joined2  %>% filter (key %in% tags_needed4) %>% 
  ggplot(aes(x = value,
             y = Proportion,
             fill = name)) +
  geom_bar(stat = "identity",
           position=position_dodge() ) + 
  theme_bw() +
  facet_wrap(~key, scales = "free_x") +
  geom_text(aes(label=round(Proportion * 100)), 
            color="black", size=3,
            position = position_dodge(1), vjust = 0.02)+
  scale_y_continuous(labels = scales::percent, name = "Percentage") +
  scale_fill_discrete(name = "Metropolitan counties", labels = c("Greater London", "Greater Manchester", "Merseyside", "West Workshire"))+
  xlab("Tag type")+
  ylab("Proportion")+
  theme(legend.position = "top",
        legend.direction = "horizontal")
joined_plot4
# ggsave("joined_plot4.png",
#        plot = joined_plot4,
#        dpi = 700)

# ========
# Let's check out if est_width is used
# let's have a look if est_width is used
gl %>% filter(!is.na(est_width)) %>% nrow()
#> 680
mers %>% filter(!is.na(est_width)) %>% nrow
#> 10
gm %>% filter(!is.na(est_width)) %>% nrow()
#> 17
wy %>% filter(!is.na(est_width)) %>% nrow()
#> 261


## interactive map =====
wy_df_high_foot = wy %>% filter(!is.na(highway)) %>% select(highway)
wy_df_sidewalk = wy %>% filter(!is.na(sidewalk)) %>% 
  mutate(sidewalk_cat = case_when(
    str_detect(sidewalk, "both")~ "both",
    str_detect(sidewalk, "left")~ "left",
    str_detect(sidewalk, "right")~ "right",
    str_detect(sidewalk, "no|none")~ "no",
    str_detect(sidewalk, "separate")~ "separated",
    str_detect(sidewalk, "yes|mapped")~ "yes",
    str_detect(sidewalk, "cross") & TRUE~ "other"))

tmap_mode("view")
wy_foot_side_interactive = tm_shape(wy_df_high_foot)+
  tm_lines(col = "black")+
  tm_shape(wy_df_sidewalk)+
  tm_lines("sidewalk_cat",
           palette = "Set1",
           lwd = 2)

tmap_save(wy_foot_side_interactive, 
          "wy_foot_side_interactive.png",
          dpi = 700)
tmap_save(wy_foot_side_interactive, 
          "wy_foot_side_interactive.html",
          dpi = 700)

## regions from pct 
# pct::pct_regions %>% pull(region_name)

wy_pct = pct::get_pct(region = "west-yorkshire",
                      layer = "z",
                      purpose = "commute",
                      geography = "lsoa")
gm_pct = pct::get_pct(region = "greater-manchester",
                      layer = "z",
                      purpose = "commute",
                      geography = "lsoa")
liv_pct = pct::get_pct(region = "liverpool-city-region",
                      layer = "z",
                      purpose = "commute",
                      geography = "lsoa")
lond_pct = pct::get_pct(region = "london",
                      layer = "z",
                      purpose = "commute",
                      geography = "lsoa")


wy_pct_geom = wy_pct %>% select(geo_code) %>% st_combine
gm_pct_geom = gm_pct %>% select(geo_code) %>% st_combine
liv_pct_geom = liv_pct %>% select(geo_code) %>% st_combine
lond_pct_geom = lond_pct %>% select(geo_code) %>% st_combine

# tmap_options(check.and.fix = TRUE)

tmap_mode("view")
tm_shape(wy_pct_geom)+
  tm_polygons(col = "blue")+
  tm_shape(gm_pct_geom)+
  tm_polygons(col = "yellow")+
  tm_shape(liv_pct_geom)+
  tm_polygons(col = "red")+
  tm_shape(lond_pct_geom)+
  tm_polygons(col = "black")
  

wy_foot_bi = wy %>% filter(highway == "footway", bicycle == "designated")
wy_foot_bi %>% nrow
wy_cycle_foot = wy %>% filter(highway == "cycleway", foot == "designated") 

tm_shape(wy_cycle_foot)+
  tm_lines(lwd = 5)

tm_shape(wy_foot_bi)+
  tm_lines(lwd = 5)


