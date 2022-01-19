library(tidyverse)
library(sf)
library(mapview)
library(tmap)
library(osmextract)

# downdload LONDON

osmextract::oe_match_pattern("London")
region_gl = "Greater London"
tags_needed = c("cycleway",
                "bicycle",
                "wheelchair",
                "kerb",
                "disabled",
                "mobility_scooter",
                "handicap",
                "foot",
                "lit", 
                "access",
                "sidewalk",
                "footway",
                "incline",
                "smoothness",
                "est_width",
                "width",
                "ramp",
                "sidewalk_left",
                "sidewalk_right",
                "ramp_wheelchair",
                "footway_left",
                "footway_right",
                "footway_surface",
                "priority",
                "sidewalk_both_surface",
                "path",
                "pedestrian",
                "capacity_disabled",
                "sidewalk_left_width",
                "sidewalk_right_surface",
                "maxspeed"
                )

gl <- osmextract::oe_get(place = region_gl,
                         layer = "lines",
                         force_download = TRUE,
                         force_vectortranslate = TRUE,
                         extra_tags = tags_needed)

saveRDS(gl,
        "gl.Rds")

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

