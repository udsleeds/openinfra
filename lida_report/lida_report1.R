# script for LIDA report 1

library(tidyverse)
library(sf)
library(mapview)
library(tmap)
library(osmextract)

# get data

## london

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
                "sidewalk_both_width",
                "path",
                "pedestrian",
                "sidewalk_left_width",
                "sidewalk_right_width",
                "sidewalk_right_surface",
                "sidewalk_left_surface",
                "maxspeed",
                "segregated",
                "sloped_curb",
                "surface",
                "tactile_paving",
                "crossing"
                )

gl = osmextract::oe_get(place = region_gl,
                         layer = "lines",
                         force_download = TRUE,
                         force_vectortranslate = TRUE,
                         extra_tags = tags_needed)

saveRDS(gl,
        "gl-01-04-2022.Rds")

gl = readRDS("gl-01-04-2022.Rds")

## West Yorkshire 
osmextract::oe_match_pattern("west yorkshire")
region_wy = "West Yorkshire"
wy = osmextract::oe_get(place = region_wy,
                         layer = "lines",
                         force_download = TRUE,
                         force_vectortranslate = TRUE,
                         extra_tags = tags_needed)

saveRDS(wy,
        "wy-01-04-2022.Rds")
wy = readRDS("wy-01-04-2022.Rds")

## Merseyside
# osmextract::oe_match_pattern("Merseyside")
region_mers = "Merseyside"
mers = osmextract::oe_get(place = region_mers,
                         layer = "lines",
                         force_download = TRUE,
                         force_vectortranslate = TRUE,
                         extra_tags = tags_needed)
saveRDS(mers,
        "mers-01-04-2022.Rds")

mers = readRDS("mers-01-04-2022.Rds")

## Greater Manchester
osmextract::oe_match_pattern("Greater Manchester")
region_gm = "Greater Manchester"
gm = osmextract::oe_get(place = region_gm,
                          layer = "lines",
                          force_download = TRUE,
                          force_vectortranslate = TRUE,
                          extra_tags = tags_needed)
saveRDS(gm,
        "gm-01-04-2022.Rds")

gm = readRDS("gm-01-04-2022.Rds")

# inclusive_mobility_get function

inclusive_mobility_get = function(osm_sf) {
  osm_sf_im = osm_sf %>% 
    # kerb: flush or not
    dplyr::mutate(im_kerb = dplyr::if_else(kerb == "flush" | kerb == "no", "flush", "other")) %>% 
    # footway is a ‘pavement’ adjacent to a road
    dplyr::mutate(im_footway = dplyr::case_when(
      footway %in% c("left", "right", "both", "sidewalk") |
        sidewalk %in% c("left", "right", "both", "yes", "separate") |
        # trying to capture footways shared with cyclists
        !is.na(cycleway) & # map cycling infrastructure that is an inherent part of the road
        foot %in% c("yes", "designated") |
        segregated %in% "yes"
      ~ "yes",
      TRUE ~ "no" 
    ) 
    ) %>% 
    # footpath is any other right of way for pedestrians, that does not run adjacent to a road.
    dplyr::mutate(im_footpath = dplyr::case_when(
      highway %in% "footway" & 
        im_footway %in% "no" | 
        # not (always) an inherent part of the road
        highway %in% c("cycleway", "bridleway", "path") & # foot = "designated" is implied
        im_footway %in% "no" &
        ! foot %in% c("no", "private") | 
        ! access %in% c("no", "private") &
        segregated %in% "no" # shared space
      ~ "yes",
      TRUE ~ "no"
    )
    ) %>%
    # presence of a crossing: give-way, signal controlled, none, or yes (but the type is unknown)
    dplyr::mutate(im_crossing = dplyr::case_when(
      stringr::str_detect(crossing, "zebra|uncontr|marked")~ "give-way",
      stringr::str_detect(crossing, "toucan|pedex|puffin|equestrian|light|signal")~ "signal-controlled",
      highway %in% "crossing" | footway  %in% "crossing" | !is.na(crossing) ~ "yes",
      TRUE ~ "no"
    )) %>% 
    # implied footways but there's a lack of data to verify
    dplyr::mutate(im_footway_imp = dplyr::case_when(
      im_footway %in% "no" &
        im_footpath %in% "no" &
        im_crossing %in% "no"
      ~ "yes",
      TRUE ~ "no"
    )
    ) %>% 
    # lighting: yes or no
    dplyr::mutate(im_light = dplyr::case_when( 
      # highway %in% "street_lamp" |
      ! lit %in% c("no", "disused") & ! is.na(lit)
      ~ "yes",
      TRUE ~ "no"
    )
    ) %>% 
    # recategorize speed
    dplyr::mutate(im_maxspeed = maxspeed %>% 
                    parse_number(),
                  im_maxspeed =  dplyr::case_when(
                    im_maxspeed > 1 & im_maxspeed <= 20 ~ "1-20", # up to 20 mph
                    im_maxspeed > 20 & im_maxspeed <= 40 ~ "21-40", # 21 - 40 mph
                    im_maxspeed > 40 & im_maxspeed <= 60 ~ "41-60", # 41 - 60 mph
                    im_maxspeed > 60 ~ "61" # over 60 mph
                  )
    ) %>% 
    # tactile paving: yes, no
    dplyr::mutate(im_tactile = dplyr::case_when(
      ! tactile_paving %in% c("no", "incorrect", "bad") & ! is.na(tactile_paving) 
      ~ "yes",
      ! is.na(tactile_paving)
      ~ "no"
    )
    ) %>% 
    # surface: paved, unpaved, or other
    dplyr::mutate(
      im_surface_paved = dplyr::case_when(
        highway %in% "cycleway"
        ~ "paved",
        
        stringr::str_detect(surface,
                            "pav|asph|chipseal|concrete|paving|sett|cobble|metal|wood|stepping")
        ~ "paved",
        highway %in% c("footway", "bridleway") & # highway = footway implied surface value is unpaved
          ! surface %in% stringr::str_detect(surface, "pav|asph|chipseal|concrete|paving|sett|cobble|metal|wood|stepping")
        ~ "unpaved",
        stringr::str_detect(surface, "unpav|compact|gravel|rock|pebble|ground|dirt|grass|mud|sand|woodchips|snow|ice|salt")
        ~ "unpaved",
        TRUE & !is.na(surface) ~ "other"
      )
    ) %>% 
    # surface: even  or not
    dplyr::mutate(im_surface = dplyr::case_when(
      stringr::str_detect(surface, "asph|concrete")
      ~ "even",
      
      im_surface_paved %in% "paved" &
        smoothness %in% c("excellent", "good")
      ~ "even",
      ! is.na(im_surface_paved) 
      ~ "uneven"
    )
    ) %>% 
    # width: under 1.5 meters, 1.5-2 meters, or over 2 meters
    dplyr::mutate(
      im_width =  width %>% 
        parse_number(),
      im_width = case_when(
        im_width > 0 & im_width < 1.5 ~ " 1.5",
        im_width <= 1.5 & im_width <= 2 ~ "1.5 - 2",
        im_width > 2 ~ "2"
      )
    ) %>% 
    # estimated width: under 1.5 meters, 1.5-2 meters, or over 2 meters
    dplyr::mutate(
      im_width_est = est_width %>% 
        parse_number(),
      im_width_est = case_when(
        im_width_est > 0 & im_width_est < 1.5 ~ " 1.5",
        im_width_est <= 1.5 & im_width_est <= 2 ~ "1.5 - 2",
        im_width_est > 2 ~ "2"
      )
    )
}

# clean + recategorise data

gl_df = gl %>% sf::st_drop_geometry() %>% filter(!is.na(highway) & highway != "motorway" & highway != "motorway_link") %>%  inclusive_mobility_get() 
wy_df = wy %>% sf::st_drop_geometry() %>% filter(!is.na(highway) & highway != "motorway" & highway != "motorway_link") %>%  inclusive_mobility_get() 
mers_df = mers %>% sf::st_drop_geometry() %>% filter(!is.na(highway) & highway != "motorway" & highway != "motorway_link") %>%  inclusive_mobility_get() 
gm_df = gm %>% sf::st_drop_geometry() %>% filter(!is.na(highway) & highway != "motorway" & highway != "motorway_link") %>%  inclusive_mobility_get() 

## GM
gm_tagged_im = rbind(
  gm_df %>% filter(!is.na(im_footway)) %>% mutate(key = "footway", value = im_footway, name = "gm"),
  gm_df %>% filter(!is.na(im_footpath)) %>% mutate(key = "footpath", value = im_footpath, name = "gm"),
  gm_df %>% filter(!is.na(im_kerb)) %>% mutate(key = "kerb", value = im_kerb, name = "gm"),
  gm_df %>% filter(!is.na(im_light)) %>% mutate(key = "light", value = im_light, name = "gm"),
  gm_df %>% filter(!is.na(im_maxspeed)) %>% mutate(key = "maxspeed", value = im_maxspeed, name = "gm"),
  gm_df %>% filter(!is.na(im_tactile)) %>% mutate(key = "tactile_paving", value = im_tactile, name = "gm"),
  gm_df %>% filter(!is.na(im_surface)) %>% mutate(key = "surface", value = im_surface, name = "gm")
) 

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
      str_detect(sidewalk, "cross")~ "other")
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
  gm_df_cat %>% filter(!is.na(sidewalk_cat)) %>% mutate(key = "sidewalk", value = sidewalk_cat, name = "gm")
) 

gm_tagged_grouped = gm_tagged %>% 
  group_by(key, value) %>% 
  mutate(n = n()) %>% 
  ungroup() 
gm_tagged_grouped_im = gm_tagged_im %>% 
  group_by(key, value) %>% 
  mutate(n = n()) %>% 
  ungroup() 

# plot 1
gm_tagged_grouped_prop = gm_tagged_grouped %>% 
  group_by(highway) %>% 
  summarize(n = n(),
            Proportion = n / nrow(gm_df))

gm_plot1 = gm_tagged_grouped_prop %>%  filter(str_detect(highway, "foot|cycle|ped|steps|living"))   %>% 
  ggplot( aes(x = highway,
              y = Proportion)) +
  scale_y_continuous(labels = scales::percent) +
  geom_bar(stat = "identity") 
gm_plot1

# plot 2

tags_plot = c("bicycle", "cycleway", "foot", "footway")
gm_tagged_grouped_prop2 = gm_tagged_grouped %>% 
  group_by(value, key) %>% 
  summarize(n = n(),
            Proportion = n / nrow(gm_df))
gm_plot2 = gm_tagged_grouped_prop2 %>% filter(key %in% tags_plot) %>% select(key, Proportion) %>% 
  ggplot( aes(x = value,
              y = Proportion)) +
  geom_bar(stat = "identity") +
  facet_wrap(~key, scales = "free_x") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1)) 
gm_plot2

# plot 3
tags_plot_im = c("footway", "footpath", "kerb", "surface")
gm_tagged_grouped_prop_im = gm_tagged_grouped_im %>% 
  group_by(value, key) %>% 
  summarize(n = n(),
            Proportion = n / nrow(gm_df))
gm_plot3 = gm_tagged_grouped_prop_im %>% filter(key %in% tags_plot_im) %>% select(key, Proportion) %>% 
  ggplot( aes(x = value,
              y = Proportion)) +
  geom_bar(stat = "identity") +
  facet_wrap(~key, scales = "free_x") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1)) 
gm_plot3

## WY
wy_tagged_im = rbind(
  wy_df %>% filter(!is.na(im_footway)) %>% mutate(key = "footway", value = im_footway, name = "wy"),
  wy_df %>% filter(!is.na(im_footpath)) %>% mutate(key = "footpath", value = im_footpath, name = "wy"),
  wy_df %>% filter(!is.na(im_kerb)) %>% mutate(key = "kerb", value = im_kerb, name = "wy"),
  wy_df %>% filter(!is.na(im_light)) %>% mutate(key = "light", value = im_light, name = "wy"),
  wy_df %>% filter(!is.na(im_maxspeed)) %>% mutate(key = "maxspeed", value = im_maxspeed, name = "wy"),
  wy_df %>% filter(!is.na(im_tactile)) %>% mutate(key = "tactile_paving", value = im_tactile, name = "wy"),
  wy_df %>% filter(!is.na(im_surface)) %>% mutate(key = "surface", value = im_surface, name = "wy")
) 

wy_df_cat = wy_df %>% 
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
      str_detect(sidewalk, "cross")~ "other")
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
  wy_df_cat %>% filter(!is.na(sidewalk_cat)) %>% mutate(key = "sidewalk", value = sidewalk_cat, name = "wy")
) 

wy_tagged_grouped = wy_tagged %>% 
  group_by(key, value) %>% 
  mutate(n = n()) %>% 
  ungroup() 
wy_tagged_grouped_im = wy_tagged_im %>% 
  group_by(key, value) %>% 
  mutate(n = n()) %>% 
  ungroup() 

# plot 1
wy_tagged_grouped_prop = wy_tagged_grouped %>% 
  group_by(highway) %>% 
  summarize(n = n(),
            Proportion = n / nrow(wy_df))

wy_plot1 = wy_tagged_grouped_prop %>%  filter(str_detect(highway, "foot|cycle|ped|steps|living"))   %>% 
  ggplot( aes(x = highway,
              y = Proportion)) +
  scale_y_continuous(labels = scales::percent) +
  geom_bar(stat = "identity") 
wy_plot1

# plot 2

tags_plot = c("bicycle", "cycleway", "foot", "footway")
wy_tagged_grouped_prop2 = wy_tagged_grouped %>% 
  group_by(value, key) %>% 
  summarize(n = n(),
            Proportion = n / nrow(wy_df))
wy_plot2 = gm_tagged_grouped_prop2 %>% filter(key %in% tags_plot) %>% select(key, Proportion) %>% 
  ggplot( aes(x = value,
              y = Proportion)) +
  geom_bar(stat = "identity") +
  facet_wrap(~key, scales = "free_x") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1)) 
wy_plot2

# plot 3
tags_plot_im = c("footway", "footpath", "kerb", "surface")
wy_tagged_grouped_prop_im = wy_tagged_grouped_im %>% 
  group_by(value, key) %>% 
  summarize(n = n(),
            Proportion = n / nrow(wy_df))
wy_plot3 = wy_tagged_grouped_prop_im %>% filter(key %in% tags_plot_im) %>% select(key, Proportion) %>% 
  ggplot( aes(x = value,
              y = Proportion)) +
  geom_bar(stat = "identity") +
  facet_wrap(~key, scales = "free_x") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1)) 
wy_plot3

## GL
gl_tagged_im = rbind(
  gl_df %>% filter(!is.na(im_footway)) %>% mutate(key = "footway", value = im_footway, name = "gl"),
  gl_df %>% filter(!is.na(im_footpath)) %>% mutate(key = "footpath", value = im_footpath, name = "gl"),
  gl_df %>% filter(!is.na(im_kerb)) %>% mutate(key = "kerb", value = im_kerb, name = "gl"),
  gl_df %>% filter(!is.na(im_light)) %>% mutate(key = "light", value = im_light, name = "gl"),
  gl_df %>% filter(!is.na(im_maxspeed)) %>% mutate(key = "maxspeed", value = im_maxspeed, name = "gl"),
  gl_df %>% filter(!is.na(im_tactile)) %>% mutate(key = "tactile_paving", value = im_tactile, name = "gl"),
  gl_df %>% filter(!is.na(im_surface)) %>% mutate(key = "surface", value = im_surface, name = "gl")
) 

gl_df_cat = gl_df %>% 
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
  ungroup() 
gl_tagged_grouped_im = gl_tagged_im %>% 
  group_by(key, value) %>% 
  mutate(n = n()) %>% 
  ungroup() 

# plot 1
gl_tagged_grouped_prop = gl_tagged_grouped %>% 
  group_by(highway) %>% 
  summarize(n = n(),
            Proportion = n / nrow(gl_df))

gl_plot1 = gl_tagged_grouped_prop %>%  filter(str_detect(highway, "foot|cycle|ped|steps|living"))   %>% 
  ggplot( aes(x = highway,
              y = Proportion)) +
  scale_y_continuous(labels = scales::percent) +
  geom_bar(stat = "identity") 
gl_plot1

# plot 2

tags_plot = c("bicycle", "cycleway", "foot", "footway")
gl_tagged_grouped_prop2 = gl_tagged_grouped %>% 
  group_by(value, key) %>% 
  summarize(n = n(),
            Proportion = n / nrow(gl_df))
gl_plot2 = gl_tagged_grouped_prop2 %>% filter(key %in% tags_plot) %>% select(key, Proportion) %>% 
  ggplot( aes(x = value,
              y = Proportion)) +
  geom_bar(stat = "identity") +
  facet_wrap(~key, scales = "free_x") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1)) 
gl_plot2

# plot 3
tags_plot_im = c("footway", "footpath", "kerb", "surface")
gl_tagged_grouped_prop_im = gl_tagged_grouped_im %>% 
  group_by(value, key) %>% 
  summarize(n = n(),
            Proportion = n / nrow(gl_df))
gl_plot3 = gl_tagged_grouped_prop_im %>% filter(key %in% tags_plot_im) %>% select(key, Proportion) %>% 
  ggplot( aes(x = value,
              y = Proportion)) +
  geom_bar(stat = "identity") +
  facet_wrap(~key, scales = "free_x") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1)) 
gl_plot3

## Mers
mers_tagged_im = rbind(
  mers_df %>% filter(!is.na(im_footway)) %>% mutate(key = "footway", value = im_footway, name = "mers"),
  mers_df %>% filter(!is.na(im_footpath)) %>% mutate(key = "footpath", value = im_footpath, name = "mers"),
  mers_df %>% filter(!is.na(im_kerb)) %>% mutate(key = "kerb", value = im_kerb, name = "mers"),
  mers_df %>% filter(!is.na(im_light)) %>% mutate(key = "light", value = im_light, name = "mers"),
  mers_df %>% filter(!is.na(im_maxspeed)) %>% mutate(key = "maxspeed", value = im_maxspeed, name = "mers"),
  mers_df %>% filter(!is.na(im_tactile)) %>% mutate(key = "tactile_paving", value = im_tactile, name = "mers"),
  mers_df %>% filter(!is.na(im_surface)) %>% mutate(key = "surface", value = im_surface, name = "mers")
) 

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
      str_detect(sidewalk, "cross")~ "other")
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
  mers_df_cat %>% filter(!is.na(sidewalk_cat)) %>% mutate(key = "sidewalk", value = sidewalk_cat, name = "mers")
) 

mers_tagged_grouped = mers_tagged %>% 
  group_by(key, value) %>% 
  mutate(n = n()) %>% 
  ungroup() 
mers_tagged_grouped_im = mers_tagged_im %>% 
  group_by(key, value) %>% 
  mutate(n = n()) %>% 
  ungroup() 

# plot 1
mers_tagged_grouped_prop = mers_tagged_grouped %>% 
  group_by(highway) %>% 
  summarize(n = n(),
            Proportion = n / nrow(mers_df))

mers_plot1 = mers_tagged_grouped_prop %>%  filter(str_detect(highway, "foot|cycle|ped|steps|living"))   %>% 
  ggplot( aes(x = highway,
              y = Proportion)) +
  scale_y_continuous(labels = scales::percent) +
  geom_bar(stat = "identity") 
mers_plot1

# plot 2

tags_plot = c("bicycle", "cycleway", "foot", "footway")
mers_tagged_grouped_prop2 = mers_tagged_grouped %>% 
  group_by(value, key) %>% 
  summarize(n = n(),
            Proportion = n / nrow(mers_df))
mers_plot2 = mers_tagged_grouped_prop2 %>% filter(key %in% tags_plot) %>% select(key, Proportion) %>% 
  ggplot( aes(x = value,
              y = Proportion)) +
  geom_bar(stat = "identity") +
  facet_wrap(~key, scales = "free_x") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1)) 
mers_plot2

# plot 3
tags_plot_im = c("footway", "footpath", "kerb", "surface")
mers_tagged_grouped_prop_im = mers_tagged_grouped_im %>% 
  group_by(value, key) %>% 
  summarize(n = n(),
            Proportion = n / nrow(mers_df))
mers_plot3 = mers_tagged_grouped_prop_im %>% filter(key %in% tags_plot_im) %>% select(key, Proportion) %>% 
  ggplot( aes(x = value,
              y = Proportion)) +
  geom_bar(stat = "identity") +
  facet_wrap(~key, scales = "free_x") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1)) 
mers_plot3

# All plots joined

all_tagged = rbind(wy_tagged,
                   gm_tagged,
                   mers_tagged,
                   gl_tagged)
all_tagged_im = rbind(wy_tagged_im,
                   gm_tagged_im,
                   mers_tagged_im,
                   gl_tagged_im)

all_tagged_grouped = all_tagged %>% 
  group_by(key, value, name) %>% 
  mutate(n = n()) %>% 
  ungroup() 
all_tagged_grouped_im = all_tagged_im %>% 
  group_by(key, value, name) %>% 
  mutate(n = n()) %>% 
  ungroup() 

# plot 1

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
  scale_fill_discrete(name = "Case Study",
                      labels = c("Greater London", "Greater Manchester", "Merseyside", "West Workshire")
                      )+
  xlab("Type of highway")+
  ylab("Proportion")+
  theme(legend.position = "top",
        legend.direction = "horizontal")
joined_plot1
saveRDS(joined_plot1, "lida_report/joined_plot1.Rds")

# plot 2

gm_tagged_grouped_prop_name2 = gm_tagged_grouped_prop2 %>% mutate(name = "gm")
wy_tagged_grouped_prop_name2 = wy_tagged_grouped_prop2 %>% mutate(name = "wy")
mers_tagged_grouped_prop_name2 = mers_tagged_grouped_prop2 %>% mutate(name = "mers")
gl_tagged_grouped_prop_name2 = gl_tagged_grouped_prop2 %>% mutate(name = "gl")

joined2 = rbind(gm_tagged_grouped_prop_name2,
                wy_tagged_grouped_prop_name2,
                mers_tagged_grouped_prop_name2,
                gl_tagged_grouped_prop_name2)

joined_plot2 = joined2  %>% filter (key %in% tags_plot) %>% 
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
  scale_fill_discrete(name = "Case Study", 
                      labels = c("Greater London", "Greater Manchester", "Merseyside", "West Workshire")
                      )+
  xlab("Tag type")+
  ylab("Proportion")+
  theme(legend.position = "top",
        legend.direction = "horizontal")
joined_plot2

saveRDS(joined_plot2, "lida_report/joined_plot2.Rds")

# plot 2.1

tags_plot1 = c("cycleway", "footway", "kerb", "width")
gm_tagged_grouped_prop_name2 = gm_tagged_grouped_prop2 %>% mutate(name = "gm")
wy_tagged_grouped_prop_name2 = wy_tagged_grouped_prop2 %>% mutate(name = "wy")
mers_tagged_grouped_prop_name2 = mers_tagged_grouped_prop2 %>% mutate(name = "mers")
gl_tagged_grouped_prop_name2 = gl_tagged_grouped_prop2 %>% mutate(name = "gl")

joined2 = rbind(gm_tagged_grouped_prop_name2,
                wy_tagged_grouped_prop_name2,
                mers_tagged_grouped_prop_name2,
                gl_tagged_grouped_prop_name2)

joined_plot2.1 = joined2  %>% filter (key %in% tags_plot1) %>% 
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
  scale_fill_discrete(name = "Case Study", 
                      labels = c("Greater London", "Greater Manchester", "Merseyside", "West Workshire")
  )+
  xlab("Tag type")+
  ylab("Proportion")+
  theme(legend.position = "top",
        legend.direction = "horizontal")
joined_plot2.1

saveRDS(joined_plot2.1, "lida_report/joined_plot2.1.Rds")

# plot 3

gm_tagged_grouped_prop_name_im = gm_tagged_grouped_prop_im %>% mutate(name = "gm")
wy_tagged_grouped_prop_name_im = wy_tagged_grouped_prop_im %>% mutate(name = "wy")
mers_tagged_grouped_prop_name_im = mers_tagged_grouped_prop_im %>% mutate(name = "mers")
gl_tagged_grouped_prop_name_im = gl_tagged_grouped_prop_im %>% mutate(name = "gl")

joined_im = rbind(gm_tagged_grouped_prop_name_im,
                wy_tagged_grouped_prop_name_im,
                mers_tagged_grouped_prop_name_im,
                gl_tagged_grouped_prop_name_im)

joined_plot_im = joined_im  %>% filter (key %in% tags_plot_im) %>% 
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
  scale_fill_discrete(name = "Case Study", 
                      labels = c("Greater London", "Greater Manchester", "Merseyside", "West Workshire")
  )+
  xlab("Tag type")+
  ylab("Proportion")+
  theme(legend.position = "top",
        legend.direction = "horizontal")
joined_plot_im

saveRDS(joined_plot_im, "lida_report/joined_plot_im.Rds")


# 

wy1 = wy %>% 
  filter(!is.na(highway) & highway != "motorway" & highway != "motorway_link") %>%
  inclusive_mobility_get() %>% 
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
      str_detect(sidewalk, "cross")~ "other")
  )


tmap::tmap_mode("view")
tmap::tm_shape(wy1 %>% filter(im_footpath == "yes"))+
  tmap::tm_lines(col = "blue")+
tmap::tm_shape(wy1 %>% filter(im_footway == "yes"))+
  tmap::tm_lines(col = "red") 

