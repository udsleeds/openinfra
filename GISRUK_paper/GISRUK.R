library(tidyverse)
library(sf)
library(osmextract)
library(tmap)
library(ggplot2)

# GETTING DATA  =================
# piggyback::pb_download("wy.RDS")
# piggyback::pb_download("gm.Rds")
# piggyback::pb_download("mers.Rds")
file.copy("wy.RDS", "GISRUK_paper/wy.Rds")
file.copy("gm.Rds", "GISRUK_paper")
file.copy("mers.Rds", "GISRUK_paper")
wy = readRDS("GISRUK_paper/wy.Rds")
gm = readRDS("GISRUK_paper/gm.Rds")
mers = readRDS("GISRUK_paper/mers.Rds")

# EDA =====================
## WY ========
wy_df = wy %>% 
  sf::st_drop_geometry() %>% 
  filter(!is.na(highway) & highway != "motorway" & highway != "motorway_link") 

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
  filter(n > 10) %>% 
  ungroup()

wy_tagged_grouped %>% select(lit) %>% table


## plot 1 ====
tags_needed = c("bicycle", "foot", "cycleway","footway", "value")
wy_plot1 = wy_tagged_grouped %>% filter(key %in% tags_needed) %>% select(key, tags_needed) %>% 
ggplot( aes(x = value)) +
  geom_bar(position=position_dodge()) +
  facet_wrap(~key, scales = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1)) 

## plot 2 ====
wy_tagged_grouped_prop = wy_tagged_grouped %>% 
  group_by(highway) %>% 
  summarize(n = n(),
            Proportion = n / nrow(wy_df))

wy_plot2 = wy_tagged_grouped_prop %>%  filter(str_detect(highway, "foot|cycle|ped|steps|living"))  %>% 
  ggplot( aes(x = highway,
              y = Proportion)) +
  geom_bar(stat = "identity") 

## plot 3 ====
wy_tagged_grouped_prop2 = wy_tagged_grouped %>% 
  group_by(value, key) %>% 
  summarize(n = n(),
            Proportion = n / nrow(wy_df))

wy_plot3 = wy_tagged_grouped_prop2 %>% filter(key %in% tags_needed) %>% select(key, Proportion) %>% 
  ggplot( aes(x = value,
              y = Proportion)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~key, scales = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1)) 
wy_plot3

## plot 4 =====

tags_needed2 = c("maxspeed", "lit", "kerb", "width")
wy_plot4 = wy_tagged_grouped_prop2 %>% filter(key %in% tags_needed2) %>% select(key, Proportion) %>% 
  ggplot( aes(x = value,
              y = Proportion)) +
  scale_y_continuous(labels = scales::percent) +
  geom_bar(stat = "identity") +
  facet_wrap(~key, scales = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1)) 

wy_plot4

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
  filter(n > 10) %>% 
  ungroup() 
  
### plot 1 ====
tags_needed = c("bicycle", "foot", "cycleway","footway", "value")
gm_plot1 = gm_tagged_grouped %>% filter(key %in% tags_needed) %>% select(key, tags_needed) %>% 
  ggplot( aes(x = value)) +
  geom_bar(position=position_dodge()) +
  facet_wrap(~key, scales = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1)) 
gm_plot1

### plot 2 ====

gm_tagged_grouped_prop = gm_tagged_grouped %>% 
  group_by(highway) %>% 
  summarize(n = n(),
            Proportion = n / nrow(gm_df))

gm_plot2 = gm_tagged_grouped_prop %>%  filter(str_detect(highway, "foot|cycle|ped|steps|living"))   %>% 
  ggplot( aes(x = highway,
              y = Proportion)) +
  scale_y_continuous(labels = scales::percent) +
  geom_bar(stat = "identity") 
gm_plot2

### plot 3 ====

gm_tagged_grouped_prop2 = gm_tagged_grouped %>% 
  group_by(value, key) %>% 
  summarize(n = n(),
            Proportion = n / nrow(gm_df))
gm_plot3 = gm_tagged_grouped_prop2 %>% filter(key %in% tags_needed) %>% select(key, Proportion) %>% 
  ggplot( aes(x = value,
              y = Proportion)) +
  geom_bar(stat = "identity") +
  facet_wrap(~key, scales = "free_x") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1)) 

### plot 4 ====
tags_needed2 = c("maxspeed", "lit", "kerb", "width")
gm_plot4 = gm_tagged_grouped_prop2 %>% filter(key %in% tags_needed2) %>% select(key, Proportion) %>% 
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
  filter(n > 10) %>% 
  ungroup() 

### plot 1 ====
tags_needed = c("bicycle", "foot", "cycleway","footway", "value")
mers_plot1 = mers_tagged_grouped %>% filter(key %in% tags_needed) %>% select(key, tags_needed) %>% 
  ggplot( aes(x = value)) +
  geom_bar(position=position_dodge()) +
  facet_wrap(~key, scales = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1)) 

### plot 2 ====

mers_tagged_grouped_prop = mers_tagged_grouped %>% 
  group_by(highway) %>% 
  summarize(n = n(),
            Proportion = n / nrow(mers_df))

mers_plot2 = mers_tagged_grouped_prop %>%  filter(str_detect(highway, "foot|cycle|ped|steps|living"))   %>% 
  ggplot( aes(x = highway,
              y = Proportion)) +
  scale_y_continuous(labels = scales::percent) +
  geom_bar(stat = "identity") 

### plot 3 ====

mers_tagged_grouped_prop2 = mers_tagged_grouped %>% 
  group_by(value, key) %>% 
  summarize(n = n(),
            Proportion = n / nrow(mers_df))
mers_plot3 = mers_tagged_grouped_prop2 %>% filter(key %in% tags_needed) %>% select(key, Proportion) %>% 
  ggplot( aes(x = value,
              y = Proportion)) +
  geom_bar(stat = "identity") +
  facet_wrap(~key, scales = "free_x") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1)) 

### plot 4 =====

tags_needed2 = c("maxspeed", "lit", "kerb", "width")
mers_plot4 = mers_tagged_grouped_prop2 %>% filter(key %in% tags_needed2) %>% select(key, Proportion) %>% 
  ggplot( aes(x = value,
              y = Proportion)) +
  geom_bar(stat = "identity") +
  facet_wrap(~key, scales = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1)) 

## WY + GM + mers joined =============

all_tagged = rbind(wy_tagged,
                     gm_tagged,
                   mers_tagged)

all_tagged_grouped = all_tagged %>% 
  group_by(key, value, name) %>% 
  mutate(n = n()) %>% 
  filter(n > 10) %>% 
  ungroup() 


### plot 1 ======
all_plot1 = all_tagged_grouped %>% filter(key %in% tags_needed) %>% select(key, tags_needed, name) %>% group_by(value, name, key) %>% summarise(N = n()) %>% 
  ggplot( aes(x = value,
           y = N,
              fill= name)) +
  geom_bar(stat = "identity", 
           position=position_dodge() ) +
  facet_wrap(~key, scales = "free_x") +
  # scale_y_continuous(labels=scales::percent) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1.5, hjust=0.5)) +
  geom_text(aes(label=N), 
            color="black", size=3.5,
            position = position_dodge(1))+
  scale_fill_discrete(name = "Metropolitan counties", labels = c("Greater Manchester", "Merseyside", "West Workshire"))+
  xlab("Tag name")+
  ylab("Frequency")
all_plot1

### plot 2 ======

gm_tagged_grouped_prop_name = gm_tagged_grouped_prop %>% mutate(name = "gm")
wy_tagged_grouped_prop_name = wy_tagged_grouped_prop %>% mutate(name = "wy")
mers_tagged_grouped_prop_name = mers_tagged_grouped_prop %>% mutate(name = "mers")

joined = rbind(gm_tagged_grouped_prop_name,
               wy_tagged_grouped_prop_name,
               mers_tagged_grouped_prop_name)

all_plot2 = joined %>%  filter(str_detect(highway, "foot|cycle|ped|steps|living"))   %>% 
  ggplot(aes(x = highway,
              y = Proportion,
              fill = name)) +
  geom_bar(stat = "identity",
           position=position_dodge() ) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5)) +
  geom_text(aes(label=round(Proportion * 100)), 
            color="black", size=3.5,
            position = position_dodge(1), vjust = 0.02)+
  scale_y_continuous(labels = scales::percent, name = "Percentage") +
  scale_fill_discrete(name = "Metropolitan counties", labels = c("Greater Manchester", "Merseyside", "West Workshire"))+
  xlab("Type of highway")+
  ylab("Proportion")+
  theme(legend.position = "top",
        legend.direction = "horizontal")
all_plot2
saveRDS(all_plot2, "GISRUK_paper/all_plot2.Rds")

### plot 3 ====

gm_tagged_grouped_prop_name2 = gm_tagged_grouped_prop2 %>% mutate(name = "gm")
wy_tagged_grouped_prop_name2 = wy_tagged_grouped_prop2 %>% mutate(name = "wy")
mers_tagged_grouped_prop_name2 = mers_tagged_grouped_prop2 %>% mutate(name = "mers")

joined2 = rbind(gm_tagged_grouped_prop_name2,
               wy_tagged_grouped_prop_name2,
               mers_tagged_grouped_prop_name2)

joined_plot3 = joined2  %>% filter (key %in% tags_needed) %>% 
  ggplot(aes(x = value,
             y = Proportion,
             fill = name)) +
  geom_bar(stat = "identity",
           position=position_dodge() ) + 
  theme_bw() +
  facet_wrap(~key, scales = "free_x") +
  geom_text(aes(label=round(Proportion * 100)), 
            color="black", size=3.5,
            position = position_dodge(1), vjust = 0.02)+
  scale_y_continuous(labels = scales::percent, name = "Percentage") +
  scale_fill_discrete(name = "Metropolitan counties", labels = c("Greater Manchester", "Merseyside", "West Workshire"))+
  xlab("Tag type")+
  ylab("Proportion")+
  theme(legend.position = "top",
        legend.direction = "horizontal")


joined_plot3.1 = joined2  %>% filter (key %in% tags_needed) %>% 
  ggplot(aes(x = value,
             y = n,
             fill = name)) +
  geom_bar(stat = "identity",
           position=position_dodge() ) + 
  theme_bw() +
  facet_wrap(~key, scales = "free_x") +
  geom_text(aes(label=round(n)),
            color="black", size=3.5,
            position = position_dodge(1)
  ) +
  scale_fill_discrete(name = "Metropolitan counties", labels = c("Greater Manchester", "Merseyside", "West Workshire"))+
  xlab("Tag type")+
  ylab("Frequency")+
  theme(legend.position = "top",
        legend.direction = "horizontal")

### plot 4 
tags_needed3 = c("kerb", "sidewalk", "width", "lit")
joined_plot4 = joined2  %>% filter (key %in% tags_needed3) %>% 
  ggplot(aes(x = value,
             y = Proportion,
             fill = name)) +
  geom_bar(stat = "identity",
           position=position_dodge() ) + 
  theme_bw() +
  facet_wrap(~key, scales = "free_x") +
  geom_text(aes(label=round(Proportion * 100)), 
            color="black", size=3.5,
            position = position_dodge(1), vjust = 0.02)+
  scale_y_continuous(labels = scales::percent, name = "Percentage") +
  scale_fill_discrete(name = "Metropolitan counties", labels = c("Greater Manchester", "Merseyside", "West Workshire"))+
  xlab("Tag type")+
  ylab("Proportion")+
  theme(legend.position = "top",
        legend.direction = "horizontal")

joined_plot4.1 = joined2  %>% filter (key %in% tags_needed2) %>% 
  ggplot(aes(x = value,
             y = n,
             fill = name)) +
  geom_bar(stat = "identity",
           position=position_dodge() ) + 
  theme_bw() +
  facet_wrap(~key, scales = "free_x") +
  geom_text(aes(label=round(n)),
            color="black", size=3.5,
            position = position_dodge(1)
  ) +
  scale_fill_discrete(name = "Metropolitan counties", labels = c("Greater Manchester", "Merseyside", "West Workshire"))+
  xlab("Tag type")+
  ylab("Frequency")+
  theme(legend.position = "top",
        legend.direction = "horizontal")




### interactive map 1 =======

tmap_mode("view")
wy_int = wy %>% filter(str_detect(highway, "foot|cycle|ped|steps|living"))
gm_int = gm %>% filter(str_detect(highway, "foot|cycle|ped|steps|living"))
mers_int = mers %>% filter(str_detect(highway, "foot|cycle|ped|steps|living"))

all_join2_int = tm_shape(wy_int)+
  tm_lines("highway")+
  tm_shape(gm_int)+
  tm_lines("highway")+
  tm_shape(mers_int)+
  tm_lines("highway")

tmap_save(all_join2_int,
          filename = "all_join2_int.html")


### interactive maps 2 and 3

wy_int_cat_plot2 = wy %>% 
  mutate(
    bicycle_cat = case_when(
      str_detect(bicycle, "designated|yes")  ~ "yes/designated",
      str_detect(bicycle, "no|dismount")  ~ "no",
      str_detect(bicycle, "destin|permis|priva|unknown")~ "maybe"),

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
    cycleway_cat = case_when(
      str_detect(cycleway, "buff|lane|segr|sep|track") ~ "separate",
      str_detect(cycleway, "no") ~ "no",
      str_detect(cycleway, "share") ~ "shared",
      str_detect(cycleway, "cross|cyclestreet|left|opp|path|side|yes|unm") ~ "other"
    )
  ) %>% select("bicycle_cat", "cycleway_cat", "foot_cat", "footway_cat")

gm_int_cat_plot2 = gm %>% 
  mutate(
    bicycle_cat = case_when(
      str_detect(bicycle, "designated|yes")  ~ "yes/designated",
      str_detect(bicycle, "no|dismount")  ~ "no",
      str_detect(bicycle, "destin|permis|priva|unknown")~ "maybe"),
    
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
    cycleway_cat = case_when(
      str_detect(cycleway, "buff|lane|segr|sep|track") ~ "separate",
      str_detect(cycleway, "no") ~ "no",
      str_detect(cycleway, "share") ~ "shared",
      str_detect(cycleway, "cross|cyclestreet|left|opp|path|side|yes|unm") ~ "other"
    )
  ) %>% select("bicycle_cat", "cycleway_cat", "foot_cat", "footway_cat")

mers_int_cat_plot2 = mers %>% 
  mutate(
    bicycle_cat = case_when(
      str_detect(bicycle, "designated|yes")  ~ "yes/designated",
      str_detect(bicycle, "no|dismount")  ~ "no",
      str_detect(bicycle, "destin|permis|priva|unknown")~ "maybe"),
    
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
    cycleway_cat = case_when(
      str_detect(cycleway, "buff|lane|segr|sep|track") ~ "separate",
      str_detect(cycleway, "no") ~ "no",
      str_detect(cycleway, "share") ~ "shared",
      str_detect(cycleway, "cross|cyclestreet|left|opp|path|side|yes|unm") ~ "other"
    )
  ) %>% filter(!is.na(bicycle_cat) | !is.na(cycleway_cat) | !is.na(foot_cat) | !is.na(footway_cat)) %>% 
  select("bicycle_cat", "cycleway_cat", "foot_cat", "footway_cat")

joined_plot3_int1.1 = tm_shape(wy_int_cat_plot2 )+
  tm_lines("bicycle_cat")+
  tm_shape(gm_int_cat_plot2)+
  tm_lines("bicycle_cat")+
  tm_shape(mers_int_cat_plot2)+
  tm_lines("bicycle_cat")
joined_plot3_int1.2 = tm_shape(wy_int_cat_plot2 )+
  tm_lines("cyclway_cat")+
  tm_shape(gm_int_cat_plot2)+
  tm_lines("cycleway_cat")+
  tm_shape(mers_int_cat_plot2)+
  tm_lines("cycleway_cat")
joined_plot3_int1.3 = tm_shape(wy_int_cat_plot2 )+
  tm_lines("foot_cat")+
  tm_shape(gm_int_cat_plot2)+
  tm_lines("foot_cat")+
  tm_shape(mers_int_cat_plot2)+
  tm_lines("foot_cat")
joined_plot3_int1.4 = tm_shape(wy_int_cat_plot2 )+
  tm_lines("footway_cat")+
  tm_shape(gm_int_cat_plot2)+
  tm_lines("footway_cat")+
  tm_shape(mers_int_cat_plot2)+
  tm_lines("footway_cat")

joined_plot3_int = tmap_arrange(joined_plot3_int1.1, joined_plot3_int1.2,joined_plot3_int1.3, joined_plot3_int1.4,
                                ncol = 2)

tmap_save(joined_plot3_int,
          filename = "joined_plot3_int.html")

