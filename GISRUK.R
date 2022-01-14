library(tidyverse)
library(sf)
library(osmextract)
library(mapview)
library(tmap)
library(ggplot)

# GETTING DATA  =================
piggyback::pb_download("wy.Rds")
piggyback::pb_download("gm.Rds")
piggyback::pb_download("mers.Rds")
wy = readRDS("wy.Rds")
gm = readRDS("gm.Rds")
mers = readRDS("mers.Rds")



# EDA =====================
## WY

wy_df = wy %>% 
  sf::st_drop_geometry() %>% 
  filter(!is.na(highway))


wy_tagged = rbind(
  wy_df %>% filter(!is.na(wheelchair)) %>% mutate(key = "wheelchair", value = wheelchair, name = "wy"),
  wy_df %>% filter(!is.na(foot)) %>% mutate(key = "foot", value = foot, name = "wy"),
  wy_df %>% filter(!is.na(footway)) %>% mutate(key = "footway", value = footway, name = "wy"),
  wy_df %>% filter(!is.na(lit)) %>% mutate(key = "lit", value = lit, name = "wy"),
  wy_df %>% filter(str_detect(highway, "footway|living_street|path|pedestrian|steps|cycle")) %>%
    mutate(key = "highway", value = highway, name = "wy"),
  wy_df %>% filter(!is.na(maxspeed)) %>% mutate(key = "maxspeed", value = maxspeed, name = "wy"),
  wy_df %>% filter(!is.na(bicycle)) %>% mutate(key = "bicycle", value = bicycle, name = "wy"),
  wy_df %>% filter(!is.na(cycleway)) %>% mutate(key = "cycleway", value = cycleway, name = "wy")
) 

wy_tagged_grouped = wy_tagged %>% 
  group_by(key, value) %>% 
  mutate(n = n()) %>% 
  filter(n > 10) %>% 
  ungroup()


## plot 1 
tags_needed = c("bicycle", "foot", "cycleway","footway", "value")
wy_plot1 = wy_tagged_grouped %>% filter(key %in% tags_needed) %>% select(key, tags_needed) %>% 
ggplot( aes(x = value)) +
  geom_bar(position=position_dodge()) +
  facet_wrap(~key, scales = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1)) 

## plot 2

wy_tagged_grouped_prop = wy_tagged_grouped %>% 
  group_by(highway) %>% 
  summarize(n = n(),
            Proportion = n / nrow(wy_df) * 100)

wy_plot2 = wy_tagged_grouped_prop %>%  filter(highway == "footway" | highway == "cycleway")  %>% 
  ggplot( aes(x = highway,
              y = Proportion)) +
  geom_bar(stat = "identity") 

## plot 3

wy_tagged_grouped_prop2 = wy_tagged_grouped %>% 
  group_by(value, key) %>% 
  summarize(n = n(),
            Proportion = n / nrow(wy_df) * 100)
wy_plot3 = wy_tagged_grouped_prop2 %>% filter(key %in% tags_needed) %>% select(key, Proportion) %>% 
  ggplot( aes(x = value,
              y = Proportion)) +
  geom_bar(stat = "identity") +
  facet_wrap(~key, scales = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1)) 

# ============

## GM

gm_df = gm %>% 
  sf::st_drop_geometry() %>% 
  filter(!is.na(highway))

gm_tagged = rbind(
  gm_df %>% filter(!is.na(wheelchair)) %>% mutate(key = "wheelchair", value = wheelchair, name = "gm"),
  gm_df %>% filter(!is.na(foot)) %>% mutate(key = "foot", value = foot, name = "gm"),
  gm_df %>% filter(!is.na(footway)) %>% mutate(key = "footway", value = footway, name = "gm"),
  gm_df %>% filter(!is.na(lit)) %>% mutate(key = "lit", value = lit, name = "gm"),
  gm_df %>% filter(str_detect(highway, "footway|living_street|path|pedestrian|steps|cycle")) %>%
    mutate(key = "highway", value = highway, name = "gm"),
  gm_df %>% filter(!is.na(maxspeed)) %>% mutate(key = "maxspeed", value = maxspeed, name = "gm"),
  gm_df %>% filter(!is.na(bicycle)) %>% mutate(key = "bicycle", value = bicycle, name = "gm"),
  gm_df %>% filter(!is.na(cycleway)) %>% mutate(key = "cycleway", value = cycleway, name = "gm")
) 

gm_tagged_grouped = gm_tagged %>% 
  group_by(key, value) %>% 
  mutate(n = n()) %>% 
  filter(n > 10) %>% 
  ungroup() 
  
## plot 1 
tags_needed = c("bicycle", "foot", "cycleway","footway", "value")
gm_plot1 = gm_tagged_grouped %>% filter(key %in% tags_needed) %>% select(key, tags_needed) %>% 
  ggplot( aes(x = value)) +
  geom_bar(position=position_dodge()) +
  facet_wrap(~key, scales = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1)) 

## plot 2

gm_tagged_grouped %>% filter(highway == "footway") %>% nrow()

gm_tagged_grouped_prop = gm_tagged_grouped %>% 
  group_by(highway) %>% 
  summarize(n = n(),
            Proportion = n / nrow(gm_df) * 100)

gm_plot2 = gm_tagged_grouped_prop %>%  filter(highway == "footway" | highway == "cycleway")  %>% 
  ggplot( aes(x = highway,
              y = Proportion)) +
  geom_bar(stat = "identity") 

## plot 3

gm_tagged_grouped_prop2 = gm_tagged_grouped %>% 
  group_by(value, key) %>% 
  summarize(n = n(),
            Proportion = n / nrow(gm_df) * 100)
gm_plot3 = gm_tagged_grouped_prop2 %>% filter(key %in% tags_needed) %>% select(key, Proportion) %>% 
  ggplot( aes(x = value,
              y = Proportion)) +
  geom_bar(stat = "identity") +
  facet_wrap(~key, scales = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1)) 

gm_plot3

# MERSEYSIDE ===========

mers_df = mers %>% 
  sf::st_drop_geometry() %>% 
  filter(!is.na(highway))

mers_tagged = rbind(
  mers_df %>% filter(!is.na(wheelchair)) %>% mutate(key = "wheelchair", value = wheelchair, name = "mers"),
  mers_df %>% filter(!is.na(foot)) %>% mutate(key = "foot", value = foot, name = "mers"),
  mers_df %>% filter(!is.na(footway)) %>% mutate(key = "footway", value = footway, name = "mers"),
  mers_df %>% filter(!is.na(lit)) %>% mutate(key = "lit", value = lit, name = "mers"),
  mers_df %>% filter(str_detect(highway, "footway|living_street|path|pedestrian|steps|cycle")) %>%
    mutate(key = "highway", value = highway, name = "mers"),
  mers_df %>% filter(!is.na(maxspeed)) %>% mutate(key = "maxspeed", value = maxspeed, name = "mers"),
  mers_df %>% filter(!is.na(bicycle)) %>% mutate(key = "bicycle", value = bicycle, name = "mers"),
  mers_df %>% filter(!is.na(cycleway)) %>% mutate(key = "cycleway", value = cycleway, name = "mers")
) 

mers_tagged_grouped = mers_tagged %>% 
  group_by(key, value) %>% 
  mutate(n = n()) %>% 
  filter(n > 10) %>% 
  ungroup() 

## plot 1 
tags_needed = c("bicycle", "foot", "cycleway","footway", "value")
mers_plot1 = mers_tagged_grouped %>% filter(key %in% tags_needed) %>% select(key, tags_needed) %>% 
  ggplot( aes(x = value)) +
  geom_bar(position=position_dodge()) +
  facet_wrap(~key, scales = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1)) 

## plot 2

mers_tagged_grouped_prop = mers_tagged_grouped %>% 
  group_by(highway) %>% 
  summarize(n = n(),
            Proportion = n / nrow(mers_df) * 100)

mers_plot2 = mers_tagged_grouped_prop %>%  filter(highway == "footway" | highway == "cycleway")  %>% 
  ggplot( aes(x = highway,
              y = Proportion)) +
  geom_bar(stat = "identity") 

## plot 3

mers_tagged_grouped_prop2 = mers_tagged_grouped %>% 
  group_by(value, key) %>% 
  summarize(n = n(),
            Proportion = n / nrow(mers_df) * 100)
mers_plot3 = mers_tagged_grouped_prop2 %>% filter(key %in% tags_needed) %>% select(key, Proportion) %>% 
  ggplot( aes(x = value,
              y = Proportion)) +
  geom_bar(stat = "identity") +
  facet_wrap(~key, scales = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1)) 

mers_plot3



## WY + GM + mers joined =============

all_tagged = rbind(wy_tagged,
                     gm_tagged,
                   mers_tagged)

all_tagged_grouped = all_tagged %>% 
  group_by(key, value, name) %>% 
  mutate(n = n()) %>% 
  filter(n > 10) %>% 
  ungroup() 


## plot 1 
all_plot1 = all_tagged_grouped %>% filter(key %in% tags_needed) %>% select(key, tags_needed, name) %>% group_by(value, name, key) %>% summarise(N = n()) %>% 
  ggplot( aes(x = value,
           y = N,
              fill= name)) +
  geom_bar(stat = "identity", 
           position=position_dodge() ) +
  facet_wrap(~key, scales = "free_x") +
  # scale_y_continuous(labels=scales::percent) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1)) +
  geom_text(aes(label=N), 
            color="black", size=3.5,
            position = position_dodge(1))


## plot 2

gm_tagged_grouped_prop_name = gm_tagged_grouped_prop %>% mutate(name = "gm")
wy_tagged_grouped_prop_name = wy_tagged_grouped_prop %>% mutate(name = "wy")
mers_tagged_grouped_prop_name = mers_tagged_grouped_prop %>% mutate(name = "mers")

joined = rbind(gm_tagged_grouped_prop_name,
               wy_tagged_grouped_prop_name,
               mers_tagged_grouped_prop_name)

all_plot2 = joined %>%  filter(highway == "footway" | highway == "cycleway")  %>% 
  ggplot(aes(x = highway,
              y = Proportion,
              fill = name)) +
  geom_bar(stat = "identity",
           position=position_dodge() ) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, vjust = .5, hjust=1)) +
  geom_text(aes(label=round(Proportion,2)), 
            color="black", size=3.5,
            position = position_dodge(1))

## plot 3

gm_tagged_grouped_prop_name2 = gm_tagged_grouped_prop2 %>% mutate(name = "gm")
wy_tagged_grouped_prop_name2 = wy_tagged_grouped_prop2 %>% mutate(name = "wy")
mers_tagged_grouped_prop_name2 = mers_tagged_grouped_prop2 %>% mutate(name = "mers")

joined2 = rbind(gm_tagged_grouped_prop_name2,
               wy_tagged_grouped_prop_name2,
               mers_tagged_grouped_prop_name2)

joined_plot3 = joined2  %>% filter (key %in% tags_needed & value %in% c("designated", "no", "yes", "segregated", "permissive", "sidewalk", "crossing")) %>% 
  ggplot(aes(x = value,
             y = Proportion,
             fill = name)) +
  geom_bar(stat = "identity",
           position=position_dodge() ) + 
  theme_bw() +
  facet_wrap(~key, scales = "free_x") +
  geom_text(aes(label=round(Proportion,2)),
           color="black", size=3.5,
           position = position_dodge(1)
            ) 


joined_plot3.1 = joined2  %>% filter (key %in% tags_needed & value %in% c("designated", "no", "yes", "segregated", "permissive", "sidewalk", "crossing")) %>% 
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
  ) 


