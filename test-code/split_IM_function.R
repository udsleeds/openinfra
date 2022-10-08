# This script is for splitting up the oi_inclusive_mobility function into 
# individual functions on their own.


# List outputs of im_inclusive_mobility -----------------------------------

# Tags used in each function are shown in [square brackets]
# 'outputs' from each function are shown in (curved brackets)

#0 # openinfra_im_kerb | ["kerb"] | ("no", "flush", "other") XXXXX
#1 # openinfra_im_footway | ["footway", "sidewalk", "cyclewalk", "foot", "segregated"] | ("yes", "no") 
#2 # openinfra_im_footpath | ["highway", "openinfra_im_footway", "foot", "access", "segregated"] | ("yes", "no")
#3 # openinfra_im_crossing (pedestrian) | ["crossing", "highway", "footway"] | ("give-way", "signal-controlled", "yes" ,"no")
#4 # openinfra_im_imp_footway | ["openinfra_im_footway", "openinfra_im_footpath", "openinfra_im_crossing"] | ("yes", "no")
#5 # openinfra_im_tactile | ["tactile_paving"] | ("yes", "no")
#6 # openinfra_im_surface_paved (detects if surface is paved or not) not sure of use - this will classify roads as being paved (asphalt)
#c # | [highway, surface, ] | ("paved", "unpaved", "other")
#7 # openinfra_im_surface (just use surface tag -values descriptive enough) (currently even/un-even) | ["smoothness", "surface", "openinfra_im_surface_paved"] | ("even", "uneven")
#8 # openinfra_im_width (groups pavement width data, where available, from width column) | ["width"] | ("< 1.5", "1.5 - 2", "> 2")
#9 # openinfra_im_width_est (groups pavement width data, where available, from est_width column) | ["est_width"] | ("< 1.5", "1.5 - 2", "> 2")

# Ideas from above --------------------------------------------------------

# oi_im_flush_kerbs - assesses #0                         [X]
# oi_im_pedestrian_infra - assesses #1, #2, #3, #4 in one [X]
# oi_im_tactile_paving - assesses #5                      [X]
# oi_im_surface - assesses #6 #7                          [X]
# oi_im_pavement_width - assesses #8 #9                   [X]

# Testing the new functions -----------------------------------------------

# old IM function
old_oi_inclusive_mobility = function(osm_sf) {
  
  #browser() # <-- Uncomment for debugging.
  
  osm_sf_im = osm_sf %>% 
    # Assesses whether a kerb is flush or not
    dplyr::mutate(openinfra_im_kerb = dplyr::if_else(kerb == "flush" | kerb == "no", "flush", "other")) %>% 
    
    # Assesses footway - a ‘pavement’ adjacent to a road
    dplyr::mutate(openinfra_im_footway = dplyr::case_when(
      footway %in% c("left", "right", "both", "sidewalk") |
        sidewalk %in% c("left", "right", "both", "yes", "separate") |
        # trying to capture footways shared with cyclists
        !is.na(cycleway) & # map cycling infrastructure that is an inherent part of the road
        foot %in% c("yes", "designated") |
        segregated %in% "yes"
      ~ "footway",
      TRUE ~ "no" 
    ) 
    ) %>% 
    # Assesses footpath - any other right of way for pedestrians, that does not run adjacent to a road.
    dplyr::mutate(openinfra_im_footpath = dplyr::case_when(
      highway %in% "footway" & 
        openinfra_im_footway %in% "no" | 
        # not (always) an inherent part of the road
        highway %in% c("cycleway", "bridleway", "path") & # foot = "designated" is implied
        openinfra_im_footway %in% "no" &
        ! foot %in% c("no", "private") | 
        ! access %in% c("no", "private") &
        segregated %in% "no" # shared space
      ~ "footpath",
      TRUE ~ "no"
    )
    ) %>%
    
    # Assesses presence of a crossing and what type: give-way, signal controlled, none, or yes (but the type is unknown)
    dplyr::mutate(openinfra_im_crossing = dplyr::case_when(
      stringr::str_detect(crossing, "zebra|uncontr|marked")~ "give-way crossing",
      stringr::str_detect(crossing, "toucan|pedex|puffin|equestrian|light|signal")~ "signal-controlled crossing",
      highway %in% "crossing" | footway  %in% "crossing" | !is.na(crossing) ~ "unknown crossing type",
      TRUE ~ "no"
    )) %>% 
    
    # implied footways but there's a lack of data to verify
    dplyr::mutate(openinfra_im_footway_imp = dplyr::case_when(
      openinfra_im_footway %in% "no" &
        openinfra_im_footpath %in% "no" &
        openinfra_im_crossing %in% "no"
      ~ "implied footway",
      TRUE ~ "no"
    )
    ) %>% 
    # Assesses whether the way is lit or not
    dplyr::mutate(openinfra_im_light = dplyr::case_when( 
      # highway %in% "street_lamp" |
      ! lit %in% c("no", "disused") & ! is.na(lit)
      ~ "yes",
      TRUE ~ "no"
    )
    ) %>% 
    
    # Assesses the presence of tactile paving - either yes, no.
    dplyr::mutate(openinfra_im_tactile = dplyr::case_when(
      ! tactile_paving %in% c("no", "incorrect", "bad") & ! is.na(tactile_paving) 
      ~ "yes",
      ! is.na(tactile_paving)
      ~ "no"
    )
    ) %>%
    
    # Assesses whether surface is paved, unpaved, or other
    dplyr::mutate(
      openinfra_im_surface_paved = dplyr::case_when(
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
        (TRUE & !is.na(surface)) ~ "other"
      )
    ) %>% 
    # Assesses whether surface is even or uneven
    dplyr::mutate(openinfra_im_surface = dplyr::case_when(
      stringr::str_detect(surface, "asph|concrete")
      ~ "even",
      
      openinfra_im_surface_paved %in% "paved" &
        smoothness %in% c("excellent", "good")
      ~ "even",
      ! is.na(openinfra_im_surface_paved) 
      ~ "uneven"
    )
    ) %>% 
    # Assesses way width - either under 1.5 meters, 1.5-2 meters, or over 2 meters
    dplyr::mutate(
      openinfra_im_width =  width %>% 
        readr::parse_number(),
      openinfra_im_width = dplyr::case_when(
        openinfra_im_width > 0 & openinfra_im_width < 1.5 ~ " < 1.5",
        openinfra_im_width <= 1.5 & openinfra_im_width <= 2 ~ "1.5 - 2",
        openinfra_im_width > 2 ~ "> 2"
      )
    ) %>% 
    # Assesses estimated way width - either under 1.5 meters, 1.5-2 meters, or over 2 meters
    dplyr::mutate(
      openinfra_im_width_est = est_width %>% 
        readr::parse_number(),
      openinfra_im_width_est = dplyr::case_when(
        openinfra_im_width_est > 0 & openinfra_im_width_est < 1.5 ~ "< 1.5",
        openinfra_im_width_est <= 1.5 & openinfra_im_width_est <= 2 ~ "1.5 - 2",
        openinfra_im_width_est > 2 ~ "> 2"
      )
    )
  return(osm_sf_im)
}

devtools::load_all()
data = example_data

# Obtain old IM function outputs
output_old = old_oi_inclusive_mobility(data)

# Obtain new IM functions outputs
flush_kerb_output = oi_im_flush_kerb(data)
pedestrian_infra_output = oi_im_pedestrian_infra(data)
tactile_paving_output = oi_im_tactile_paving(data)
infra_surfaces_output = oi_im_surfaces(data)
pavement_width_output = oi_im_pavement_width(data)
  
###############################################################################
# Check column by column value counts to ensure outputs are consistent. 
im_cols = names(output_old)
im_cols = im_cols[grep(pattern = "openinfra", im_cols)]
message("List of IM columns:\n", paste0(im_cols, sep="\n"))

message(paste0("\n If any FALSE show up in the following checks then ",
               "there is a missmatch between old and new (split) IM",
               " functions\n"))

# IM Flush Kerb
og_im_kerb = as.data.frame(table(output_old$openinfra_im_kerb))
new_im_kerb = as.data.frame(table(flush_kerb_output$openinfra_im_flush_kerb))

# IM Footway
og_footway = as.data.frame(table(output_old$openinfra_im_footway))
new_footway = as.data.frame(table(pedestrian_infra_output$openinfra_im_footway))

# IM Footpath
og_footpath = as.data.frame(table(output_old$openinfra_im_footpath))
new_footpath = as.data.frame(table(pedestrian_infra_output$openinfra_im_footpath))

# IM Crossing
og_crossing = as.data.frame(table(output_old$openinfra_im_crossing))
new_crossing = as.data.frame(table(pedestrian_infra_output$openinfra_im_crossing))

# IM Footway implied
og_footway_imp = as.data.frame(table(output_old$openinfra_im_footway_imp))
new_footway_imp = as.data.frame(table(pedestrian_infra_output$openinfra_im_footway_imp))

# IM Light (REMOVED DUE TO OPENINFRA IS LIT FUNCTION)
#og_light = as.data.frame(table(output_old$openinfra_im_light))
#new_light = as.data.frame(table(flush_kerb_output$openinfra_im_light))
#message("IM Light\n", og_light == new_light)

# IM tactile
og_tactile = as.data.frame(table(output_old$openinfra_im_tactile))
new_tactile = as.data.frame(table(tactile_paving_output$openinfra_im_tactile))

# IM Surface Paved
og_surface_paved = as.data.frame(table(output_old$openinfra_im_surface_paved))
new_surface_paved = as.data.frame(table(infra_surfaces_output$openinfra_im_paved_surface))

# IM Surface
og_surface = as.data.frame(table(output_old$openinfra_im_surface))
new_surface = as.data.frame(table(infra_surfaces_output$openinfra_im_surface))

# IM Width 
og_width = as.data.frame(table(output_old$openinfra_im_width))
new_width = as.data.frame(table(pavement_width_output$openinfra_im_width))

# IM Width estimate
og_width_est = as.data.frame(table(output_old$openinfra_im_width_est))
new_width_est = as.data.frame(table(pavement_width_output$openinfra_im_width_est))

# Messages
message("IM Flush Kerb\n", og_im_kerb[2] == new_im_kerb[2])
message("IM Footway\n", og_footway[2] == new_footway[2])
message("IM Footpath\n", og_footpath[2] == new_footpath[2])
message("IM Crossing\n", og_crossing[2] == new_crossing[2])
message("IM Footway Implied\n", og_footway_imp[2] == new_footway_imp[2])
message("IM Tactile Paving\n", og_tactile[2] == new_tactile[2])
message("IM Surface Paved\n", og_surface_paved[2] == new_surface_paved[2])
message("IM Even Surface\n", og_surface[2] == new_surface[2])
message("IM Pavement Width\n", og_width[2] == new_width[2])
message("IM Pavement Width Estimaates\n", og_width_est[2] == new_width_est[2])



#tmap::tm_shape(output_old) + 
#  tmap::tm_lines(col = "openinfra_im_footpath") + 
#tmap::tm_shape(pedestrian_infra_output) + 
#  tmap::tm_lines(col = "openinfra_im_footpath")
  
