# Test script to investigating cyclway="crossing" tag distributins. 

# LTN Guide Context -------------------------------------------------------


# [10.4.1 LTN1/20 Guide Definition]
# Cycle crossings are mid-link stand-alone
# facilities to enable cyclists to cross a carriageway that
# would otherwise form a hazardous or impenetrable
# barrier on the cycle route network. Crossings may also
# form part of junction treatments where cyclists are taken
# off the carriageway. They may be used to connect
# off-highway cycle routes across a major road and enable
# connections with quieter street networks via cycle-only
# access points.

#  Uncontrolled Crossings:
#     - With or without crossings
#      
#  Controlled crossings
#     - Cycle priority crossing using give-way markings
#     - Parallel crossing
#     - Signal controlled - Toucan and Cycle Signal Crossings.

# Imports -----------------------------------------------------------------

pkgs = c("osmextract",
         "openinfra",
         "dplyr",
         "tidyverse",
         "sf")

lapply(pkgs, library, character.only = TRUE)[length(pkgs)]


# Set up buffer & get data ------------------------------------------------

# Set up buffer
leeds_centre_point = sf::st_sfc(sf::st_point(c(-1.549, 53.801)), 
                                crs = "EPSG:4326")
leeds_buffer = sf::st_buffer(leeds_centre_point, dist = 5000)

# Required tags
needed_tags = c("amenity", "cycleway", "bicycle", "crossing", "crossing:island",
                "crossing:ref", "crossing_ref")

# Get data
leeds = oe_get(
  place = "Leeds",
  layer = "lines",
  extra_tags = needed_tags, 
  never_skip_vectortranslate = TRUE,
  boundary = leeds_buffer,
  boundary_type = "clipsrc",
  quiet = FALSE
)

# Remove NA highways (waterways, aerialways, etc.)
leeds = leeds %>% dplyr::filter(! is.na(highway))


# Investigate additional tags ---------------------------------------------

leeds_crossings = leeds %>% dplyr::filter(cycleway == "crossing")

leeds_analyse = leeds %>% dplyr::filter(cycleway == "crossing")

more_tags = leeds_analyse %>% dplyr::select(other_tags)

# Looking at the other tags (more_tags) column we can see a whole host of 
# additional tags, to view these in more detail, consider using the taginfo 
# site - see here: https://taginfo.openstreetmap.org/keys/crossing#values

# Most notably are the additional values assigned to the "crossing" key which
# can be used to classify the type of crossing present.

# Most important seem to be crossing = c("marked", "unmarked", "uncontrolled",
# "traffic_signals", "zebra").

# Additionally we should be excluding, or at least making note of crossings that
# require cyclists to dismount (bicycle="dismount") as LTN1/20 implies that 
# there should be no need to cyclists to dismount, and if this is a requirement
# due to a busy junction (shared with pedestrians) then a separate piece of 
# infrastructure should be created for cyclists.


# Testing oi_cycle_crossings function -------------------------------------
devtools::load_all()

func_output = oi_cycle_crossings(leeds)
vc_output = as.data.frame(table(func_output$openinfra_cycle_crossings))

func_output = func_output %>% dplyr::filter(openinfra_cycle_crossings == "yes")

tmap::tmap_mode("view")
tmap::tm_shape(func_output) + 
  tmap::tm_lines(col = "openinfra_cycle_crossings")

