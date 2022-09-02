# Test script to investigating cyclway="crossing" tag distributins. 



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
needed_tags = c("amenity", "cycleway", "bicycle")

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

leeds_analyse = leeds %>% dplyr::filter(cycleway == "crossing")

more_tags = leeds_analyse %>% dplyr::select(other_tags)

# Looking at the other tags (more_tags) column we can see a whole host of 
# additional tags, to view these in more detail, consider using the taginfo 
# site - see here: https://taginfo.openstreetmap.org/keys/crossing#values

# Most notably are the additional values assigned to the "crossing" key which
# can be used to classify the type of corssing present.  