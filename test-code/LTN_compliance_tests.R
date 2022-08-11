# Script for test-code on LTN1/20 compliance - will investigate the physical 
# requirements (width, road separation etc.) of cycleways to see whether or not 
# they comply with LTN1/20 guide. Of particular use is the LTN1/20 guidance: 
# https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/951074/cycle-infrastructure-design-ltn-1-20.pdf
# specifically, chapters 4 & 5.


# Set-up ------------------------------------------------------------------

# Library Imports
pkgs = c("sf",
         "osmextract",
         "tidyverse",
         "tmap")
lapply(pkgs, library, character.only = TRUE)[length(pkgs)]

required_tags = c("foot", "bicycle", "access", "service", "maxspeed", "oneway",
                  "kerb", "footway", "sidewalk", "cycleway", "segregated", "highway",
                  "crossing", "lit", "tactile_paving", "surface", "smoothness", "width",
                  "est_width", "lit_by_led", "boundary", "admin_level", "name", "ref")

# Get data ----------------------------------------------------------------

LADs = sf::read_sf("https://github.com/udsleeds/openinfra/raw/main/data-small/lads_joined_2021.geojson")
leeds_lad_poly = LADs %>% 
  dplyr::filter(LADs$LAD21NM == "Leeds") %>% 
  dplyr::select("geometry")

# Request data specify leeds_lad_poly as palce
leeds_first_network = osmextract::oe_get(
  place = leeds_lad_poly,
  layer = "lines",
  boundary_type = "clipsrc",
  extra_tags = required_tags
)

### THIS MIGHT NO LONGER EB NEEDED THNKS TO "clipsrc" WITHIN OE_GET
# Re sample the network to contain ONLY the network covered by leeds_lad_poly
leeds_network = leeds_first_network[leeds_lad_poly, ]



