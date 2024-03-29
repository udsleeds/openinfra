# Investigate bugs...
# Bug1: url miss-matching within oe_get wrapper.
# Bug2: Cannot read downloaded files with oe_read. 

# Installs & Imports ------------------------------------------------------
required_tags = all_extra_tags

remotes::install_github("ropensci/osmextract")
remotes::install_github("udsleeds/openinfra")

pkgs = c("sf",
         "dplyr",
         "tidyverse",
         "openinfra",
         "osmextract")

# Imports
lapply(pkgs, library, character.only = TRUE)[length(pkgs)]

# Get Data ----------------------------------------------------------------
download_dir = "/home/james/Desktop/r_dl_tests"

test_leeds_driving = oe_get(
  place = "Leeds",
  layer = "lines",
  force_download = TRUE,
  download_only = TRUE,
  skip_vectortranslate = TRUE,
  download_directory = download_dir,
  extra_tags = c("bicycle", "access", "service")
)

# Auto-match place query to available URL
test_url_match = oe_match("Leeds")
url = test_url$url
message(paste("URL:", url))

# Download file using matched URL
test_file_dl = oe_download(file_url = url,
                      download_directory = "/home/james/Desktop/r_dl_tests/",
                      force_download = TRUE)

# List downloaded files by test_leeds_driving & custom approach
filenames = list.files("/home/james/Desktop/r_dl_tests/")

message("downloaded files:    ", gsub(".osm.pbf", ".osm.pbf    ", filenames))


read_bbb = oe_read("/home/james/Desktop/r_dl_tests/bbbike_Leeds.osm.pbf") 
read_geo = oe_read("/home/james/Desktop/r_dl_tests/geofabrik_Leeds.osm.pbf")
read_man = oe_read("/home/james/Desktop/r_dl_tests/west-yorkshire-latest.osm.pbf")

read_bbb_clip = oe_read(
  file_path = "/home/james/Desktop/r_dl_tests/bbbike_Leeds.osm.pbf",
  boundary = leeds_buffer,
  boundary_type = "clipsrc"
)

read_bbb_full = oe_read(
  file_path = "/home/james/Desktop/r_dl_tests/bbbike_Leeds.osm.pbf",
  extra_tags = required_tags,
  boundary = leeds_buffer,
  boundary_type = "clipsrc"
)


default_test = oe_get(
  place = "Leeds",
  layer = "lines",
  boundary = leeds_buffer,
  boundary_type = "clipsrc"
)
