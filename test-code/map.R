# Aim: create maps of Manchester city centre for walking and cycling Innovations
library(stplanr)
library(osmextract)
library(tmap)
library(ceramic)

route_mcr = route_google(from = "Manchester Piccadily", to = "Manchester Conference Centre", mode = "walking")
m = mapview::mapview(route_mcr) # looks good!
m
# Create custom bounding box:
map_edited = mapedit::editMap(m)
bbox = sf::st_bbox(map_edited$drawn$geometry)
bbox
# xmin      ymin      xmax      ymax 
# -2.236786 53.474959 -2.229480 53.477265 
sf::write_sf(route_mcr, "data-small/route_mcr.geojson")









# Get satellite data ------------------------------------------------------

# Source: https://github.com/hypertidy/ceramic/issues/42
library(gdalio)
get_im_bing_from_sf = function(x, dimension = c(512, 512), resample = "bilinear") {
  bb = sf::st_bbox(x)
  ex = as.numeric(bb)[c(1, 3, 2, 4)]
  g = gdalio_set_default_grid(list(extent = ex, dimension = dimension, projection = sf::st_crs(x)$wkt))
  source(system.file("raster_format/raster_format.codeR", package = "gdalio", mustWork = TRUE))
  ve_src <- '<GDAL_WMS>
  <Service name="VirtualEarth">
    <ServerUrl>http://a${server_num}.ortho.tiles.virtualearth.net/tiles/a${quadkey}.jpeg?g=90</ServerUrl>
  </Service>
  <MaxConnections>4</MaxConnections>
  <Cache/>
</GDAL_WMS>'
  gdalio_terra(ve_src, bands = 1:3, band_output_type = "integer", resample = resample)
}
route_mcr_projected = sf::st_transform(route_mcr, 2264)
map_satellite_terra = get_im_bing_from_sf(route_mcr_projected, dimension = c(512, 1024))
terra::plotRGB(map_satellite_terra)

