# highway=cycleway with width & estimated width tags


# Get the data for England. -----------------------------------------------


tags_needed = c("surface", "width", "est_width")

osm_sf_test = osmextract::oe_get(
  place = "England",
  layer = "lines",
  extra_tags = tags_needed
)

# Remove NA highways
osm_sf_test = osm_sf_test %>% dplyr::filter(! is.na(highway))

# Filter highway=cycleways only.  -----------------------------------------

# Select relevant highways
osm_sf_test = osm_sf_test %>% dplyr::filter(highway == "cycleway")


# View distribution of width & est_width tag values.  ---------------------
width_vc = as.data.frame(table(osm_sf_test$width))
est_width_vc = as.data.frame(table(osm_sf_test$est_width))


# Capture non-NA values for width & est_width -----------------------------

no_NA_width = osm_sf_test %>% dplyr::filter(! is.na(osm_sf_test$width))
no_NA_est_width = osm_sf_test %>% dplyr::filter(! is.na(osm_sf_test$est_width))
both_no_NA = osm_sf_test %>% dplyr::filter( (! is.na(osm_sf_test$width)) & (! is.na(osm_sf_test$est_width)) )

# Ouputs ------------------------------------------------------------------
message(paste0("Current analysis has been performed using England-latest.osm",
               ".pbf from 12/09/2022 applying the parsing -> highway==cycleway"))
message("\nTotal ways returned from England-latest.osm.pbf: ", nrow(osm_sf_test))
message("\nTotal ways that contain non-NA width tag: ",nrow(no_NA_width))
message("\nTotal ways that contain non-NA est_width tag: ",nrow(no_NA_est_width)) 
message("\nTotal ways contatining non-NA width & est_width tag: ",nrow(both_no_NA))
message("\nTotal ways containing either non-NA width OR est_width tag: ", (nrow(no_NA_width) + nrow(no_NA_est_width) - nrow(both_no_NA)))
        
        

