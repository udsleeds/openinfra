
# Library imports ---------------------------------------------------------
library(tidyverse)
library(pct)
library(dplyr)
library(tmap)


# Function to find best matched LAN --> RN --------------------------------

get_region_name = function(LAN, auto_match=FALSE){
  matches = pct::pct_regions_lookup %>% dplyr::filter(lad16nm == LAN)
  
  # No matches found
  if(nrow(matches) == 0){
    # No exact match found, try find best match. 
    message(paste0("No exact Local Authority match found for: ", LAN, 
                   " so searching for a match now..."))
    
    # Score matches against provided LAN
    matches = utils::adist(pct_regions_lookup$lad16nm, LAN)
    # Identify most similar place ID against LAN provided
    best_match_id  = which(matches == min(matches, na.rm = TRUE))
    # Identify best matched place using place ID
    best_matched_place = pct_regions_lookup[best_match_id, ]
    # Extract region_name from best matched place
    message("Best matched place is: ", best_matched_place$lad16nm, ", ",
            best_matched_place$region_name)
    # Get region name field from lookup table
    RN = best_matched_place$region_name
    
    # Should we promt the user to accept the best match? 
    if(! auto_match){
      # Ask user if they want to use the best match
      continue = utils::menu(
        choices = c("Yes", "No"),
        title = "Would you like to obtain data for the best matched region?"
      )
      
      # Assess users response
      if(continue != 1){
        stop(paste0("User aborted match. To see supported authorities use ",
                    "`View(pct_regions_lookup)`"))
        return(NULL)
      }else{
        message("Using best matched local authority ", LAN, " with",
                " region_name: ", RN)
        return(RN)
      }
    }else{
      message("Using best matched local authority ", best_matched_place$lad16nm,
              " with region_name: ", RN)
      return(RN)
    }

  
  # A perfect match has been found, return matching region_name
  }else{
    RN = matches$region_name[[1]]
    message(paste0("Local authority ", LAN, "has been exactly matched to",
                   " region-name: ", RN))
    return(RN)
  }
}


# Testing  ----------------------------------------------------------------

# LAN = "Peterborough"
# matches = utils::adist(pct_regions_lookup$lad16nm, LAN)
# best_match_id  = which(matches == min(matches, na.rm = TRUE))
# 
# if (length(best_match_id) > 1L) {
#   best_match_id = best_match_id[1L]
# }
# best_match_id
# 
# best_matched_place = pct_regions_lookup[best_match_id, ]
# message("Best matched place is: ", best_matched_place$lad16nm, ", ",
#         best_matched_place$region_name)
# RN = best_matched_place$region_name
# Set-up ------------------------------------------------------------------


# Get all Local Authority Names
places = pct_regions_lookup %>% dplyr::filter(! is.na(region_name))
LANs = places$lad16nm
LANs = LANs[1:5] # Subset of LANs for testing purposes
RN = get_region_name("Leiester")

# Network directory to store geosjons or plots (static & htmls?)
GAIST_pct_network_dir = paste0("/home/james/Desktop/LIDA_OSM_Project/openin",
                               "fra/GAIST_PCT_Networks/")

# Get data & create plots.  -----------------------------------------------

# for(LAN in LANs){
#   message("Working on: ", LAN)
#   
#   # Get region_name to acquire data
#   RN = get_region_name(LAN, auto_match = TRUE)
#   
#   # Get pct route network
#   rnet_pct = pct::get_pct_rnet(RN)
#   
#   # Get top 10% of routes, scaled by the government uptake targets (SLC)
#   rnet_top_10 = rnet_pct |> 
#     top_frac(n = 0.1, wt = govtarget_slc)
#   
#   # Plot routes with highest potential, overlayed with top 10% of routes
#   tmap::tmap_mode("view")
#   
#   tmap::tm_shape(rnet_pct) + 
#     tmap::tm_lines(col = "black") + 
#     tmap::tm_shape(rnet_top_10) + 
#     tmap::tm_lines(col = "red") + 
#     tmap::tm_layout(title = paste0("Local Autority: ", LAN, 
#                                    ", Region Name: ", RN),
#                     legend.show = TRUE)
#   
#   
#   #plot(rnet_pct$geometry, col = "grey")
#   #plot(rnet_top_10$geometry, add = TRUE)
# }


# Iterate over region names -----------------------------------------------

# Get unique, non-NA region names only
RNs = pct_regions_lookup %>% dplyr::filter(! is.na(region_name))
RNs = unique(RNs$region_name)


for(RN in RNs){

  message("Loading region name: ", RN)

  # Get pct route network
  rnet_pct = pct::get_pct_rnet(RN)
  rnet_pct = rnet_pct %>% dplyr::mutate(desc = "Default routes")
  
  # Get top 10% of routes, scaled by the government uptake targets (SLC)
  rnet_top_10 = rnet_pct |> 
    top_frac(n = 0.1, wt = govtarget_slc)
  
  # Remove and replace desc column for legend
  rnet_top_10 = subset(rnet_top_10, select = -c(desc) )
  rnet_top_10 = rnet_top_10 %>% dplyr::mutate(desc ="90% percentile routes")
  
  
  # Plot routes with highest cycling potential, overlayed with top 10% of routes
  RegionMap = tmap::tm_shape(rnet_pct) + 
    tmap::tm_lines(col="desc", palette="black", title.col="Routes type") + 
  tmap::tm_shape(rnet_top_10) + 
    tmap::tm_lines(col="desc", palette="red", title.col="Routes type") + 
    tmap::tm_layout(main.title = paste0("Region Name: ", str_to_title(RN)),
                    #main.title.position = "centre",
                    title = paste0("Region Name: ", RN),
                    legend.bg.color = "grey",
                    legend.bg.alpha = .4
                    ) 

  # Save the map
  filepath = paste0(GAIST_pct_network_dir, RN, ".html")
  tmap::tmap_save(RegionMap, filepath)
} 

# Create a buffer with high cycling potential -----------------------------

#rnet_pct = pct::get_pct_rnet(RN)

# Get top 10% of routes, scaled by the government uptake targets (SLC)
#rnet_top_10 = rnet_pct |> 
#  top_frac(n = 0.1, wt = govtarget_slc)

# Plot routes with highest potential, overlayed with top 10% of routes
#plot(rnet_pct$geometry, col = "grey")
#plot(rnet_top_10$geometry, add = TRUE)
