# Set-up ------------------------------------------------------------------
library(dplyr)

# Function to find best matched LAN --> RN --------------------------------
# for the pct tool. 
get_region_name = function(LAN, auto_match=FALSE){
  matches = pct::pct_regions_lookup %>% dplyr::filter(lad16nm == LAN)
  
  # No matches found
  if(nrow(matches) == 0){
    # No exact match found, try find best match. 
    message(paste0("No exact Local Authority match found for: ", LAN, 
                   " so searching for a match now..."))
    
    # Score matches against provided LAN
    matches = utils::adist(pct::pct_regions_lookup$lad16nm, LAN)
    # Identify most similar place ID against LAN provided
    best_match_id  = which(matches == min(matches, na.rm = TRUE))
    # Identify best matched place using place ID
    best_matched_place = pct::pct_regions_lookup[best_match_id, ]
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

# Library imports ---------------------------------------------------------
pkgs = c("od", "tmaptools", "stplanr", "tidyverse", "pct", "dplyr", "tmap", "sf")
# install.packages(pkgs)
lapply(pkgs, library, character.only = TRUE)[length(pkgs)]


# Code --------------------------------------------------------------------

# Get all Local Authority Names
places = pct::pct_regions_lookup %>% dplyr::filter(! is.na(region_name))
LANs = places$lad16nm
LANs = LANs[1:5] # Subset of LANs for testing purposes
RN = get_region_name("Leiester", auto_match = TRUE)

# Network directory to store geosjons or plots (static & htmls?)
GAIST_pct_network_dir = paste0("~/GAIST_PCT_Networks/")
dir.create(GAIST_pct_network_dir)


# Iterate over region names | Create & save interactive plots of PCT routes----

# Get unique, non-NA region names only
RNs = pct::pct_regions_lookup %>% dplyr::filter(! is.na(region_name))
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
  
  # Add fast & quiet routes too!
  
  
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