# Imports
devtools::load_all()
data = example_data

# New function ( using parse_number() )
oi_clean_maxspeed_update = function(osm_sf, no_NA = FALSE, del = FALSE){
  
  # Define NOT IN 
  `%!in%` = Negate(`%in%`)
  
  osm_clean = osm_sf %>%
    dplyr::mutate(maxspeed = dplyr::case_when(
      # maxspeed == national, when on motorway
      (maxspeed == "national" & highway %in% c("motorway", "motorway_link")) ~ "70 mph",
      
      # maxspeed == national, when NOT on motorway
      (maxspeed == "national" & highway %!in% c("motorway", "motorway_link")) ~ "60 mph",  
      
      # maxspeed == national, when on standard (i.e Non-Residential) dual carriageway
      (maxspeed == "national" & highway %in% c("trunk", "trunk_link")) ~ "70 mph",
    ))
    
  
    # Above case_when sets speeds "national" as a string now parseable to number
    osm_clean = osm_sf %>% dplyr::mutate(
      oi_maxspeed = maxspeed %>% 
        parse_number(),
      oi_maxspeed = dplyr::case_when(
        oi_maxspeed == 20 ~ "20 mph",
        oi_maxspeed == 30 ~ "30 mph",
        oi_maxspeed == 40 ~ "40 mph",
        oi_maxspeed == 50 ~ "50 mph",
        oi_maxspeed == 60 ~ "60 mph",
        oi_maxspeed == 70 ~ "70 mph",
        
      ))
    
    if (no_NA){
      # if TURE, will remove features if their oi_maxspeed == NA
      osm_clean = osm_clean %>% dplyr::filter(!is.na(oi_maxspeed))
    }
    
    if (del){
      # If TRUE, will delete original `maxspeed` column
      osm_clean = subset(osm_clean, select = -c(maxspeed))
    } 
    
    return(osm_clean)
}


# Compare
oi_current_1 = oi_clean_maxspeed_uk(data, no_NA = FALSE, del = FALSE)
oi_new_1 = oi_clean_maxspeed_update(data, no_NA = FALSE, del = FALSE)

oi_current_2 = oi_clean_maxspeed_uk(data, no_NA = TRUE, del = FALSE) 
oi_new_2 = oi_clean_maxspeed_update(data, no_NA = TRUE, del = FALSE)

oi_current_3 = oi_clean_maxspeed_uk(data, no_NA = FALSE, del = TRUE) 
oi_new_3 = oi_clean_maxspeed_update(data, no_NA = FALSE, del = TRUE)

oi_current_4 = oi_clean_maxspeed_uk(data, no_NA = TRUE, del = TRUE) 
oi_new_4 = oi_clean_maxspeed_update(data, no_NA = TRUE, del = TRUE)

# Comparing network dimensions
c(dim(oi_current_1), dim(oi_new_1))
dim(oi_current_1) == dim(oi_new_1)
c(dim(oi_current_2), dim(oi_new_2))
dim(oi_current_2) == dim(oi_new_2)
c(dim(oi_current_3), dim(oi_new_3))
dim(oi_current_3) == dim(oi_new_3)
c(dim(oi_current_4), dim(oi_new_4))
dim(oi_current_4) == dim(oi_new_4)

# Comparing maxspeed value counts
vc_current_1 = as.data.frame(table(oi_current_1$oi_maxspeed))
vc_new_1 = as.data.frame(table(oi_new_1$oi_maxspeed))
FALSE %in% (vc_current_1 == vc_new_1)

vc_current_2 = as.data.frame(table(oi_current_2$oi_maxspeed))
vc_new_2 = as.data.frame(table(oi_new_2$oi_maxspeed))
FALSE %in% (vc_current_2 == vc_new_2)

vc_current_3 = as.data.frame(table(oi_current_3$oi_maxspeed))
vc_new_3 = as.data.frame(table(oi_new_3$oi_maxspeed))
FALSE %in% (vc_current_3 == vc_new_3)

vc_current_4 = as.data.frame(table(oi_current_4$oi_maxspeed))
vc_new_4 = as.data.frame(table(oi_new_4$oi_maxspeed))
FALSE %in% (vc_current_4 == vc_new_4)

# Testing if a mismatch is detectable
vc_current_5 = vc_current_4
vc_current_5$Freq[vc_current_5$Var1 == "60 mph"] = 500 #Adding incorrect value
FALSE %in% (vc_current_5 == vc_new_4) # Returns True - thus the updated function works


# Comparing function runtimes.
library(microbenchmark)
print("Current:")
microbenchmark(oi_clean_maxspeed_uk(data, no_NA = FALSE, del = FALSE), times = 1000)
print("Update:")
microbenchmark(oi_clean_maxspeed_update(data, no_NA = FALSE, del = FALSE), times = 1000)

print("Current:")
microbenchmark(oi_clean_maxspeed_uk(data, no_NA = TRUE, del = FALSE), times = 1000)
print("Update:")
microbenchmark(oi_clean_maxspeed_update(data, no_NA = TRUE, del = FALSE), times = 1000)