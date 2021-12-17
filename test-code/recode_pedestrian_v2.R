```{r}
# this function has been developed to recategorize OSM data into a pedestrian network. Network is composed of the following OSM keys and tags: tag:highway=footway, key:footway, key:foot, key:sidewalk, key:pedestrian, key:path.
# the function takes in the OSM dataframe and returns a new OSM dataframe with a new column called `pedestrian_friendly`.
# `pedestrian_friendly` has three values (NAs excluded): "yes" (the highway is pedestrian friendly), "maybe" (the highway is potentially pedestrian friendly), and "no" (highways are not pedestrian friendly).
# the OSM data requires the keys and tags of which the pedestrian network is composed, otherwise it will not be able to return a `pedestrian_friendly` column.
# It is possible to modify the values that are considered pedestrian friendly or not but not the keys and tags of which the network is made up.
# Finally, `filter_df` allows to filter the OSM dataframe based on the new `pedestrian_friendly` column. The argument can take three values: "yes", "maybe", and "no". If "yes" is given, then only the rows containing "yes" in `pedestrian_friendly` will be returned. To return *all* the values, keep filter_df=NULL
recode_pedestrian <- function(df = osm_dataframe,
                              encouraged = NULL) {
  # defining default values used in the function
  highway_footway_yes = c("footway",
                          "pedestrian",
                          "living_street",
                          "residential",
                          "path",
                          "steps",
                          "track",
                          "unclassified",
                          "bridleway",
                          "tertiary",
                          "primary",
                          "secondary",
                          "trunk"
  )
  highway_footway_maybe <- c("service",
                             "cycleway",
                             "corridor",
                             "construction",
                             "road"
  )  
  highway_footway_no <- c("motorway",
                          "motorway_link",
                          "trunk_link",
                          "primary_link",
                          "tertiary_link",
                          "secondary_link",
                          "proposed",
                          "raceway",
                          "bus_guideway",
                          "busway",
                          "no", 
                          "services"
  )
  footway_yes = c("sidewalk",
                  "crossing",
                  "access_aisle",
                  "left",
                  "yes"
  )
  footway_maybe <- c("traffic_island", 
                     "link",
                     "alley"
  )
  footway_no <- c("no",
                  "none",
                  "np") 
  foot_yes <-  c("yes",
                 "designated"
  )
  foot_maybe <- c("permissive",
                  "private",
                  "customers",
                  "destination", # or no?
                  "discouraged",
                  "delivery",
                  "permit"
  )
  foot_no <- c("no",
               "unknown",
               "emergency",
               "informal", 
               "limited" 
  )
  sidewalk_yes = c("both",
                   "left",
                   "right",
                   "separate",
                   "mapped", 
                   "yes",
                   "crossing", 
                   "left;right"
  ) 
  sidewalk_no <- c("no",
                   "none")
  pedestrian_yes = c("crossing ")
  path_yes = c("footpath")
  path_no <- c("no")
  
  encouraged_default = "no"
  
  if(encouraged == "no"){
    # recategorization
    new_osm_df =  df %>% 
      mutate(pedestrian_friendly = case_when(
        highway %in% highway_footway_yes ~ "yes",
        highway %in% highway_footway_no ~ "no",
        
        highway %in% highway_footway_maybe & 
          foot %in% foot_yes | 
          footway %in% footway_yes | 
          sidewalk %in% sidewalk_yes |
          pedestrian %in% pedestrian_yes |
          path %in% path_yes
        ~ "yes",
        
        highway %in% highway_footway_maybe |
          foot %in% foot_maybe |
          footway %in% footway_maybe 
        ~ "maybe",
        
        highway %in% highway_footway_maybe & 
          foot %in% foot_no  | 
          footway %in% footway_no | 
          sidewalk %in% sidewalk_no |
          path %in% path_no
        ~ "no"
      )
      )
    return(new_osm_df)
  } 
}
