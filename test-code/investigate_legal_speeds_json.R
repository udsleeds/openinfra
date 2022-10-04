# Read and analyse legal maxspeed data from https://github.com/westnordost/osm-legal-default-speeds/blob/master/demo/distribution/legal_default_speeds.json

library(rjson)
library(jsonlite)

speeds_data = fromJSON("legal_default_speeds.json")
# GB Road type structure

# [[1]] - restricted road
# [[2]] - unknown
# [[3]] - rural road
# [[4]] - dual carriadgeway
# [[5]] - motorway

# Get tags for each road category
GB_speeds = speeds_data$speedLimitsByCountryCode$GB

GB_restricted_tags = GB_speeds[[1]][["tags"]]
GB_unknown_tags = GB_speeds[[2]][["tags"]]
GB_rural_tags = GB_speeds[[3]][["tags"]]
GB_dual_tags = GB_speeds[[4]][["tags"]]
GB_motorway_tags = GB_speeds[[5]][["tags"]]

# Example of getting default maxspeed
motorway_max = motorway_tags[["maxspeed"]]
message(paste("Motorway maxspeed is:", motorway_max))
hgv_max = motorway_tags[["maxspeed:hgv:conditional"]]
message(paste("Conditional motorway maxspeeds for GHVs:", hgv_max))


rr_max = GB_speeds[[1]][["tags"]][["maxspeed"]]

b_test = speeds_data$speedLimitsByCountryCode$GB

a_1 = b_test[[1]]


# get maxspeeds by category from json file


