# Aim: get UK's national cycle network

library(osmextract)
library(tidyverse)

# Test on a small amount of data
query = "SELECT * FROM 'multilinestrings' WHERE ncn is not null OR network = 'ncn'"
et = c("oneway", "maxspeed", "ref", "ncn", "network")
cycleways_wyca_relations = oe_get(place = "West Yorkshire", query = query, extra_tags = et)
nrow(cycleways_wyca_relations) # 15 multilinestrings
mapview::mapview(cycleways_wyca_relations)
# See https://github.com/ropensci/osmextract/issues/265

# On national network:
ncn = oe_get(place = "England", query = query, extra_tags = et)
nrow(ncn) # 337 multilinestrings
mapview::mapview(ncn)

# Previous test:
query = "SELECT * FROM 'lines' WHERE ncn = 'yes'"
et = c("oneway", "maxspeed", "ref", "ncn")
cycleways_wyca = oe_get(place = "West Yorkshire", query = query, extra_tags = et)
nrow(cycleways_wyca) # 24 rows
mapview::mapview(cycleways_wyca)


# How to get the results above as individual lines?
# wyca_relations = oe_get(place = "West Yorkshire", ...)

