# AIM: create an OSM dataset of central Leeds

tags = c("kerb", "width", "sidewalk", "cycleway", "footway", "lit", "wheelchair", "maxspeed")
leeds = osmextract::oe_get(place = "Leeds",
                   provider = "bbbike",
                   force_vectortranslate = TRUE,
                   force_download = TRUE,
                   extra_tags = tags)

map_leeds = mapview::mapview(leeds)
map_central_leeds = mapedit::editMap(map_leeds)
box = sf::st_bbox(map_central_leeds$drawn$geometry)
leeds_central = leeds[map_central_leeds$drawn$geometry, op=sf::st_within]

saveRDS(leeds_central,
        "leeds_central_14-02-2022.RDS")
