# Script containing varibales listed in devtools::check ) stage [❯ checking R code for possible problems ... NOTE]
# created as a suggested fix from (https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887)

# reocde_road_class 
utils::globalVariables(c("openinfra_road_class"))
# oi_clean_maxspeed_uk.R
utils::globalVariables(c("maxspeed", "openinfra_maxspeed"))
# oi_inclusive_mobility
utils::globalVariables(c("kerb", "width", "est_width"))
# oi_bicycle_parking
utils::globalVariables(c("openinfra_cycle_parking"))
# oi_cycle_separation
utils::globalVariables(c("openinfra_cycle_infra"))
# oi_cycle_crossings
utils::globalVariables(c("openinfra_cycle_crossings"))
# oi_road_names
utils::globalVariables(c("openinfra_road_name"))