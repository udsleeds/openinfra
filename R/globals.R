# Script containing varibales listed in devtools::check ) stage [❯ checking R code for possible problems ... NOTE]
# created as a suggested fix from (https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887)

# reocde_road_class function
utils::globalVariables(c("road_class"))
# oi_clean_maxspeed_uk.R
utils::globalVariables(c("maxspeed", "oi_maxspeed"))
# oi_inclusive_mobility
utils::globalVariables(c("kerb", "width", "est_width"))


