
leeds = openinfra::example_data

devtools::load_all()

leeds_small = leeds[leeds_buffer, op = st_within]
leeds_small = leeds_small %>% dplyr::mutate(maxspeed = NA)

func_out = openinfra::oi_cycle_separation(leeds_small, remove = TRUE)
#func_out = func_out %>% dplyr::mutate(openinfra_maxspeed = NA)

tmap::tmap_mode("view")
func_out = func_out %>% dplyr::filter(! is.na(openinfra_cycle_infra)) 

#tmap::qtm(func_out["openinfra_cycle_infra"])

tmap::tm_shape(func_out) + 
  tmap::tm_lines(col = "openinfra_cycle_infra", palette = c("red", "blue", "orange", "green", "pink"))
