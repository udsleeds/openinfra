og_test_data = openinfra::sotm_data

test_data = og_test_data

test_data_output = openinfra::oi_inclusive_mobility(test_data)

setdiff(colnames(test_data_output), colnames(test_data))


#_________ TESTING IM FUNCTION______________________________________________
# Load test data
im_test_data = openinfra::sotm_data

# Apply respective fucntions
im_output = openinfra::oi_inclusive_mobility(im_test_data)
oi_is_lit_output = openinfra::oi_is_lit(im_test_data)

# Observe value counts of unique items. 
oi_lit_vc = as.data.frame(table(oi_is_lit_output$openinfa_is_lit))
lit_vc = as.data.frame(table(im_output$openinfra_im_light))

# IM function visualisation
tmap::tmap_mode("view")
tmap::tm_shape(im_output) + 
  tmap::tm_lines(col = "openinfra_im_light")

# openinfra function visualisation
tmap::tm_shape(oi_is_lit_output) + 
  tmap::tm_lines(col = "openinfra_is_lit")
