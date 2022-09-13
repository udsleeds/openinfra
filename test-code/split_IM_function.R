# This script is for splitting up the oi_inclusive_mobility function into 
# individual functions on their own.


# List outputs of im_inclusive_mobility -----------------------------------

# Tags used in each function are shown in [square brackets]
# 'outputs' from each function are shown in (curved brackets)

#0 # openinfra_im_kerb | ["kerb"] | ("no", "flush", "other")
#1 # openinfra_im_footway | ["footway", "sidewalk", "cyclewalk", "foot", "segregated"] | ("yes", "no") 
#2 # openinfra_im_footpath | ["highway", "openinfra_im_footway", "foot", "access", "segregated"] | ("yes", "no")
#3 # openinfra_im_crossing (pedestrian) | ["crossing", "highway", "footway"] | ("give-way", "signal-controlled", "yes" ,"no")
#4 # openinfra_im_imp_footway | ["openinfra_im_footway", "openinfra_im_footpath", "openinfra_im_crossing"] | ("yes", "no")
#5 # openinfra_im_tactile | ["tactile_paving"] | ("yes", "no")
#TODO: test the functionality of im_surface_paved, does this classify roads as paved? 
#6 # openinfra_im_surface_paved (detects if surface is paved or not) not sure of use - this will classify roads as being paved (asphalt)
#c # | [highway, surface, ] | ("paved", "unpaved", "other")
#7 # openinfra_im_surface (just use surface tag -values descriptive enough) (currently even/un-even) | ["smoothness", "surface", "openinfra_im_surface_paved"] | ("even", "uneven")
#8 # openinfra_im_width (groups pavement width data, where available, from width column) | ["width"] | ("< 1.5", "1.5 - 2", "> 2")
#9 # openinfra_im_width_est (groups pavement width data, where available, from est_width column) | ["est_width"] | ("< 1.5", "1.5 - 2", "> 2")

# Ideas from above --------------------------------------------------------

# oi_flush_kerbs - assesses #0
# oi_pedestrian_infra - assesses #1, #2, #3, #4 in one
# oi_tactile_paving - assesses #5