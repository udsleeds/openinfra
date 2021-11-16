This notebook contains the initial exploration of OSM data on walking
and cycling. Particular focus is paid to the information (keys + tags)
on accessibility.

# libraries and data

## libraries

    # load up the libraries
    library(osmextract) 

    ## Data (c) OpenStreetMap contributors, ODbL 1.0. https://www.openstreetmap.org/copyright.
    ## Check the package website, https://docs.ropensci.org/osmextract/, for more details.

    library(tidyverse)

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.5     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.4     ✓ stringr 1.4.0
    ## ✓ readr   2.0.2     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

    library(sf)

    ## Linking to GEOS 3.8.1, GDAL 3.2.1, PROJ 7.2.1

## datasets

Data is limited to West Yorkshire. `osmextract` was used to query the
OSM database. For a query example see the code below, otherwise proceed
to uploading the provided .Rds files.

    # QUERY EXAMPLE
    # oe_match_pattern("Yorkshire")
    # region_name <- "West Yorkshire"
    # wy <- osmextract::oe_get(place = region_name,
    #                          layer = "lines",
    #                          force_download = TRUE,
    #                          force_vectortranslate = TRUE) # importing West Yorkshire data

    # note: you might have to indicate the path to the directory
    wy <- readRDS("wy.Rds") 
    # wy_short <- readRDS("wy_short.Rds")
    # wy_cycling_net <- readRDS("wy_cycling_net.Rds")
    # wy_cn_short <- readRDS("wy_cn_short.Rds")
    wy_pct <- readRDS("wy_pct.Rds")
    leeds <- wy_pct %>% filter(lad_name == "Leeds")
    wy_walking <- readRDS("wy_walking.Rds")
    # wy_walk_short <- readRDS("wy_walking.Rds")

## additional functions

    # My very first function of which I am very proud :)
    # what it does:
    # 1. calculates the ratio of the (sf) column's non-NA cases (rows) to the total number of cases (rows)
    # 2. converts ratio to percentages
    # 3. rounds to 2 significant numbers
    perc_ratio <- function(df = dataframe, x = character()){
    ((df %>% pull(x) %>% table() %>% sum()) / (df %>% nrow()) * 100) %>% 
        signif(digits = 2)
    }

    # a function that returns a table of sf object's column's values sorted decreasingly
    sf_col_table <- function(df = dataframe,
                             x = character()){
      df %>% pull(x) %>% table %>% sort(decreasing = TRUE)
                             }

# cycling

Tags of interest for cycling:

surface maxspeed:type width maxwidth width:lanes

bicycle cycleway cycleway:both cycleway:right cycleway:left
cycleway:both:lane cycleway:left:lane cycleway:right:lane
cycleway:left:width cycleway:right:width oneway:bicycle
cycleway:otherside  
cycleway:otherside:width cycleway:both:width cycleway:oneside
cycleway:oneside:width cycleway:right:oneway cycleway:width
cycleway:surface bicycle\_road cyclestreet cycleway:buffer
cycleway:left:separation:right bicycle:lanes:conditional
bicycle:lanes:forward:conditional cycleway:lane cycleway:left:foot
cycleway:left:oneway  
cycleway:left:segregated  
cycleway:left:seperation:right  
cycleway:proposed  
cycleway:right:separation:left “cycle” “cycleway:est\_width”

# walking + accessibility

“wheelchair”, “kerb”, “disabled”, “mobility\_scooter”, “handicap”,
“foot”, “lit”, “access”, “sidewalk”, “footway”, “incline”, “smoothness”,
“est\_width”, “ramp”, “sidewalk\_left”, “sidewalk\_right”,
“ramp\_wheelchair”, “footway\_left”, “footway\_right”,  
“footway\_surface”, “priority”, “sidewalk\_both\_surface”, “path”,  
“pedestrian”, “capacity\_disabled”, “sidewalk\_left\_width”,  
“sidewalk\_right\_surface”

## Some notes from lit review:

Biagi et al. (2020) note that mapping accessibility related information
gained attention following WheelMap launch.

Key road elements for the accessible mobility are as following (based on
Biani et al. 2020):

-   sidewalk width
-   slope
-   kerb
-   road signs and their height
-   drains and other elements
-   parking
-   traffic lights (sound might be needed for the (partially) deaf)

<!-- -->

    # the code below was used to import the data.

    # et_walking <- c("wheelchair",
    # "kerb",
    # "disabled",
    # "mobility_scooter",
    # "handicap",
    # "foot",
    # "lit", # https://wiki.openstreetmap.org/wiki/Key:lit
    # "access",
    # "sidewalk",
    # "footway",
    # "incline",
    # "smoothness",
    # "est_width",
    # "ramp",
    # "sidewalk_left",
    # "sidewalk_right",
    # "ramp_wheelchair",
    # "footway_left",
    # "footway_right",          
    # "footway_surface", 
    # "priority",
    # "sidewalk_both_surface", 
    # "path",                                   
    # "pedestrian",
    # "capacity_disabled",
    # "sidewalk_left_width",                    
    # "sidewalk_right_surface")
    # 
    # oe_match_pattern("Yorkshire")
    # region_name <- "West Yorkshire"
    # 
    # wy_walking <- osmextract::oe_get(region_name,
    #                                  force_vectortranslate = TRUE,
    #                                  extra_tags = et_walking
    #                                  )

    # ============
    # `wy_walking` is the data we'll use in this section. 

    # wy_walking %>% str() 
    wy_walking %>% names()

    ##  [1] "osm_id"                 "name"                   "highway"               
    ##  [4] "waterway"               "aerialway"              "barrier"               
    ##  [7] "man_made"               "wheelchair"             "kerb"                  
    ## [10] "disabled"               "mobility_scooter"       "handicap"              
    ## [13] "foot"                   "lit"                    "access"                
    ## [16] "sidewalk"               "footway"                "incline"               
    ## [19] "smoothness"             "est_width"              "ramp"                  
    ## [22] "sidewalk_left"          "sidewalk_right"         "ramp_wheelchair"       
    ## [25] "footway_left"           "footway_right"          "footway_surface"       
    ## [28] "priority"               "sidewalk_both_surface"  "path"                  
    ## [31] "pedestrian"             "capacity_disabled"      "sidewalk_left_width"   
    ## [34] "sidewalk_right_surface" "z_order"                "other_tags"            
    ## [37] "geometry"

    wy_walking %>% nrow()

    ## [1] 233598

    wy_walking %>% ncol()

    ## [1] 37

## keys and tags

### key:foot

Legal access restriction for pedestrians.
<https://wiki.openstreetmap.org/wiki/Key:foot>

    foot_perc <- perc_ratio(wy_walking, "foot") # see chunk 3 (or 'additional functions' section)
    foot_perc

    ## [1] 5.3

    sf_col_table(wy_walking, "foot") # see chunk 4 (or 'additional functions' section)

    ## .
    ##         yes  designated  permissive          no     unknown     private 
    ##        7479        3652         516         374         151         123 
    ##   customers destination discouraged    delivery   emergency     limited 
    ##          38          34           3           1           1           1 
    ##      permit 
    ##           1

    wy_walking %>% select(foot) %>% plot

![](walking_cycling_files/figure-markdown_strict/unnamed-chunk-7-1.png)

    st_geometry(leeds) %>% plot(reset = FALSE)
    wy_walking %>% select(foot) %>% plot(add = TRUE)

![](walking_cycling_files/figure-markdown_strict/unnamed-chunk-7-2.png)

### <tag:footway>

The tag highway=footway is used for mapping minor pathways which are
used mainly or exclusively by pedestrians.
<https://wiki.openstreetmap.org/wiki/Tag:highway%3Dfootway>

    footway_perc <- perc_ratio(wy_walking, "footway") # see chunk 3 (or 'additional functions' section)
    footway_perc

    ## [1] 0.91

    sf_col_table(wy_walking, "footway") # see chunk 4 (or 'additional functions' section)

    ## .
    ##       sidewalk       crossing   access_aisle           left             no 
    ##           1505            505             77             11              9 
    ## traffic_island            yes           none          alley           link 
    ##              6              5              3              1              1 
    ##             np 
    ##              1

    wy_walking %>% select(footway) %>% plot

![](walking_cycling_files/figure-markdown_strict/unnamed-chunk-8-1.png)

    st_geometry(leeds) %>% plot(reset = FALSE)
    wy_walking %>% select(footway) %>% plot(add = TRUE)

![](walking_cycling_files/figure-markdown_strict/unnamed-chunk-8-2.png)

Not sure if `foot` and `footway` overlap? `footway` might be excessive
(overlap with `sidewalk` as well).

    wy_walking %>% select(foot) %>% filter(!is.na(foot)) %>% plot(reset= FALSE,
                                         col = "red",
                                         main = "tag:footway (green) and key:foot (red)")
    wy_walking %>% select(footway) %>% filter(!is.na(footway)) %>% plot(add = TRUE,
                                            col = "green")

![](walking_cycling_files/figure-markdown_strict/unnamed-chunk-9-1.png)

    # Let's figure out many of `footway` is within `foot`.

    foot_walking <- wy_walking %>% select(foot) %>% filter(!is.na(foot)) 
    # foot_walking %>% pull(foot) %>% table()
    footway_walking <- wy_walking %>% select(footway) %>% filter(!is.na(footway))

    # I'll use `st_within` argument because I want to make sure that *both* ways are being mapped in the same location rather than, for example, touching each other (for that I'd use `st_touches`).

    # footway_walking[foot_walking, op = st_contains] # how is this argument different from `st_within`?
    footway_in_foot <- foot_walking[footway_walking, op = st_within]
    foot_table <- footway_in_foot %>% # which paths in foot are within footway?
      pull(foot) %>% 
      table() # making a table with values and their counts
    foot_table

    ## .
    ## designated         no        yes 
    ##        167          1         44

    # the sum of the values in the `footway_table` should be the same even if we swap variables in subsetting. Let's check.
    footway_table <- footway_walking[foot_walking, op = st_within] %>% 
      pull(footway) %>% 
      table() 
    (foot_table %>% sum()) == (footway_table %>% sum())

    ## [1] TRUE

    # >  TRUE
    footway_table

    ## .
    ## access_aisle     crossing     sidewalk          yes 
    ##            1           96          114            1

    # Let's plot the ways in Leeds area
    st_geometry(leeds) %>% plot(reset = FALSE)
    foot_walking %>% plot(add = TRUE,
                          col = "blue")
    footway_walking %>% plot(add = TRUE,
                             col = "green",
                             lwd = 1.5)
    footway_in_foot %>% plot(add = TRUE,
                             col='red',
                             lwd = 2)
    title(sub = "blue -> foot, green -> footway, red -> footway in foot")

![](walking_cycling_files/figure-markdown_strict/unnamed-chunk-10-1.png)
Initial thoughts: most of the ways that are marked as both `foot` and
`footway` conglomerate in central Leeds. It makes sense because these
might be shopping streets/areas. However, it makes data cleaning a bit
harder: do we just create a new `sf` object where we add
`foot_in_footway` to `foot`? How do we preserve the information given by
`footway` if we merge? does `footway` give any useful information that
is needed to be preserved?

-   I guess it’s a matter of conceptualizing (quality) walking network…

Additional note: using `footway` to indicate additional sidewalk
information (e.g., left or right) has been depreciated and using
`sidewalk` is preferred.

    # a question on how `st_disjoint` works, if we have time...
    foot_walking %>% nrow() 

    ## [1] 12374

    foot_walking[footway_walking, op = st_disjoint] %>% pull(foot) %>%  table() %>% sum() # why does it return *all* rows and not only the ones it should be unconnected with?

    ## [1] 12374

    # footway_walking[foot_walking, ] %>% pull(footway) %>%  table() 
    # footway_walking[foot_walking, op = st_disjoint] %>% table() %>% sum()

### <tag:sidewalk>

The sidewalk (or pavement) is that part of a highway set aside for the
use of pedestrians and sometimes also cyclists, separated from the \[W\]
carriageway (or roadway).
<https://wiki.openstreetmap.org/wiki/Sidewalks>

    sidewalk_perc <- perc_ratio(wy_walking, "sidewalk") # see chunk 3 (or 'additional functions' section)
    sidewalk_perc

    ## [1] 0.93

    sf_col_table(wy_walking, "sidewalk") # see chunk 4 (or 'additional functions' section)

    ## .
    ##       both         no       left      right       none   separate     mapped 
    ##        820        452        431        200        163         79         23 
    ##        yes   crossing left;right 
    ##          6          1          1

    st_geometry(leeds) %>% plot(reset = FALSE)
    wy_walking %>% select(sidewalk) %>% 
      filter(!is.na(sidewalk) & sidewalk != "no" & sidewalk != "none") %>%
      plot(add = TRUE,
           col = 'red')

![](walking_cycling_files/figure-markdown_strict/unnamed-chunk-12-1.png)

    # let's check if `sidewalk` is within `footway`
    sidewalk_walking <- wy_walking %>% select(sidewalk) %>% filter(!is.na(sidewalk))
    footway_walking[sidewalk_walking, op = st_within] # empty; does

    ## Simple feature collection with 0 features and 1 field
    ## Bounding box:  xmin: NA ymin: NA xmax: NA ymax: NA
    ## Geodetic CRS:  WGS 84
    ## [1] footway  geometry
    ## <0 rows> (or 0-length row.names)

It’s empty. Most likely mappers use `sidewalk` tag instead of
`footway=sidewalk` even though this does not seem to be discouraged by
OSM. OSM does indicate, however, that ‘footway=yes’ should be replaced
with ‘sidewalk=yes’.

    # `sidewalk` does, however, contain `sidewalk` but I don't think it's a problem as they contain different information
    # foot_walking[sidewalk_walking, op = st_within] %>% plot()
    foot_walking[sidewalk_walking, op = st_within] %>% pull(foot) %>% table()

    ## .
    ## designated         no        yes 
    ##         12         15        134

### key:wheelchair

This tag may be used to mark places or ways that are suitable to be used
with a wheelchair and a person with a disability who uses another
mobility device (like a walker). *It should only be used if you are sure
about it*, this can either be because there’s a special sign or because
of personal experience/someone with a wheelchair told you.
<https://wiki.openstreetmap.org/wiki/Key:wheelchair>

    wheelchair_perc <- perc_ratio(wy_walking, "wheelchair") # see chunk 3 (or 'additional functions' section)
    sidewalk_perc

    ## [1] 0.93

    sf_col_table(wy_walking, "wheelchair") # see chunk 4 (or 'additional functions' section)

    ## .
    ##        yes         no designated    limited        bad permissive    unknown 
    ##        194        113          3          3          2          2          1

    # (((wy_walking %>%  filter(wheelchair == "yes" | wheelchair == "designated") %>% pull(wheelchair) %>% table() %>% sum()) / wy_walking %>% nrow() ) * 100) %>% signif(2)

    st_geometry(leeds) %>% plot(reset = FALSE,
                                col = "light gray")
    title("Leeds",
          sub = "blue -> mapped sidewalks (0.93%)
          red -> sidewalks accessible for wheelchair users (0.084%)"
          )
    wy_walking %>% select(sidewalk) %>% filter(sidewalk != "none" & sidewalk != "no") %>% plot(add = TRUE,
                                                                                               col = "blue",
                                                                                               lwd = 1.5)
    wy_walking %>% select(wheelchair) %>% filter(wheelchair == "yes" | wheelchair == "designated") %>%
      plot(add = TRUE,
           col = "red",
           lwd = 1.5)

![](walking_cycling_files/figure-markdown_strict/unnamed-chunk-15-1.png)

## key:kerb

Kerb is the edge where a road meets a sidewalk.
<https://wiki.openstreetmap.org/wiki/Key:kerb>

values:

-   raised: &gt;3cm, wheelchair=no
-   lowered: ~3cm, wheelchair = yes
-   flush: ~0cm, wheelchair = yes

flush kerbs might hinder the navigation process for the (partially)
blind, hence tactile paving key should be mapped at these locations.

    kerb_perc <- perc_ratio(wy_walking, "kerb") # see chunk 3 (or 'additional functions' section)
    kerb_perc

    ## [1] 0.073

    sf_col_table(wy_walking, "kerb") # see chunk 4 (or 'additional functions' section)

    ## .
    ##   flush lowered  raised     yes      fl     low 
    ##      92      51      21       3       2       1

    wy_walking %>% select(kerb) %>% plot()

![](walking_cycling_files/figure-markdown_strict/unnamed-chunk-16-1.png)

    st_geometry(leeds) %>% plot(reset = FALSE)
    wy_walking %>% select(kerb) %>% 
      filter(!is.na(kerb) ) %>%
      plot(add = TRUE,
           col = 'red')

![](walking_cycling_files/figure-markdown_strict/unnamed-chunk-16-2.png)

### key:tactile\_paving

### key:incline

Indicates a way’s grade, slope or incline. In the case of roads, there
is often a warning sign along the road.
<https://wiki.openstreetmap.org/wiki/Key:incline>

    incline_perc <- perc_ratio(wy_walking, "incline") # see chunk 3 (or 'additional functions' section)
    incline_perc

    ## [1] 0.57

    sf_col_table(wy_walking, "incline")  # see chunk 4 (or 'additional functions' section)

    ## .
    ##       up     down       0%      10%      12%      14%      20%      yes 
    ##      854      403       16       10        9        8        6        6 
    ##     -10% up;steep       5%    steep steep;up     -12%     -14%     -15% 
    ##        4        4        2        2        2        1        1        1 
    ##     -25%      -5%    -6.5%      -8%      10°    12.5%      15%       25 
    ##        1        1        1        1        1        1        1        1 
    ##      25%      30%      33%       4% down_25%  up/down 
    ##        1        1        1        1        1        1

    st_geometry(leeds) %>% plot(reset = FALSE)
    wy_walking %>% select(incline) %>% 
      filter(!is.na(incline) & incline == "down" | incline == "up") %>%
      plot(add = TRUE,
           col = 'red',
           lwd = 2)

![](walking_cycling_files/figure-markdown_strict/unnamed-chunk-18-1.png)

## 

    cbind(foot_perc,
          footway_perc,
          sidewalk_perc,
          wheelchair_perc,
          kerb_perc,
          incline_perc) 

    ##      foot_perc footway_perc sidewalk_perc wheelchair_perc kerb_perc
    ## [1,]       5.3         0.91          0.93            0.14     0.073
    ##      incline_perc
    ## [1,]         0.57
