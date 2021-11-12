This notebook focues on exploring OSM data on active travel with a
particular focus paid to the information (keys and tags) on
assessibility present in the dataset. Several questions to think about:

-   is cycling or walking mapped more extensively?
-   is the road information crucial to the disabled (smoothness,
    incline/decline, etc.) captured in the data?
    -   if so, how extensive it is? (note: extensive in relation to
        what?)
    -   is the data reliable? (note: how reliability can be
        conceptualized?)

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

    # datasets that will be used throughout this notebook
    wy <- readRDS("wy.Rds")
    wy_short <- readRDS("wy_short.Rds")
    wy_cycling_net <- readRDS("wy_cycling_net.Rds")
    wy_cn_short <- readRDS("wy_cn_short.Rds")
    wy_pct <- readRDS("wy_pct.Rds")
    leeds <- wy_pct %>% filter(lad_name == "Leeds")
    wy_walking <- readRDS("wy_walking.Rds")
    wy_walk_short <- readRDS("wy_walking.Rds")
