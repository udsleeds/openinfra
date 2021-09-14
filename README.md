
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![.github/workflows/render-rmarkdown.yaml](https://github.com/Robinlovelace/odjitter/actions/workflows/render-rmarkdown.yaml/badge.svg)](https://github.com/Robinlovelace/odjitter/actions/workflows/render-rmarkdown.yaml)

# Open access data for transport research: tools, modelling and simulation

This repo contains code and example data to explore the utility of open
data for transport planning and, specifically, open data on transport
infrastructure. It was created to support a LIDA internship, the
objectives of which are to:

-   develop new methods for bulk downloading, querying and analysing
    OpenStreetMap data on transport infrastructure
-   assess the quality of OSM data with reference to ‘ground truth’
    datasets including data from satellite imagery and Ordnance Survey
    data
-   publish reproducible methods and documentation on using OSM data for
    transport planning with reference to the strengths and potential
    pitfalls of the data
-   develop a typology of transport infrastructure data and data schemas
    for each infrastructure type and an actionable definition of ‘active
    travel infrastructure’
-   develop ‘OSM transport infrastructure data packs’ for every
    transport authority in Great Britain, with layers reflecting a
    typology of transport infrastructure data developed in the project
-   develop and publish guidance on using OSM data for transport
    planning

There are already good tools open tools for working with transport
infrastructure data, including the R packages
[`osmextract`](https://docs.ropensci.org/osmextract/),
[`stplanr`](https://docs.ropensci.org/stplanr/), and
[`sfnetworks`](https://luukvdmeer.github.io/sfnetworks/). These, and
packages written in other languages such as Julia and Python, are
largely academic-led and technical projects with little uptake among
practitioners. This project will be about gaining an understanding of
the treatment of transport infrastructure in OSM and, vitally, how OSM
and other open datasets can lead to more evidence-based transport plans.

The brief example below shows how quickly you can get started with OSM
data using command-line driven open source software to ensure
reproducibility and scalability.

# Example of transport infrastructure in R

# References
