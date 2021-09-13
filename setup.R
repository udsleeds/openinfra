# Aim: This script documents the steps to set up this repo for reproducibility
# and sharing of knowledge/know-how

# The repo was created on github with the following command in the system CLI
# (requires the github cli tool)
# cd papers
# gh repo create odjitter

# # Open RStudio and create a project as follows
# rstudioapi::openProject("~/papers/odjitter")
#
# # Set-up with usethis
# usethis::use_readme_rmd()
# unlink(".git/hooks/pre-commit")
# # Edit the paper/readme
# file.edit("README.Rmd")
#
# usethis::use_github_action("render-rmarkdown")
# file.edit(".github/workflows/render-rmarkdown.yaml")

# Get and process OD data for Edinburgh
library(tidyverse)
library(sf)

iz_zones11_uk = readRDS("iz_zones11_uk_simplified.Rds")
iz_cents11_uk = readRDS("iz_cents11_uk.Rds")
edinburgh_region = readRDS("edinburgh_region.Rds")
od_iz = readRDS("od_izo.Rds")

# Subset to Edinburgh
iz_cents11_ed = iz_cents11_uk[edinburgh_region, ]
iz_zones11_ed = iz_zones11_uk %>% filter(InterZone %in% iz_cents11_ed$InterZone)
od_iz_ed = od_iz %>%
  filter(geo_code1 %in% iz_zones11_ed$InterZone) %>%
  filter(geo_code2 %in% iz_zones11_ed$InterZone)
plot(iz_zones11_ed$geometry)

saveRDS(iz_zones11_ed, "iz_zones11_ed.Rds")
saveRDS(iz_cents11_ed, "iz_cents11_ed.Rds")
saveRDS(od_iz_ed, "od_iz_ed.Rds")

f = list.files(pattern = "_ed")
piggyback::pb_upload(f, repo = "itsleeds/od")
piggyback::pb_download_url(f)
