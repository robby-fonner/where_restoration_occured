###########################################
#
# Brittany King - OSU &
# Robby Fonner - NOAA Fisheries
#
# Study area Boundaries Shapefile used for Project
# 
#
########################################

#Load packages for all scripts in this project
library(here)
library(sf)
library(maptools)
library(tmap)
library(foreign)
library(rgdal)
library(stars)
library(sp)
library(tigris)
library(ggmap)
library(ggsidekick)
library(lwgeom)
library(tidycensus)
library(tidyverse)
library(raster)
library(fasterize)
library(elevatr)
library(exactextractr)
library(rmapshaper)
library(vtable)
library(units)

#Prevent "Loop 0 is not valid: Edge X has duplicate vertex ..." error in new R version. 
sf_use_s2(FALSE)

#Prepare environment
#Do not read strings as factors
options(stringsAsFactors=F)
#Do not use scientific notation of numbers
options(scipen = 999) 

#Set themes
theme_set(
  theme_sleek(base_size = 14))
theme_update(
  plot.background = element_rect(color = NA),
  plot.title.position = "plot"
)

#Import NHD and WBD data
#downloaded from: https://www.epa.gov/waterdata/nhdplus-pacific-northwest-data-vector-processing-unit-17
# 1711 - Puget Sound
# 1709 - Willamette
# 1702 - Upper Columbia
# 1703 - Yakima

study_area <- c( 1711, 1709, 1703)
st_area_names <- c("Puget Sound", "Willamette" , "Yakima")

#HU Polygons
huc12_wbd <- read_sf(here("Data/NHDPlusPN/NHDPlus17/WBDSnapshot/WBD/WBD_Subwatershed.shp")) %>%
  filter( as.numeric(substr(HUC_12,1,4)) %in% study_area ) %>%
  st_transform(4326) %>%
  group_by(HUC_12, HU_12_NAME) %>%
  summarise(do_union = TRUE) %>%
  ungroup() %>%
  mutate(
    HU_8_NAME = case_when(
      substr(HUC_12,1,4) == "1711"  ~ "PUGET SOUND",
      substr(HUC_12,1,4) == "1709"  ~ "WILLAMETTE",
      substr(HUC_12,1,4) == "1703"  ~ "YAKIMA"), 
    huc_area = as.numeric(set_units( st_area(geometry), ha))
    
    )




