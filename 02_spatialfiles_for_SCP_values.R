# Copyright 2021 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

#####################################################################################
.libPaths("C:/Program Files/R/R-4.0.5/library") # to ensure reading/writing libraries from C drive
# tz = Sys.timezone() # specify timezone in BC

# Load Packages
list.of.packages <- c("tidyverse", "lubridate","bcdata", "bcmaps","sp","sf", "rgdal", "readxl", "Cairo",
                      "OpenStreetMap", "ggmap", "nngeo", "raster")
# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
#####################################################################################

GISDir <- "//spatialfiles.bcgov/work/wlap/sry/Workarea/jburgar/CoViST"

aoi <- st_read(dsn=GISDir, layer="NewStudyBoundary_STSA")
aoi <- st_transform(aoi, crs=3005) # change projection to Albers

###--- read in secure species data
# list.files(path=GISDir)
CH_secure <- st_read(dsn=GISDir, layer = "CRITICAL_HABITAT_SECURED_12May2021")
EO_secure <- st_read(dsn=GISDir, layer = "OCCR_SENS_AREA_SVW_12April2021")

###--- read in publicly available data
# Critical Habitat
# bcdc_search("critical habitat", res_format = "wms")
# 1: Critical Habitat for federally-listed species at risk (posted) (other, wms, kml)
# ID: 076b8c98-a3f1-429b-9dae-03faed0c6aef
# Name: critical-habitat-for-federally-listed-species-at-risk-posted-

# CH_public <- bcdc_query_geodata("076b8c98-a3f1-429b-9dae-03faed0c6aef") %>%
#   # filter(INTERSECTS(aoi)) %>% # for some reason this isn't working
#   collect()
# aoi.CH_public <- CH_public %>% st_intersection(aoi)

fname="data/aoi.CH_public.rds"
# write_rds(aoi.CH_public, fname)
aoi.CH_public <- readRDS(fname)

ggplot()+
  geom_sf(data = CH_secure, col="red") +
  geom_sf(data = aoi.CH_public, col="blue")

###--- DECISION to BUFFER ELEMENT OCCURRENCES BY 250 M (BASED ON CH/IWMS RECOVERY STRATEGIES TO FIT WITH MOST SPECIES)
# Decision made by Joanna Burgar 12-May-2021
