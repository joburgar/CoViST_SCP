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
                      "OpenStreetMap", "ggmap", "nngeo", "raster", "units", "PNWColors","fasterize")
# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
#####################################################################################

###--- Area of Interest (AOI) is Study Area



##############################################################################################
###--- now start looking into "lands that contribute to conservation" layers
# thought is to include each of the different types of conservation protections and then stack the rasters
# this may give an idea of the "strength" of protection (i.e., >1 mechanism = stronger protection)

# 1.	National Wildlife Areas and Migratory Bird Sanctuaries obtained from federal government portal (filtered to only these two categories; http://www.ccea.org/download-carts-data/);
# 2.	BC Parks, Ecological Reserves, and Protected Areas : https://catalogue.data.gov.bc.ca/dataset/bc-parks-ecological-reserves-and-protected-areas;
# 3.	Local and Regional Greenspaces : https://catalogue.data.gov.bc.ca/dataset/local-and-regional-greenspaces (decision made to filter to Conservation Areas and Parks only and to exclude the Green Space category as it included non conservation areas such as Right-of-Ways);
# 4.	Conservation Lands  (Wildlife Management Areas and other administered conservation lands): https://catalogue.data.gov.bc.ca/dataset/conservation-lands;
# 5.	NGO fee simple lands  (contact Danielle Morrison from NTBC to get the sensitive layer, or the newer version of the non-sensitive BCGW layer): https://catalogue.data.gov.bc.ca/dataset/ngo-conservation-areas-fee-simple;
# 6.	Ungulate Winter Range - Approved : https://catalogue.data.gov.bc.ca/dataset/ungulate-winter-range-approved;
# 7.	Wildlife Habitat Areas – Approved (non-secured) : https://catalogue.data.gov.bc.ca/dataset/wildlife-habitat-areas-approved (there is also a secured version of this layer, as some polygons are identified as “data sensitive; the contact is Don Philip, Spatial Information Specialist (Don.Philip@gov.bc.ca): https://catalogue.data.gov.bc.ca/dataset/wildlife-habitat-areas-approved-secure-
# 8.	Old Growth Management Areas – Legal – Current ; https://catalogue.data.gov.bc.ca/dataset/old-growth-management-areas-legal-current


# Wildlife Habitat Area
# bcdc_search("Wildlife habitat area", res_format = "wms")
# 1: Wildlife Habitat Areas - Approved (other, wms, kml)
# ID: b19ff409-ef71-4476-924e-b3bcf26a0127

# aoi.WHA <- bcdc_query_geodata("b19ff409-ef71-4476-924e-b3bcf26a0127") %>%
#   # filter(INTERSECTS(aoi)) %>% # for some reason this isn't working
#   collect()
# aoi.WHA <- aoi.WHA %>% st_intersection(aoi)

# fname="data/aoi.WHA.rds"
# write_rds(aoi.WHA, fname)
aoi.WHA <- readRDS(fname)

aoi.WHA %>% count(COMMON_SPECIES_NAME)
aoi.WHA %>% group_by(COMMON_SPECIES_NAME) %>% summarise(sum(HECTARES))


ggplot()+
  geom_sf(data=aoi)+
  geom_sf(data=aoi.WHA, aes(fill=COMMON_SPECIES_NAME))

WHA_FCO <- st_nn(aoi.WHA, aoi.FCO, k=1, returnDist = T)

aoi.WHA$FCO_type <- unlist(WHA_FCO$nn)
aoi.WHA %>% dplyr::select(COMMON_SPECIES_NAME, FCO_type)
aoi.WHA$FCO_type <- aoi.FCO$OWNERSHIP_DESCRIPTION[match(aoi.WHA$FCO_type,rownames(aoi.FCO))]
write.csv(as.data.frame(aoi.WHA %>% group_by(COMMON_SPECIES_NAME) %>% count(FCO_type) %>% st_drop_geometry()), "out/WHA_FCO.csv", row.names = F)
summary(sta_sf)

# Forest Cover Ownership layer
bcdc_search("contribute to conservation", res_format = "wms")
# 1: Generalized Forest Cover Ownership (wms, kml, other)
# ID: 5fc4e8ce-dd1d-44fd-af17-e0789cf65e4e

# aoi.FCO <- bcdc_query_geodata("5fc4e8ce-dd1d-44fd-af17-e0789cf65e4e") %>%  # Forest Cover Ownership
#   #filter(INTERSECTS(aoi)) %>% # for some reason this isn't working
#   collect()
# aoi.FCO <- aoi.FCO %>% st_intersection(aoi)

fname="data/aoi.FCO.rds"
# write_rds(aoi.FCO, fname)
