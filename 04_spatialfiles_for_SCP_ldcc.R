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
# keep in mind that WGS84 lat/long espg = 4326; BC Albers espg = 3005; NAD83 / UTM zone 10N espg = 26910

SBDir <- "//spatialfiles.bcgov/work/srm/sry/Local/projlib/StewBase/Small_Home_Range_SAR/Analysis/Input/Focal_areas"
GISDir <- "//spatialfiles.bcgov/work/wlap/sry/Workarea/jburgar/CoViST"

aoi <- st_read(dsn=GISDir, layer="CoViST_Study_Boundary_July2021") # updated to reflect decision at June 22 STSA meeting
aoi <- st_transform(aoi, crs=3005) # change projection to Albers

aoi <- st_zm(aoi) # remove the z dimension
aoi$Shape_Area <- st_area(aoi) # ensure correct area and then change to km2
aoi$Area_km2 <- aoi$Shape_Area * 1e-6
aoi <- drop_units(aoi)
aoi <- aoi[4] # delete m2 area and length


##############################################################################################
###--- now start looking into "lands that contribute to conservation" layers
# thought is to include each of the different types of conservation protections and then stack the rasters
# this may give an idea of the "strength" of protection (i.e., >1 mechanism = stronger protection)


###--- load conservation layers from bcdata
# using the bc data warehouse option to aoi, unless need to download from external portals
###--- layers used in the first run of prioritizr
# 1.	National Wildlife Areas and Migratory Bird Sanctuaries obtained from federal government portal (filtered to only these two categories; http://www.ccea.org/download-carts-data/);
# as of August 2021, the Canadian Protected and Conserved Areas Database: https://www.canada.ca/en/environment-climate-change/services/national-wildlife-areas/protected-conserved-areas-database.html
# 2.	BC Parks, Ecological Reserves, and Protected Areas : https://catalogue.data.gov.bc.ca/dataset/bc-parks-ecological-reserves-and-protected-areas;
# 3.	Local and Regional Greenspaces : https://catalogue.data.gov.bc.ca/dataset/local-and-regional-greenspaces (decision made to filter to Conservation Areas and Parks only and to exclude the Green Space category as it included non conservation areas such as Right-of-Ways);
# 4.	Conservation Lands  (Wildlife Management Areas and other administered conservation lands): https://catalogue.data.gov.bc.ca/dataset/conservation-lands;
# 5.	NGO fee simple lands  (contact Danielle Morrison from NTBC to get the sensitive layer, or the newer version of the non-sensitive BCGW layer): https://catalogue.data.gov.bc.ca/dataset/ngo-conservation-areas-fee-simple;
# 6.	Ungulate Winter Range - Approved : https://catalogue.data.gov.bc.ca/dataset/ungulate-winter-range-approved;
# 7.	Wildlife Habitat Areas – Approved (non-secured) : https://catalogue.data.gov.bc.ca/dataset/wildlife-habitat-areas-approved (there is also a secured version of this layer, as some polygons are identified as “data sensitive; the contact is Don Philip, Spatial Information Specialist (Don.Philip@gov.bc.ca): https://catalogue.data.gov.bc.ca/dataset/wildlife-habitat-areas-approved-secure-
# 8.	Old Growth Management Areas – Legal – Current ; https://catalogue.data.gov.bc.ca/dataset/old-growth-management-areas-legal-current

# Federal Protected Areas (Canadian Wildlife Service owned)
# National Wildlife Areas, Migratory Bird Sanctuaries, Nature Reserves & Conservation Areas
download.file("https://cws-scf.ca/CPCAD-BDCAPC_Dec2020.gdb.zip", "CPCAD-BDCAPC_Dec2020.gdb.zip")
unzip("CPCAD-BDCAPC_Dec2020.gdb.zip", exdir = ".")
fgdb = "CPCAD-BDCAPC_Dec2020.gdb"

# List all feature classes in a file geodatabase
st_layers(fgdb)

# Read the feature class
CPCAD_aoi <- st_read(dsn=fgdb,layer="CPCAD_Dec2020") %>%
  st_transform(crs=3005) %>% st_intersection(aoi)
CPCAD_aoi$Area_km2 <- st_area(CPCAD_aoi)*1e-6
CPCAD_aoi <- drop_units(CPCAD_aoi)

ggplot()+
  geom_sf(data = aoi)+
  geom_sf(data = CPCAD_aoi, aes(fill=TYPE_E))

as.data.frame(CPCAD_aoi %>% filter(TYPE_E=="A - Park") %>% count(NAME_E))
as.data.frame(CPCAD_aoi %>% filter(TYPE_E=="C - Park") %>% count(NAME_E))
as.data.frame(CPCAD_aoi %>% filter(TYPE_E=="Conservation Area") %>% count(NAME_E))
CPCAD_aoi %>% group_by(OWNER_E,TYPE_E) %>% summarise(sum(Area_km2)) %>% st_drop_geometry()

CPCAD_aoi %>% filter(grepl("Canadian Wildlife Service",OWNER_E)) %>% st_drop_geometry()
CPCAD_aoi %>% filter(TYPE_E %in% c("National Wildlife Area","Migratory Bird Sanctuary", "Nature Reserve", "Conservation Area")) %>%
  dplyr::select(NAME_E, TYPE_E, OWNER_E, Area_km2) %>% st_drop_geometry()

aoi.CPCAD <- CPCAD_aoi %>% filter(grepl("Canadian Wildlife Service",OWNER_E))

# BC Parks, Ecological Reserves, and Protected Areas : https://catalogue.data.gov.bc.ca/dataset/bc-parks-ecological-reserves-and-protected-areas;
bcdc_search("BC Parks, Ecological Reserves, and Protected Areas", res_format = "wms")
# BC Parks, Ecological Reserves, and Protected Areas (wms, kml, other)
# ID: 1130248f-f1a3-4956-8b2e-38d29d3e4af7
# Name: bc-parks-ecological-reserves-and-protected-areas

aoi.BCPA <- bcdc_query_geodata("1130248f-f1a3-4956-8b2e-38d29d3e4af7") %>%  # bc-parks-ecological-reserves-and-protected-areas
  filter(BBOX(st_bbox(aoi))) %>%
  collect()
aoi.BCPA <- aoi.BCPA %>% st_intersection(aoi)
as.data.frame(aoi.BCPA %>% count(PROTECTED_LANDS_NAME)%>% st_drop_geometry())

# # Protected Lands Access Restrictions (csv, other, wms, kml)
# # ID: a10c0626-424c-4e9f-b84a-673c02e4d2b9
# # Name: protected-lands-access-restrictions
#
# aoi.PLAR <- bcdc_query_geodata("a10c0626-424c-4e9f-b84a-673c02e4d2b9") %>%  # protected-lands-access-restrictions
#   filter(BBOX(st_bbox(aoi))) %>%
#   collect()
# aoi.PLAR <- aoi.PLAR %>% st_intersection(aoi)
# summary(aoi.PLAR)

# Protected Lands Facilities (other, csv, wms, kml)
# ID: d58353ac-b063-4d8f-a199-c102d0b9407f
# Name: protected-lands-facilities
# aoi.PLF <- bcdc_query_geodata("d58353ac-b063-4d8f-a199-c102d0b9407f") %>%  # protected-lands-facilities
#   filter(BBOX(st_bbox(aoi))) %>%
#   collect()
# aoi.PLF <- aoi.PLF %>% st_intersection(aoi)
# summary(aoi.PLF)

# NGO Conservation Areas - Fee Simple (other, wms, kml, pdf)
# ID: a306e21b-58d6-4b71-bac7-f3b1c8a4c779
# Name: ngo-conservation-areas-fee-simple

aoi.NGOCA <- bcdc_query_geodata("a306e21b-58d6-4b71-bac7-f3b1c8a4c779") %>%  # ngo-conservation-areas-fee-simple  filter(BBOX(st_bbox(aoi))) %>%
  collect()
aoi.NGOCA <- aoi.NGOCA %>% st_intersection(aoi)
aoi.NGOCA$Area_km2 <- st_area(aoi.NGOCA)*1e-6
aoi.NGOCA <- drop_units(aoi.NGOCA)

ggplot()+
  geom_sf(data=aoi)+
  geom_sf(data=aoi.NGOCA)
names(aoi.NGOCA)

# CPCAD_aoi %>% group_by(OWNER_E,TYPE_E) %>% summarise(area = sum(Area_km2)) %>% st_drop_geometry()
# aoi.NGOCA %>% group_by(LEAD_SECUREMENT_ORG) %>% summarise(area = sum(Area_km2)) %>% st_drop_geometry()
# aoi.NGOCA contains same # and area of NGO lands as CPCAD, stick with NGOCA (likely updated more frequently)

# Local and Regional Greenspaces : https://catalogue.data.gov.bc.ca/dataset/local-and-regional-greenspaces (decision made to filter to Conservation Areas and Parks only and to exclude the Green Space category as it included non conservation areas such as Right-of-Ways);
bcdc_search("Local and Regional Greenspaces", res_format = "wms")
# Local and Regional Greenspaces (wms, kml, xlsx, other)
# ID: 6a2fea1b-0cc4-4fc2-8017-eaf755d516da
# Name: local-and-regional-greenspaces

aoi.LRG <- bcdc_query_geodata("6a2fea1b-0cc4-4fc2-8017-eaf755d516da") %>%  # local-and-regional-greenspaces
  filter(BBOX(st_bbox(aoi))) %>% # works with BBOX but not INTERSECT
  collect()
aoi.LRG <- aoi.LRG %>% st_intersection(aoi)
aoi.LRG$Area_km2 <- st_area(aoi.LRG)*1e-6
aoi.LRG <- drop_units(aoi.LRG)

aoi.LRG %>% group_by(PARK_TYPE) %>% count(PARK_PRIMARY_USE) %>% st_drop_geometry()
aoi.LRG %>% group_by(PARK_TYPE, PARK_PRIMARY_USE) %>% summarise(area = sum(Area_km2)) %>% st_drop_geometry()
aoi.LRG %>% group_by(PARK_PRIMARY_USE, PARK_TYPE) %>% summarise(area = sum(Area_km2)) %>% st_drop_geometry()

aoi.LRG %>% filter(PARK_TYPE=="Federal") %>% dplyr::select(PARK_NAME, Area_km2)# Widgeon Marsh - 6.08 km2 # must include as extension of federal (CPCAD)
aoi.LRG %>% filter(PARK_TYPE=="Federal") %>% dplyr::select(PARK_NAME, Area_km2)# Widgeon Marsh - 6.08 km2 # must include as extension of federal (CPCAD)

aoi.LRG %>% filter(PARK_PRIMARY_USE %in% c("Conservation Area","Park")) %>% summarise(area = sum(Area_km2)) %>% st_drop_geometry()
264 / sum(aoi.LRG$Area_km2) # Conservation Areas & Parks comprise 78% of the local and regional greenspaces
# instead of omitting all other greenspace, RECOMMEND splitting into 2
# Conservation Areas & Parks = "locked in" lands that contribute to conservation
# all other parcels = remove the "cost" of the land (i.e., add to the cost raster layer and change cost to 0)

ggplot() +
  geom_sf(data=aoi.LRG %>% filter(PARK_TYPE=="Federal"), col="blue")+
  geom_sf(data=aoi.CPCAD %>% filter(grepl("Widgeon",NAME_E)), col="red")

# Conservation Lands  (Wildlife Management Areas and other administered conservation lands): https://catalogue.data.gov.bc.ca/dataset/conservation-lands;
bcdc_search("Wildlife Management Areas", res_format = "wms")
# TANTALIS - Wildlife Management Areas (other, wms, kml)
# ID: f3ece977-aa7f-4cb2-b7d0-de64155f6c83
# Name: tantalis-wildlife-management-areas
aoi.WMA <- bcdc_query_geodata("f3ece977-aa7f-4cb2-b7d0-de64155f6c83") %>% # tantalis-wildlife-management-areas
  filter(BBOX(st_bbox(aoi))) %>% # works with BBOX but not INTERSECT
  collect()
aoi.WMA <- aoi.WMA %>% st_intersection(aoi)
aoi.WMA$Area_km2 <- st_area(aoi.WMA)*1e-6
aoi.WMA <- drop_units(aoi.WMA)

aoi.WMA %>% group_by(WILDLIFE_MANAGEMENT_AREA_NAME) %>% summarise(area=sum(Area_km2)) %>% st_drop_geometry()

# Ungulate Winter Range - Approved : https://catalogue.data.gov.bc.ca/dataset/ungulate-winter-range-approved;
bcdc_search("Ungulate Winter Range", res_format = "wms")
# Ungulate Winter Range - Approved (other, wms, kml)
# ID: 712bd887-7763-4ed3-be46-cdaca5640cc1
# Name: ungulate-winter-range-approved

aoi.UWR <- bcdc_query_geodata("712bd887-7763-4ed3-be46-cdaca5640cc1") %>% # ungulate-winter-range-approved
  filter(BBOX(st_bbox(aoi))) %>%
  collect()
aoi.UWR <- aoi.UWR %>% st_intersection(aoi)
aoi.UWR$Area_km2 <- st_area(aoi.UWR)*1e-6
aoi.UWR <- drop_units(aoi.UWR)

aoi.UWR %>% group_by(UWR_NUMBER) %>% summarise(area=sum(Area_km2)) %>% st_drop_geometry()
aoi.UWR %>% group_by(TIMBER_HARVEST_CODE) %>% summarise(area=sum(Area_km2)) %>% st_drop_geometry()


# Wildlife Habitat Areas – Approved (non-secured) : https://catalogue.data.gov.bc.ca/dataset/wildlife-habitat-areas-approved (there is also a secured version of this layer, as some polygons are identified as “data sensitive; the contact is Don Philip, Spatial Information Specialist (Don.Philip@gov.bc.ca): https://catalogue.data.gov.bc.ca/dataset/wildlife-habitat-areas-approved-secure-
bcdc_search("Wildlife habitat area", res_format = "wms")
# Wildlife Habitat Areas - Approved (other, wms, kml)
# ID: b19ff409-ef71-4476-924e-b3bcf26a0127
# Name: wildlife-habitat-areas-approved

aoi.WHA <- bcdc_query_geodata("b19ff409-ef71-4476-924e-b3bcf26a0127") %>% # wildlife-habitat-areas-approved
  filter(BBOX(st_bbox(aoi))) %>%
  collect()
aoi.WHA <- aoi.WHA %>% st_intersection(aoi)
aoi.WHA$Area_km2 <- st_area(aoi.WHA)*1e-6
aoi.WHA <- drop_units(aoi.WHA)

aoi.WHA %>% group_by(TAG) %>% summarise(area=sum(Area_km2)) %>% st_drop_geometry()
aoi.WHA %>% group_by(TIMBER_HARVEST_CODE) %>% summarise(area=sum(Area_km2)) %>% st_drop_geometry()


# Old Growth Management Areas – Legal – Current ; https://catalogue.data.gov.bc.ca/dataset/old-growth-management-areas-legal-current
bcdc_search("Old Growth Management Areas", res_format = "wms")
# Old Growth Management Areas - Legal - Current (other, wms, kml)
# ID: 1b30f3bd-0ad0-4128-916b-66c6dd91dea4
# Name: old-growth-management-areas-legal-current

aoi.OGMA <- bcdc_query_geodata("1b30f3bd-0ad0-4128-916b-66c6dd91dea4") %>% # old-growth-management-areas-legal-current
  filter(BBOX(st_bbox(aoi))) %>%
  collect()
aoi.OGMA <- aoi.OGMA %>% st_intersection(aoi)
aoi.OGMA$Area_km2 <- st_area(aoi.OGMA)*1e-6
aoi.OGMA <- drop_units(aoi.OGMA)

sum(aoi.OGMA$Area_km2)

ggplot()+
  geom_sf(data=aoi)+
  geom_sf(data=aoi.OGMA)


###--- Managed Lands (as per the LDCC layer)
# includes: Recreation Sites, VQO_retain, Land Act Reserves, UWR & WHA conditional harves, and community watersheds

# Recreation Sites (pull from Recreation Polygon layer)
bcdc_search("Recreation Sites", res_format = "wms")
# Recreation Polygons (other, wms, kml)
# ID: 263338a7-93ee-49c1-83e8-13f0bde70833
# Name: recreation-polygons
aoi.RS <- bcdc_query_geodata("263338a7-93ee-49c1-83e8-13f0bde70833") %>% # recreation-polygons
  filter(BBOX(st_bbox(aoi))) %>%
  collect()
aoi.RS <- aoi.RS %>% st_intersection(aoi) %>% filter(PROJECT_TYPE=="Recreation Site")
aoi.RS$Area_km2 <- st_area(aoi.RS)*1e-6
aoi.RS <- drop_units(aoi.RS)

aoi.RS %>% group_by(PROJECT_TYPE) %>% summarise(area = sum(Area_km2)) %>% st_drop_geometry()

# Visual Quality (not sure about this one - need confirmation on the filtering field)
bcdc_search("Visual Quality", res_format = "wms")
# Visual Landscape Inventory (other, wms, kml)
# ID: 4e941067-20ec-4b5d-bca3-8831c9b2e4db
# Name: visual-landscape-inventory
aoi.VQO <- bcdc_query_geodata("4e941067-20ec-4b5d-bca3-8831c9b2e4db") %>% # visual-landscape-inventory
  filter(BBOX(st_bbox(aoi))) %>%
  collect()
aoi.VQO <- aoi.VQO %>% st_intersection(aoi) %>% filter(!is.na(REC_EVQO_CODE))
aoi.VQO$Area_km2 <- st_area(aoi.VQO)*1e-6
aoi.VQO <- drop_units(aoi.VQO)
sum(aoi.VQO$Area_km2) / aoi$Area_km2

aoi.VQO %>% group_by(REC_EVQO_CODE) %>% summarise(area = sum(Area_km2)) %>% st_drop_geometry()


ggplot()+
  geom_sf(data = aoi)+
  geom_sf(data = aoi.VQO)

# Land Act Reserves
bcdc_search("Land Act Reserves", res_format = "wms")
# TANTALIS - Crown Land Reserves and Notations (other, wms, kml)
# ID: 589cb979-731f-4151-bb2b-10ed66278099
# Name: tantalis-crown-land-reserves-and-notations
aoi.LAR <- bcdc_query_geodata("589cb979-731f-4151-bb2b-10ed66278099") %>% # tantalis-crown-land-reserves-and-notations
  filter(BBOX(st_bbox(aoi))) %>%
  collect()
aoi.LAR <- aoi.LAR %>% st_intersection(aoi)
aoi.LAR$Area_km2 <- st_area(aoi.LAR)*1e-6
aoi.LAR <- drop_units(aoi.LAR)

as.data.frame(aoi.LAR %>% group_by(TENURE_SUBTYPE, TENURE_PURPOSE) %>% summarise(area = sum(Area_km2)) %>% st_drop_geometry())
aoi.LAR %>% filter(TENURE_PURPOSE=="ENVIRONMENT, CONSERVATION, & RECR") %>% group_by(TENURE_SUBTYPE) %>% summarise(area = sum(Area_km2)) %>% st_drop_geometry()


# 6: Conservation Lands (other, wms, kml)
# ID: 68327529-c0d5-4fcb-b84e-f8d98a7f8612
# Name: conservation-lands
aoi.CL <- bcdc_query_geodata("68327529-c0d5-4fcb-b84e-f8d98a7f8612") %>% # tantalis-crown-land-reserves-and-notations
  filter(BBOX(st_bbox(aoi))) %>%
  collect()
aoi.CL <- aoi.CL %>% st_intersection(aoi)
aoi.CL$Area_km2 <- st_area(aoi.CL)*1e-6
aoi.CL <- drop_units(aoi.CL)

as.data.frame(aoi.CL %>% group_by(CONSERVATION_LAND_TYPE, TENURE_TYPE) %>% summarise(area = sum(Area_km2)) %>% st_drop_geometry())


# Community Watersheds
bcdc_search("Community watershed", res_format = "wms")
# 2: Community Watersheds - Current (other, wms, kml)
# ID: bc57faf7-23e4-43fe-918a-e999936dbafa
# Name: community-watersheds-current
aoi.CWtr <- bcdc_query_geodata("bc57faf7-23e4-43fe-918a-e999936dbafa") %>% # community-watersheds-current
  filter(BBOX(st_bbox(aoi))) %>%
  collect()
aoi.CWtr <- aoi.CWtr %>% st_intersection(aoi)
aoi.CWtr$Area_km2 <- st_area(aoi.CWtr)*1e-6
aoi.CWtr <- drop_units(aoi.CWtr)
names(aoi.CWtr)

as.data.frame(aoi.CWtr %>% group_by(CW_LEGISLATION) %>% summarise(area = sum(Area_km2)) %>% st_drop_geometry())
