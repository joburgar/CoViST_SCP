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
                      "OpenStreetMap", "ggmap", "nngeo", "raster", "units", "PNWColors")
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

st_write(aoi, paste0(getwd(),"/out/aoi.shp"), delete_layer = TRUE)

###--- load cost layers from bcdata
# using the bc data warehouse option to aoi
# unless layer too large, then static layer download from clipped through ArcCatalogue or through bc data warehouse submission portal

# From James Sanders (GIS Analyst) on Forest Ownership vs Parcel Map
# Forest ownership is a more reliable source for private vs crown in more rural areas.
# Parcel map is more reliable for urban areas. Parcel map is also updated more frequently, whereas Forest ownership is maybe once per year. Parcel map feeds into Forest ownership.
# (Crown) Land tenures are from Tantlas. Tantlas updates are nightly. Tantlas (from what I understand) Land tenures are Crown land only.

# Forest Cover Ownership layer
# bcdc_search("forest ownership", res_format = "wms")
# 1: Generalized Forest Cover Ownership (wms, kml, other)
# ID: 5fc4e8ce-dd1d-44fd-af17-e0789cf65e4e

aoi.FCO <- bcdc_query_geodata("5fc4e8ce-dd1d-44fd-af17-e0789cf65e4e") %>%  # Forest Cover Ownership
  filter(BBOX(st_bbox(aoi))) %>% # works with BBOX but not INTERSECT
  collect()
aoi.FCO <- aoi.FCO %>% st_intersection(aoi)

# aoi.FCO %>% count(OWN)
write.csv(aoi.FCO %>% st_drop_geometry(), "out/aoi.FCO.csv", row.names = FALSE)

FCO.type <- as.data.frame(aoi.FCO %>% group_by(OWN) %>% count(OWNERSHIP_DESCRIPTION) %>% st_drop_geometry())

ggplot()+
  geom_sf(data=aoi) +
  geom_sf(data=aoi.FCO %>% filter(OWN==41), col="red") + # 41 = Private - Land Claim Settlement Area
  geom_sf(data=aoi.FCO %>% filter(OWN==52), col="blue") + # 41 = Federal - Indian Reserve
  geom_sf(data=aoi.FCO %>% filter(OWN==53), col="darkgreen")


# to union each ownership sub-type, resolving category overlaps
aoi.FCO.OWN.union <-aoi.FCO %>% group_by(OWN) %>%
  summarise(across(geometry, ~ st_union(.)), .groups = "keep") %>%
  summarise(across(geometry, ~ st_combine(.)))
# calculate area per ownership type
aoi.FCO.OWN.union$areakm2 <- st_area(aoi.FCO.OWN.union)* 1e-6
aoi.FCO.OWN.union <- drop_units(aoi.FCO.OWN.union)
# ownership types grouped in "tens"; e.g., 52, 53, 54 are all Federal
aoi.FCO.OWN.union$OWN_Grp <- as.numeric(substr(aoi.FCO.OWN.union$OWN,1,1))

ggplot()+
  geom_sf(data=aoi) +
  geom_sf(data=aoi.FCO.OWN.union, aes(fill=OWN_Grp))+
  scale_fill_gradientn(colours = pnw_palette("Bay",6))

ggplot()+
  geom_sf(data=aoi) +
  geom_sf(data=aoi.FCO.OWN.union, aes(fill=OWN_Grp))+
  scale_fill_gradientn(colours = pnw_palette("Bay",6))

ggplot()+
  geom_sf(data=aoi) +
  geom_sf(data=aoi.FCO.OWN.union %>% filter(OWN_Grp==8), aes(fill=OWN))+
  scale_fill_gradientn(colours = pnw_palette("Bay",6))

options(scipen = 999)
FCO_area <- as.data.frame(aoi.FCO.OWN.union %>% group_by(OWN) %>% st_drop_geometry())
FCO_area$percSA <- FCO_area$areakm2 / (st_area(aoi) * 1e-6) * 100
FCO_area$OWNERSHIP_DESCRIPTION <- FCO.type$OWNERSHIP_DESCRIPTION[match(FCO_area$OWN, FCO.type$OWN)]
FCO_area <- drop_units(FCO_area)
write.csv(FCO_area, "out/aoi.FCO_area.csv", row.names = FALSE)


# to union each ownership type, considering new groupings
# Crown Provincial = 60,61,62,65,66,67,68,69,72,74,75,77,79
# Federal = 53,54
# First Nation = 52,78,41
# Municipal = 80, 81
# Private = 40
# Unknown / Other = 91,99

aoi.FCO$Final_Grp <- ifelse(aoi.FCO$OWN %in% c(60,61,62,65,66,67,68,69,72,74,75,77,79), "Provincial",
                            ifelse(aoi.FCO$OWN %in% c(53,54), "Federal",
                                   ifelse(aoi.FCO$OWN %in% c(52,78,41), "First Nation",
                                          ifelse(aoi.FCO$OWN %in% c(80,81), "Municipal",
                                                 ifelse(aoi.FCO$OWN==40, "Private",
                                                        ifelse(aoi.FCO$OWN %in% c(91,99), "Other", NA))))))
aoi.FCO %>% group_by(Final_Grp) %>% count(OWN) %>% st_drop_geometry()

aoi.FCO.Final.union <-aoi.FCO %>% group_by(Final_Grp) %>%
  summarise(across(geometry, ~ st_union(.)), .groups = "keep") %>%
  summarise(across(geometry, ~ st_combine(.)))
# calculate area per ownership type
aoi.FCO.Final.union$areakm2 <- st_area(aoi.FCO.Final.union)* 1e-6
aoi.FCO.Final.union$percSA <- aoi.FCO.Final.union$areakm2 / (st_area(aoi) * 1e-6) * 100
aoi.FCO.Final.union <- drop_units(aoi.FCO.Final.union)
aoi.FCO.Final.union %>% st_drop_geometry()

levels(aoi.FCO.Final.union$Final_Grp)
aoi.FCO.Final.union$Final_Grp <- factor(aoi.FCO.Final.union$Final_Grp,
                                        levels = c("Provincial","Federal","First Nation","Municipal","Private","Other"))
ggplot()+
  geom_sf(data=aoi) +
  geom_sf(data=aoi.FCO.Final.union, aes(fill=Final_Grp), lwd=0)+
  scale_fill_manual(values = (pnw_palette("Starfish",6)))

aoi.FCO.Final.union %>% summarise(sum(percSA), sum(areakm2)) %>% st_drop_geometry()
# # A tibble: 1 x 2
# `sum(percSA)` `sum(areakm2)`
#   99.9          8619.

# Parcel Map BC layer
bcdc_search("parcel map", res_format = "wms")
# 1: ParcelMap BC Parcel Fabric (fgdb, wms, kml, html, other)
# ID: 4cf233c2-f020-4f7a-9b87-1923252fbc24
# Name: parcelmap-bc-parcel-fabric
# aoi.PMBC <- bcdc_query_geodata("4cf233c2-f020-4f7a-9b87-1923252fbc24") %>%  # Parcel Map BC
#   filter(BBOX(st_bbox(aoi))) %>% # works with BBOX but not INTERSECT
#   collect()
# aoi.PMBC <- aoi.PMBC %>% st_intersection(aoi)
# this was taking hours to download - need to do through BCGW online with aoi or via ArcCatalogue
# for more dynamic output, could also go through bcdata but with extra conditions to reduce size
# for example, select only key columns or only download for urban areas, such as code below

# PCMB_unk <- bcdc_query_geodata("4cf233c2-f020-4f7a-9b87-1923252fbc24") %>%
#   filter(BBOX(st_bbox(aoi))) %>% # works with BBOX but not INTERSECT
#   filter(OWNER_TYPE %in% c("Unknown")) %>%
#   #filter(OWNER_TYPE %in% c("Private", "Municipal", "Unknown")) %>%
#   dplyr::select(PARCEL_CLASS, OWNER_TYPE, MUNICIPALITY, REGIONAL_DISTRICT) %>%
#   collect()
#
# aoi.PMBC <- PMBC %>% st_intersection(aoi) %>% st_zm() # clip to aoi and remove z dimension

aoi.PMBC <- st_read(dsn=GISDir, layer="CoViST_ParcelMap") %>%
  dplyr::select(PARCEL_CLA, OWNER_TYPE, MUNICIPALI, REGIONAL_D)
aoi.PMBC <- st_zm(aoi.PMBC) # remove the z dimension to reduce size

# to union each ownership sub-type, resolving category overlaps
aoi.PMBC.OWN.union <-aoi.PMBC %>% group_by(OWNER_TYPE, PARCEL_CLA) %>%
  summarise(across(geometry, ~ st_union(.)), .groups = "keep") %>%
  summarise(across(geometry, ~ st_combine(.)))
# calculate area per ownership type
aoi.PMBC.OWN.union$areakm2 <- st_area(aoi.PMBC.OWN.union)* 1e-6
aoi.PMBC.OWN.union <- drop_units(aoi.PMBC.OWN.union)


aoi.PMBC %>% count(REGIONAL_D) %>% st_drop_geometry()
ggplot()+
  geom_sf(data=aoi) +
  geom_sf(data=aoi.PMBC, aes(col=REGIONAL_D))

PMBC.parcels.area <- aoi.PMBC.OWN.union%>% st_drop_geometry()
PMBC.parcels.area$percSA <- PMBC.parcels.area$areakm2 /(st_area(aoi) * 1e-6) * 100  #  divide by study area
PMBC.parcels.area <- drop_units(PMBC.parcels.area)
write.csv(PMBC.parcels.area, "out/PMBC.parcels.area.csv")

# from looking at the map and the percentage of parcels by study area, it's clear that parcels overlap
# create flat layers for each owner type (i.e., dissolve)

ggplot()+
  geom_sf(data=aoi) +
  geom_sf(data=aoi.PMBC %>% filter(OWNER_TYPE=="Municipal"), col="red")+
  geom_sf(data=aoi.PMBC.muni2, col="blue")

# dissolve each ownership type into single multipolygon feature, without overlaps
# to union each ownership sub-type, resolving category overlaps
aoi.PMBC.FINAL.union <-aoi.PMBC %>% group_by(OWNER_TYPE) %>%
  summarise(across(geometry, ~ st_union(.)), .groups = "keep") %>%
  summarise(across(geometry, ~ st_combine(.)))
# calculate area per ownership type
aoi.PMBC.FINAL.union$areakm2 <- st_area(aoi.PMBC.FINAL.union)* 1e-6
aoi.PMBC.FINAL.union$percSA <- aoi.PMBC.FINAL.union$areakm2 /(st_area(aoi) * 1e-6) * 100  #  divide by study area
aoi.PMBC.FINAL.union <- drop_units(aoi.PMBC.FINAL.union)
aoi.PMBC.FINAL.union %>% st_drop_geometry()

# OWNER_TYPE       areakm2  percSA
# 1 Crown Agency       37.7   0.437
# 2 Crown Provincial  126.    1.46
# 3 Federal           154.    1.79
# 4 First Nations       6.27  0.0727
# 5 Mixed Ownership    13.5   0.156
# 6 Municipal         340.    3.94
# 7 None              362.    4.19
# 8 Private          1913.   22.2
# 9 Unknown            96.8   1.12

# check to see the overlap between PMBC layers
st_area(st_intersection(aoi.PMBC.FINAL.union %>% filter(OWNER_TYPE=="Private"),
                        aoi.PMBC.FINAL.union %>% filter(OWNER_TYPE=="Municipal")))* 1e-6 # 0.8 km2 overlap

st_area(st_intersection(aoi.PMBC.FINAL.union %>% filter(OWNER_TYPE=="Private"),
                        aoi.PMBC.FINAL.union %>% filter(OWNER_TYPE=="None")))* 1e-6 # 10 km2 overlap

st_area(st_intersection(aoi.PMBC.FINAL.union %>% filter(OWNER_TYPE=="Private"),
                        aoi.PMBC.FINAL.union %>% filter(OWNER_TYPE=="Unknown")))* 1e-6 # 75 km2 overlap

ggplot()+
  geom_sf(data=aoi) +
  geom_sf(data=aoi.PMBC.FINAL.union, aes(fill=OWNER_TYPE), lwd=0)+
  scale_fill_manual(values = (pnw_palette("Starfish",9)))


# RECOMMEND USING MERGED PRIVATE AND UNKNOWN OWNER_TYPE st_union POLYGON AS PRIVATE

###--- looking for overlaps between FCO and PMBC at municipal level
# note that these Municipal layers contain Park sub-class
aoi.FCO.muni.union <- aoi.FCO %>% filter(OWN_Grp==8) %>% st_union() # Crown - Municipal

st_area(aoi.FCO.muni.union) - st_area(aoi.PMBC.muni.union) # PMBC larger area

PCMB.FCO.diff.muni <- st_difference(aoi.PMBC.muni.union, aoi.FCO.muni.union)
st_area(PCMB.FCO.diff.muni) * 1e-6 # PMBC Municipal covers 74 km2 additional area
FCO.PCMB.diff.muni <- st_difference(aoi.FCO.muni.union, aoi.PMBC.muni.union)
st_area(FCO.PCMB.diff.muni) * 1e-6 # PMBC Municipal covers 53 km2 additional area

ggplot()+
  geom_sf(data=PCMB.FCO.diff.muni, col="blue")+
  geom_sf(data=FCO.PCMB.diff.muni, col="red")

# RECOMMEND USING PCMB Municipal as Municipal layer

# remove large files for housekeeping
rm(aoi.PMBC)
rm(aoi.FCO)

###--- now merge FCO and PCMB layers to create one cost layer
names(aoi.FCO.Final.union)
names(aoi.PMBC.FINAL.union)
colnames(aoi.FCO.Final.union)[1] <- "Ownership"
colnames(aoi.PMBC.FINAL.union)[1] <- "Ownership"

rbind(aoi.FCO.Final.union, aoi.PMBC.FINAL.union)


##############################################################################################
###--- now start looking into "lands that contribute to conservation" layers
# thought is to include each of the different types of conservation protections and then stack the rasters
# this may give an idea of the "strength" of protection (i.e., >1 mechanism = stronger protection)


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

save.image("data/spatialfiles_for_SCP_cost.RData")
# load("data/spatialfiles_for_SCP_cost.RData")
