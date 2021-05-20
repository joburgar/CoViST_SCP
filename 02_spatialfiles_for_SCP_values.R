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
                      "OpenStreetMap", "ggmap", "nngeo", "raster",  "readxl", "fasterize")
# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
#####################################################################################

SBDir <- "//spatialfiles.bcgov/work/srm/sry/Local/projlib/StewBase/Small_Home_Range_SAR/Analysis/Input/Focal_areas"
GISDir <- "//spatialfiles.bcgov/work/wlap/sry/Workarea/jburgar/CoViST"

###---
# read in species list data
sp.list <- read_excel("data/species_list_May-05-21.xlsx",sheet = 3, trim_ws = TRUE, col_types = c("text")) %>% type.convert()

# read in Focal Area polygons
FA.files <- list.files(path=SBDir)
FA1_CE <- st_read(dsn=paste(SBDir,"/",FA.files[1], sep=""), layer=FA.files[1]) %>% dplyr::select(Name) %>% st_transform(crs=3005)
FA1_CW <- st_read(dsn=paste(SBDir,"/",FA.files[2], sep=""), layer=FA.files[2]) %>% dplyr::select(Name) %>% st_transform(crs=3005)
FA1_RL <- st_read(dsn=paste(SBDir,"/",FA.files[3], sep=""), layer=FA.files[3]) %>% dplyr::select(Name) %>% st_transform(crs=3005)
FA1_SU <- st_read(dsn=paste(SBDir,"/",FA.files[4], sep=""), layer=FA.files[4]) %>% dplyr::select(Name) %>% st_transform(crs=3005)
FA1_VE <- st_read(dsn=paste(SBDir,"/",FA.files[5], sep=""), layer=FA.files[5]) %>% dplyr::select(Name) %>% st_transform(crs=3005)
FA2_CH <- st_read(dsn=paste(SBDir,"/",FA.files[6], sep=""), layer=FA.files[6]) %>% dplyr::select(Name) %>% st_transform(crs=3005)
FA3_AG <- st_read(dsn=paste(SBDir,"/",FA.files[7], sep=""), layer=FA.files[7]) %>% dplyr::select(Name) %>% st_transform(crs=3005)
FA4_NI <- st_read(dsn=paste(SBDir,"/",FA.files[8], sep=""), layer=FA.files[8]) %>% dplyr::select(Name) %>% st_transform(crs=3005)
FA5_LS <- st_read(dsn=paste(SBDir,"/",FA.files[9], sep=""), layer=FA.files[9]) %>% dplyr::select(Name) %>% st_transform(crs=3005)
FA6_AL <- st_read(dsn=paste(SBDir,"/",FA.files[10], sep=""), layer=FA.files[10]) %>% dplyr::select(Name) %>% st_transform(crs=3005)
FA7_PA <- st_read(dsn=paste(SBDir,"/",FA.files[11], sep=""), layer=FA.files[11]) %>% dplyr::select(Name) %>% st_transform(crs=3005)
FA8_BL <- st_read(dsn=paste(SBDir,"/",FA.files[12], sep=""), layer=FA.files[12]) %>% dplyr::select(Name) %>% st_transform(crs=3005)

FA_all <- rbind(FA1_CE, FA1_CW, FA1_RL, FA1_SU, FA1_VE, FA2_CH, FA3_AG, FA4_NI, FA5_LS, FA6_AL, FA7_PA, FA8_BL)
# FA_all %>% st_transform(crs=26910) %>% st_area()/1000000
# [1] 273.201320 205.137750 115.794737  98.822113  89.053201  35.848892 107.813251 112.969786  29.449039   6.543545  82.617137
# [12]  24.562055
# for Focal Areas <80 km2 secure species cannot be named / identified


aoi <- st_read(dsn=GISDir, layer="NewStudyBoundary_STSA")
aoi <- st_transform(aoi, crs=3005) # change projection to Albers


ggplot()+
  geom_sf(data=aoi, lwd=2)+
  geom_sf(data=FA_all, aes(fill=Name))

###--- read in secure species data
# list.files(path=GISDir)
CH_secure <- st_read(dsn=GISDir, layer = "CRITICAL_HABITAT_SECURED_12May2021")
EO_secure <- st_read(dsn=GISDir, layer = "OCCR_SENS_AREA_SVW_12April2021")

# add ELCODE to CH layer (already in EO_secure)
CH_secure$ELCODE <- sp.list$ELCODE[match(CH_secure$SCIENTIFIC, sp.list$Species_Scientific)]

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

aoi.CH_public$ELCODE <- sp.list$ELCODE[match(aoi.CH_public$SCIENTIFIC_NAME, sp.list$Species_Scientific)]
aoi.CH_public %>% filter(is.na(ELCODE)) %>% count(COMMON_NAME_ENGLISH)

sp.list %>% filter(grepl("Dun Skipper", Species_Common)) %>% dplyr::select(Species_Common, ELCODE) # IILEP77100
sp.list %>% filter(grepl("Bugbane", Species_Common)) %>% dplyr::select(Species_Common, ELCODE) # PDRAN0T012
sp.list %>% filter(grepl("Painted Turtle", Species_Common)) %>% dplyr::select(Species_Common, ELCODE) # ARAAD01015

aoi.CH_public$ELCODE <- case_when(grepl("Dun Skipper", aoi.CH_public$COMMON_NAME_ENGLISH) ~ "IILEP77100",
                                  grepl("Bugbane", aoi.CH_public$COMMON_NAME_ENGLISH) ~ "PDRAN0T012",
                                  grepl("Painted Turtle", aoi.CH_public$COMMON_NAME_ENGLISH) ~ "ARAAD01015",
                                  TRUE ~ as.character(aoi.CH_public$ELCODE))

ggplot()+
  geom_sf(data = CH_secure, col="red") +
  geom_sf(data = aoi.CH_public, col="blue")

# Element Occurrences
# filter by species on the species list
sp.to.use <- as.data.frame(sp.list %>% filter(species_sci_name!="Ursus arctos") %>% count(species_sci_name) %>% dplyr::select(species_sci_name))
sp.to.use <- sp.to.use[!is.na(sp.to.use$species_sci_name),]

bcdc_search("wsi", res_format = "wms")
# 4: Wildlife Species Inventory Telemetry Observations - Non-sensitive (other, wms, kml)
# ID: 6d48657f-ab33-43c5-ad40-09bd56140845
# Name: wildlife-species-inventory-telemetry-observations-non-sensitive
# 8: Wildlife Species Inventory Survey Observations - Non-sensitive (other, wms, kml)
# ID: 8f45a611-ce07-4e9f-a4b5-27e123972816
# Name: wildlife-species-inventory-survey-observations-non-sensitive

# wsi telemetry data
EO_telem_public <- bcdc_query_geodata("6d48657f-ab33-43c5-ad40-09bd56140845") %>%
  filter(SCIENTIFIC_NAME %in% sp.to.use) %>%
  collect()
aoi.EO_telem_public <- EO_telem_public %>% st_intersection(aoi)

aoi.EO_telem_public$ELCODE <- sp.list$ELCODE[match(aoi.EO_telem_public$SCIENTIFIC_NAME, sp.list$Species_Scientific)]

# wsi survey data
# EO_surv_public <- bcdc_query_geodata("8f45a611-ce07-4e9f-a4b5-27e123972816") %>%
#   filter(SCIENTIFIC_NAME %in% sp.to.use) %>%
#   collect()
# aoi.EO_surv_public <- EO_surv_public %>% st_intersection(aoi)

fname="data/aoi.EO_surv_public.rds"
# write_rds(aoi.EO_surv_public, fname)
aoi.EO_surv_public <- readRDS(fname)
# aoi.EO_surv_public <- aoi.EO_surv_public %>% filter(SCIENTIFIC_NAME!="Ursus arctos")

aoi.EO_surv_public$ELCODE <- sp.list$ELCODE[match(aoi.EO_surv_public$SCIENTIFIC_NAME, sp.list$Species_Scientific)]
# as.data.frame(aoi.EO_surv_public %>% count(ELCODE) %>% st_drop_geometry())
aoi.EO_surv_public %>% filter(is.na(ELCODE)) %>% count(SPECIES_ENGLISH_NAME) # great blue heron and southern
# need to provide ELCODE for Great Blue Heron and Southern Red-Backed Vole

aoi.EO_surv_public$ELCODE <- case_when(grepl("Great Blue Heron", aoi.EO_surv_public$SPECIES_ENGLISH_NAME) ~ "ABNGA04011",
                                       grepl("Southern Red-backed Vole", aoi.EO_surv_public$SPECIES_ENGLISH_NAME) ~ "AMAFF0902B",
                                       TRUE ~ as.character(aoi.EO_surv_public$ELCODE))
# now all species have ELCODE value

as.data.frame(aoi.EO_surv_public %>% group_by(ELCODE) %>% count(SPECIES_ENGLISH_NAME) %>% st_drop_geometry())


######
# CDC BIOTICS Occurence Attributes - Non Sensitive
bcdc_search("CDC BIOTICS", res_format = "wms")
# 1: Species and Ecosystems at Risk - Publicly Available Occurrences - CDC (other, wms, kml)
# ID: 0e035e55-f257-458f-9a96-80c01c69d389
# Name: species-and-ecosystems-at-risk-publicly-available-occurrences-cdc
sp.to.use2 <- as.data.frame(sp.list %>% count(ELCODE) %>% dplyr::select(ELCODE))
sp.to.use2 <- sp.to.use2[!is.na(sp.to.use2$ELCODE),]


EO_SAR_public <- bcdc_query_geodata("0e035e55-f257-458f-9a96-80c01c69d389") %>%
    filter(ELCODE %in% sp.to.use2) %>%
    collect()
aoi.EO_SAR_public <- EO_SAR_public %>% st_intersection(aoi)

as.data.frame(aoi.EO_SAR_public %>% group_by(ELCODE) %>% count(ENG_NAME) %>% st_drop_geometry())

aoi.EO_SAR_public %>% filter(grepl("Great Blue Heron", ENG_NAME)) %>% count(ELCODE) # ABNGA04011
aoi.EO_SAR_public %>% filter(grepl("Vole", ENG_NAME)) %>% count(ELCODE) # AMAFF0902B


###--- now all layers have ELCODE
# next steps are to
# (1) combine the non-CH layers,
# (2) buffer the points by 100 m and 250 m,
# (3) combine all spatial layers,
# (4) use loop / function to extract spatial data for each value and create value specific raster (at 100 m and 250 m pixel)

# (1) combine the 4 non-CH layers
names(aoi.EO_telem_public)
names(aoi.EO_surv_public)
names(aoi.EO_SAR_public)
names(EO_secure)

aoi.EO_telem_public$Source <- "wsi.telem"
aoi.EO_surv_public$Source <- "wsi.survey"
aoi.EO_SAR_public$Source <- "cdc.biotic"
EO_secure$Source <- "cdc.secure"

aoi.EO <- rbind(aoi.EO_telem_public %>% dplyr::select(ELCODE, Source) %>% st_zm(drop = TRUE, what = "ZM"),
                       aoi.EO_surv_public %>%  dplyr::select(ELCODE, Source) %>% st_zm(drop = TRUE, what = "ZM"),
                       aoi.EO_SAR_public %>%  dplyr::select(ELCODE, Source) %>% st_zm(drop = TRUE, what = "ZM"),
                       EO_secure %>% dplyr::select(ELCODE, Source) %>% st_zm(drop = TRUE, what = "ZM"))

ggplot()+
  geom_sf(data = aoi, lwd=2)+
  geom_sf(data = aoi.EO, aes(fill=Source, col=Source))+
  theme(legend.position = "bottom")

# (2) buffer the points by 100 m and 250 m
# DECISION to BUFFER ELEMENT OCCURRENCES BY 100 M and 250 M (BASED ON CH/IWMS RECOVERY STRATEGIES TO FIT WITH MOST SPECIES)
# Decision made by Joanna Burgar 12-May-2021; then revisited it 18-May-2021 to complete sensitivity analysis at 100 and 250 m

aoi.EO.utm <-  st_transform(aoi.EO, crs = 26910)
aoi.EO.100m <- st_buffer(aoi.EO.utm, dist = 100)
aoi.EO.250m <- st_buffer(aoi.EO.utm, dist = 250)


# (3) combine all spatial layers
CH_secure$Source <- "ch.secure"
aoi.CH_public$Source <- "ch.public"

aoi.CH <- rbind(CH_secure %>% dplyr::select(ELCODE, Source) %>% st_zm(drop = TRUE, what = "ZM"),
                aoi.CH_public %>% dplyr::select(ELCODE, Source) %>% st_zm(drop = TRUE, what = "ZM"))

# create utm CH layer for merging and creating raster files
aoi.CH.utm <- st_transform(aoi.CH, crs = 26910)

aoi.CH.EO.100m <- rbind(aoi.CH.utm, aoi.EO.100m)
aoi.CH.EO.250m <- rbind(aoi.CH.utm, aoi.EO.250m)

sp.map.250m <- ggplot()+
  geom_sf(data = aoi.CH.EO.250m, aes(fill=Source, col=Source))+
  geom_sf(data = FA_all %>% st_transform(crs=26910), colour = "black", lwd=1, fill = NA)+
  geom_sf(data = aoi %>% st_transform(crs=26910), fill=NA)+
  theme(legend.position = "bottom")

Cairo(file="out/species_values_mapped_250m_buffer.PNG", type="png", width=3000, height=2200,pointsize=15,bg="white",dpi=300)
sp.map.250m
dev.off()


sp.map.100m <- ggplot()+
  geom_sf(data = aoi.CH.EO.100m, aes(fill=Source, col=Source))+
  geom_sf(data = FA_all %>% st_transform(crs=26910), colour = "black", lwd=1, fill = NA)+
  geom_sf(data = aoi %>% st_transform(crs=26910), fill=NA)+
  theme(legend.position = "bottom")

Cairo(file="out/species_values_mapped_100m_buffer.PNG", type="png", width=3000, height=2200,pointsize=15,bg="white",dpi=300)
sp.map.100m
dev.off()

###--- aggregated summary of data points/polygons for each ELCODE / value, and the data sources
aoi.CH.EO.100m$Species_Common <- sp.list$Species_Common[match(aoi.CH.EO.100m$ELCODE, sp.list$ELCODE)]
aoi.CH.EO.100m$Species_Scientific <- sp.list$Species_Scientific[match(aoi.CH.EO.100m$ELCODE, sp.list$ELCODE)]
as.data.frame(aoi.CH.EO.100m %>% group_by(Species_Common) %>% count(ELCODE) %>% st_drop_geometry())

# check on Flammulated Owl - not erroneous, but in Region 8 so should drop out when boundaries confirmed
# aoi.EO_surv_public %>% filter(ELCODE=="ABNSB01020") %>% st_drop_geometry() %>% dplyr::select(REGION, PROJECT_WEB_PAGE)

write.csv(aoi.CH.EO.100m %>% group_by(Species_Common, Species_Scientific, ELCODE) %>% count(Source) %>% st_drop_geometry(),
          "data/SCP_values_spatial_sources.csv", row.names = FALSE)

# (3b) Determine species richness per focal area
# use ELCODE, but first need to group painted turtle, peregrine falcon, western screech-owl (have 2 ELCODE for species and sub-species)
aoi.CH.EO.100m %>% filter(grepl("Falcon|Screech|Turtle", Species_Common)) %>% group_by(Species_Common) %>% count(ELCODE) %>% st_drop_geometry()

aoi.CH.EO.100m$ELCODE <- case_when(grepl("Turtle", aoi.CH.EO.100m$Species_Common) ~ "ARAAD01015",
                                   grepl("Falcon", aoi.CH.EO.100m$Species_Common) ~ "ABNKD06071",
                                   grepl("Screech", aoi.CH.EO.100m$Species_Common) ~ "ABNSB01042",
                                   TRUE ~ as.character(aoi.CH.EO.100m$ELCODE))

# bit hacky but replicating the steps here to keep spatial objects consistent
aoi.CH.EO.250m$Species_Common <- sp.list$Species_Common[match(aoi.CH.EO.250m$ELCODE, sp.list$ELCODE)]
aoi.CH.EO.250m$Species_Scientific <- sp.list$Species_Scientific[match(aoi.CH.EO.250m$ELCODE, sp.list$ELCODE)]
aoi.CH.EO.250m$ELCODE <- case_when(grepl("Turtle", aoi.CH.EO.250m$Species_Common) ~ "ARAAD01015",
                                   grepl("Falcon", aoi.CH.EO.250m$Species_Common) ~ "ABNKD06071",
                                   grepl("Screech", aoi.CH.EO.250m$Species_Common) ~ "ABNSB01042",
                                   TRUE ~ as.character(aoi.CH.EO.250m$ELCODE))

unique(aoi.CH.EO.100m$ELCODE); unique(aoi.CH.EO.250m$ELCODE) # now only 105 ELCODE values

ggplot()+
  geom_sf(data=FA_all)

# using 100  m buffer
FA_value.dist <- st_nn(aoi.CH.EO.100m, FA_all %>% st_transform(crs=26910), k=1, returnDist = T)
aoi.CH.EO.100m$Value_dist <- unlist(FA_value.dist$dist)
aoi.CH.EO.100m$Value_type <- unlist(FA_value.dist$nn)
aoi.CH.EO.100m$Value_type <- FA_all$Name[match(aoi.CH.EO.100m$Value_type,rownames(FA_all))]

FA_sp.richness.100m <- aoi.CH.EO.100m %>% filter(Value_dist==0) %>% group_by(Value_type) %>% count(ELCODE) %>% st_drop_geometry()
FA_sp.richness.100m %>% count(Value_type)
FA_sp.richness.100m$Species_Common <- sp.list$Species_Common[match(FA_sp.richness.100m$ELCODE, sp.list$ELCODE)]
write.csv(FA_sp.richness.100m, "data/FA_sprich_100m.csv", row.names = FALSE)
st_write(aoi.CH.EO.100m,  paste0(getwd(),"/out/aoi.CH.EO.100m.shp"), delete_layer = TRUE)

# unique(FA_sp.richness.100m$Species_Common)

# using 250 m buffer
FA_value.dist.250 <- st_nn(aoi.CH.EO.250m, FA_all %>% st_transform(crs=26910), k=1, returnDist = T)
aoi.CH.EO.250m$Value_dist <- unlist(FA_value.dist.250$dist)
aoi.CH.EO.250m$Value_type <- unlist(FA_value.dist.250$nn)
aoi.CH.EO.250m$Value_type <- FA_all$Name[match(aoi.CH.EO.250m$Value_type,rownames(FA_all))]

FA_sp.richness.250m <- aoi.CH.EO.250m %>% filter(Value_dist==0) %>% group_by(Value_type) %>% count(ELCODE) %>% st_drop_geometry()
FA_sp.richness.250m %>% count(Value_type)
FA_sp.richness.250m$Species_Common <- sp.list$Species_Common[match(FA_sp.richness.250m$ELCODE, sp.list$ELCODE)]
write.csv(FA_sp.richness.250m, "data/FA_sprich_250m.csv", row.names = FALSE)
st_write(aoi.CH.EO.250m,  paste0(getwd(),"/out/aoi.CH.EO.250m.shp"), delete_layer = TRUE)

# (4) use loop / function to extract spatial data for each value and create value specific raster (at 100 m and 250 m pixel)
# create individual raster layers for each ELCODE and then bind into raster stack and raster brick
# do for 100 m and 250 m polygon layers at 100 m and 250 m pixel sizes
ELCODE.to.use <- unique(aoi.CH.EO.100m$ELCODE)
r100m <- raster(ext=extent(aoi.CH.EO.100m), crs=26910, res=c(100,100))
r250m <- raster(ext=extent(aoi.CH.EO.100m), crs=26910, res=c(250,250))

# for 100 m pixel & 100 m buffer
r100m_list=list()
for(i in 1:length(ELCODE.to.use)){
  poly_cast <- st_cast(aoi.CH.EO.100m %>% filter(aoi.CH.EO.100m$ELCODE==ELCODE.to.use[i]))
  r100m_list[[i]] <- fasterize(poly_cast, r100m)
}

r100m_stack = stack(r100m_list)
r100mbuff_100mpix_stack <- stackApply(r100m_stack, indices=1, fun=sum)
writeRaster(r100mbuff_100mpix_stack, file="out/r100mbuff_100mpix_species_stack.tif", bylayer=TRUE)

Cairo(file="out/r100mbuff_100mpix_species_stack.PNG", type="png", width=3000, height=2200,pointsize=15,bg="white",dpi=300)
plot(r100mbuff_100mpix_stack)
dev.off()


# for 100 m pixel & 250 m buffer
r100m_list=list()
for(i in 1:length(ELCODE.to.use)){
  poly_cast <- st_cast(aoi.CH.EO.250m %>% filter(aoi.CH.EO.250m$ELCODE==ELCODE.to.use[i]))
  r100m_list[[i]] <- fasterize(poly_cast, r100m)
}

r100m_stack = stack(r100m_list)
r250mbuff_100mpix_stack <- stackApply(r100m_stack, indices=1, fun=sum)
writeRaster(r250mbuff_100mpix_stack, file="out/r250mbuff_100mpix_species_stack.tif", bylayer=TRUE)

Cairo(file="out/r250mbuff_100mpix_species_stack.PNG", type="png", width=3000, height=2200,pointsize=15,bg="white",dpi=300)
plot(r250mbuff_100mpix_stack)
dev.off()

# for 250 m pixel & 100 m buffer
r250m_list=list()
for(i in 1:length(ELCODE.to.use)){
  poly_cast <- st_cast(aoi.CH.EO.100m %>% filter(aoi.CH.EO.100m$ELCODE==ELCODE.to.use[i]))
  r250m_list[[i]] <- fasterize(poly_cast, r250m)
}

r250m_stack = stack(r250m_list)
r100mbuff_250mpix_stack <- stackApply(r250m_stack, indices=1, fun=sum)
writeRaster(r100mbuff_250mpix_stack, file="out/r100mbuff_250mpix_species_stack.tif", bylayer=TRUE)

Cairo(file="out/r100mbuff_250mpix_species_stack.PNG", type="png", width=3000, height=2200,pointsize=15,bg="white",dpi=300)
plot(r100mbuff_250mpix_stack)
dev.off()

# for 250 m pixel & 250 m buffer
r250m_list=list()
for(i in 1:length(ELCODE.to.use)){
  poly_cast <- st_cast(aoi.CH.EO.250m %>% filter(aoi.CH.EO.250m$ELCODE==ELCODE.to.use[i]))
  r250m_list[[i]] <- fasterize(poly_cast, r250m)
}

r250m_stack = stack(r250m_list)
r250mbuff_250mpix_stack <- stackApply(r250m_stack, indices=1, fun=sum)
writeRaster(r250mbuff_250mpix_stack, file="out/r250mbuff_250mpix_species_stack.tif", bylayer=TRUE)

Cairo(file="out/r250mbuff_250mpix_species_stack.PNG", type="png", width=3000, height=2200,pointsize=15,bg="white",dpi=300)
plot(r250mbuff_250mpix_stack)
dev.off()

# (4) create overlap / heatmap of element occurrences
# (5) review overlap with focal areas (as a test for how widgets should look and to make sure we're not missing a key area)


save.image("data/spatialfiles_for_SCP_values.RData")
