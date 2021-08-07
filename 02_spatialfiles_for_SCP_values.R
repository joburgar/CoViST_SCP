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
                      "OpenStreetMap", "ggmap", "nngeo", "raster",  "readxl", "fasterize", "PNWColors", "taxize")
# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
#####################################################################################

SBDir <- "//spatialfiles.bcgov/work/srm/sry/Local/projlib/StewBase/Small_Home_Range_SAR/Analysis/Input/Focal_areas"
GISDir <- "//spatialfiles.bcgov/work/wlap/sry/Workarea/jburgar/CoViST"

#####################################################################################
###--- Species & Ecosystem Values
#####################################################################################

###---
# read in taxonomy groupings
taxon <- read.csv("data/sp_taxon.csv", row.names = 1)

# read in species list data
# sp.list <- read_excel("data/species_list_May-05-21.xlsx",sheet = 3, trim_ws = TRUE, col_types = c("text")) %>% type.convert()
sp.list <- read_excel("data/species_list_Jul-06-21.xlsx",sheet = 1, trim_ws = TRUE, col_types = c("text")) %>% type.convert() # updated to 101 included species

# read in Focal Area polygons
# FA.files <- list.files(path=SBDir)
# FA1_CE <- st_read(dsn=paste(SBDir,"/",FA.files[grepl("ChilliwackEast", FA.files)], sep=""),
#                   layer=FA.files[grepl("ChilliwackEast", FA.files)]) %>% dplyr::select(Name) %>% st_transform(crs=3005)
#
# FA1_CW <- st_read(dsn=paste(SBDir,"/",FA.files[grepl("ChilliwackWest", FA.files)], sep=""),
#                   layer=FA.files[grepl("ChilliwackWest", FA.files)]) %>% dplyr::select(Name) %>% st_transform(crs=3005)
#
# FA1_RL <- st_read(dsn=paste(SBDir,"/",FA.files[grepl("Ryder", FA.files)], sep=""),
#                   layer=FA.files[grepl("Ryder", FA.files)]) %>% dplyr::select(Name) %>% st_transform(crs=3005)
#
# FA1_SU <- st_read(dsn=paste(SBDir,"/",FA.files[grepl("Sumas", FA.files)], sep=""),
#                   layer=FA.files[grepl("Sumas", FA.files)]) %>% dplyr::select(Name) %>% st_transform(crs=3005)
#
# FA1_VE <- st_read(dsn=paste(SBDir,"/",FA.files[grepl("^((?!.*zip).)*Vedder.*$", FA.files, per=TRUE)], sep=""),
#                   layer=FA.files[grepl("^((?!.*zip).)*Vedder.*$", FA.files, per=TRUE)]) %>% dplyr::select(Name) %>% st_transform(crs=3005)
#
# FA2_CH <- st_read(dsn=paste(SBDir,"/",FA.files[grepl("Cheam", FA.files)], sep=""),
#                   layer=FA.files[grepl("Cheam", FA.files)]) %>% dplyr::select(Name) %>% st_transform(crs=3005)
#
# FA3_AG <- st_read(dsn=paste(SBDir,"/",FA.files[grepl("Agassiz", FA.files)], sep=""),
#                   layer=FA.files[grepl("Agassiz", FA.files)]) %>% dplyr::select(Name) %>% st_transform(crs=3005)
#
# FA4_NI <- st_read(dsn=paste(SBDir,"/",FA.files[grepl("Nicomen", FA.files)], sep=""),
#                   layer=FA.files[grepl("Nicomen", FA.files)]) %>% dplyr::select(Name) %>% st_transform(crs=3005)
#
# FA5_LS <- st_read(dsn=paste(SBDir,"/",FA.files[grepl("LowerStave", FA.files)], sep=""),
#                   layer=FA.files[grepl("LowerStave", FA.files)]) %>% dplyr::select(Name) %>% st_transform(crs=3005)
#
# FA6_AL <- st_read(dsn=paste(SBDir,"/",FA.files[grepl("Alouette", FA.files)], sep=""),
#                   layer=FA.files[grepl("Alouette", FA.files)]) %>% dplyr::select(Name) %>% st_transform(crs=3005)
#
# FA7_PA <- st_read(dsn=paste(SBDir,"/",FA.files[grepl("PittAddington", FA.files)], sep=""),
#                   layer=FA.files[grepl("PittAddington", FA.files)]) %>% dplyr::select(Name) %>% st_transform(crs=3005)
#
# FA8_BL <- st_read(dsn=paste(SBDir,"/",FA.files[grepl("Burnaby", FA.files)], sep=""),
#                   layer=FA.files[grepl("Burnaby", FA.files)]) %>% dplyr::select(Name) %>% st_transform(crs=3005)
#
# FA_all <- rbind(FA1_CE, FA1_CW, FA1_RL, FA1_SU, FA1_VE, FA2_CH, FA3_AG, FA4_NI, FA5_LS, FA6_AL, FA7_PA, FA8_BL)
# colnames(FA_all)[1] <- "Focal_Area"
# FA_all$Focal_Group <- c(1,1,1,1,1,2,3,4,5,6,7,8)

# FA_all %>% st_transform(crs=26910) %>% st_area()/1000000
# [1] 273.201320 205.137750 115.794737  98.822113  89.053201  35.848892 107.813251 112.969786  29.449039   6.543545  82.617137
# [12]  24.562055
# for Focal Areas <80 km2 secure species cannot be named / identified

# once FA polygon file has been created, can load previously created file
FA_all <- st_read(dsn=paste(getwd(),"/out", sep=""), layer="Focal_Area")

aoi <- st_read(dsn=GISDir, layer="CoViST_Study_Boundary_July2021") # updated to reflect decision at June 22 STSA meeting
aoi <- st_transform(aoi, crs=3005) # change projection to Albers

ggplot()+
  geom_sf(data=aoi)+
  geom_sf(data=FA_all, aes(fill=Focl_Ar))

###--- read in secure species data
# filter by species on the species list
# Decision to exclude: MAMU, NOGO, SPOW, FAOW, and grizzly bear on June 29, 2021 meeting
as.data.frame(sp.list %>% count(Species_Common, ELCODE))
sp.to.remove <- c("Flammulated Owl", "Grizzly Bear", "Marbled Murrelet", "Northern Goshawk Laingi Subspecies", "Spotted Owl")
sp.to.use_SCI <- as.data.frame(sp.list %>% filter(!Species_Common %in% sp.to.remove) %>% count(Species_Scientific) %>% dplyr::select(Species_Scientific))
sp.to.use_SCI <- sp.to.use_SCI$Species_Scientific

sp.to.use_ELCODE <- as.data.frame(sp.list %>% filter(!Species_Common %in% sp.to.remove) %>% count(ELCODE) %>% dplyr::select(ELCODE))
sp.to.use_ELCODE <- sp.to.use_ELCODE$ELCODE

# list.files(path=GISDir)
CH_secure <- st_read(dsn=GISDir, layer = "CRITICAL_HABITAT_SECURED_12May2021")
CH_secure <- CH_secure %>% filter(SCIENTIFIC %in% sp.to.use_SCI)

EO_secure <- st_read(dsn=GISDir, layer = "OCCR_SENS_AREA_SVW_12April2021")
EO_secure <- EO_secure %>% filter(SCI_NAME %in% sp.to.use_SCI)

# add ELCODE to CH layer (already in EO_secure)
CH_secure$ELCODE <- sp.list$ELCODE[match(CH_secure$SCIENTIFIC, sp.list$Species_Scientific)]


###--- read in publicly available data
# Critical Habitat
# bcdc_search("critical habitat", res_format = "wms")
# 1: Critical Habitat for federally-listed species at risk (posted) (other, wms, kml)
# ID: 076b8c98-a3f1-429b-9dae-03faed0c6aef
# Name: critical-habitat-for-federally-listed-species-at-risk-posted-

CH_public <- bcdc_query_geodata("076b8c98-a3f1-429b-9dae-03faed0c6aef") %>%
  filter(BBOX(st_bbox(aoi))) %>% # works with BBOX but not INTERSECT
  collect()
aoi.CH_public <- CH_public %>% st_intersection(aoi)

# fname="data/aoi.CH_public.rds"
# write_rds(aoi.CH_public, fname)
# aoi.CH_public <- readRDS(fname)

aoi.CH_public$ELCODE <- sp.list$ELCODE[match(aoi.CH_public$SCIENTIFIC_NAME, sp.list$Species_Scientific)]

sp.list %>% filter(grepl("Dun Skipper", Species_Common)) %>% dplyr::select(Species_Common, ELCODE) # IILEP77100
sp.list %>% filter(grepl("Bugbane", Species_Common)) %>% dplyr::select(Species_Common, ELCODE) # PDRAN0T012
sp.list %>% filter(grepl("Painted Turtle", Species_Common)) %>% dplyr::select(Species_Common, ELCODE) # ARAAD01015

aoi.CH_public$ELCODE <- case_when(grepl("Dun Skipper", aoi.CH_public$COMMON_NAME_ENGLISH) ~ "IILEP77100",
                                  grepl("Bugbane", aoi.CH_public$COMMON_NAME_ENGLISH) ~ "PDRAN0T012",
                                  grepl("Painted Turtle", aoi.CH_public$COMMON_NAME_ENGLISH) ~ "ARAAD01015",
                                  TRUE ~ as.character(aoi.CH_public$ELCODE))

aoi.CH_public %>% filter(is.na(ELCODE)) %>% count(COMMON_NAME_ENGLISH)
aoi.CH_public %>% filter(!is.na(ELCODE)) %>% count(COMMON_NAME_ENGLISH, ELCODE) %>% st_drop_geometry()

aoi.CH_public <- aoi.CH_public %>% filter(ELCODE %in% sp.to.use_ELCODE)
unique(aoi.CH_public$COMMON_NAME_ENGLISH)

ggplot()+
  geom_sf(data = CH_secure, col="red") +
  geom_sf(data = aoi.CH_public, col="blue")

# Element Occurrences

bcdc_search("wsi", res_format = "wms")
# 4: Wildlife Species Inventory Telemetry Observations - Non-sensitive (other, wms, kml)
# ID: 6d48657f-ab33-43c5-ad40-09bd56140845
# Name: wildlife-species-inventory-telemetry-observations-non-sensitive
# 8: Wildlife Species Inventory Survey Observations - Non-sensitive (other, wms, kml)
# ID: 8f45a611-ce07-4e9f-a4b5-27e123972816
# Name: wildlife-species-inventory-survey-observations-non-sensitive

# wsi telemetry data
EO_telem_public <- bcdc_query_geodata("6d48657f-ab33-43c5-ad40-09bd56140845") %>%
  filter(SCIENTIFIC_NAME %in% sp.to.use_SCI) %>%
  collect()
aoi.EO_telem_public <- EO_telem_public %>% st_intersection(aoi)

aoi.EO_telem_public$ELCODE <- sp.list$ELCODE[match(aoi.EO_telem_public$SCIENTIFIC_NAME, sp.list$Species_Scientific)]

# wsi survey data
EO_surv_public <- bcdc_query_geodata("8f45a611-ce07-4e9f-a4b5-27e123972816") %>%
  filter(BBOX(st_bbox(aoi))) %>%
  collect()
aoi.EO_surv_public <- EO_surv_public %>% st_intersection(aoi)


aoi.EO_surv_public$ELCODE <- sp.list$ELCODE[match(aoi.EO_surv_public$SCIENTIFIC_NAME, sp.list$Species_Scientific)]
as.data.frame(aoi.EO_surv_public %>% count(SPECIES_ENGLISH_NAME, ELCODE) %>% st_drop_geometry())
aoi.EO_surv_public %>% filter(is.na(ELCODE)) %>% count(SPECIES_ENGLISH_NAME) # great blue heron and southern
# need to provide ELCODE for Great Blue Heron and Southern Red-Backed Vole

aoi.EO_surv_public$ELCODE <- case_when(grepl("Great Blue Heron", aoi.EO_surv_public$SPECIES_ENGLISH_NAME) ~ "ABNGA04011",
                                       grepl("Southern Red-backed Vole", aoi.EO_surv_public$SPECIES_ENGLISH_NAME) ~ "AMAFF0902B",
                                       TRUE ~ as.character(aoi.EO_surv_public$ELCODE))
aoi.EO_surv_public <- aoi.EO_surv_public %>% filter(ELCODE %in% sp.to.use_ELCODE)

# now all species have ELCODE value
as.data.frame(aoi.EO_surv_public %>% group_by(ELCODE) %>% count(SPECIES_ENGLISH_NAME) %>% st_drop_geometry())


######
# CDC BIOTICS Occurence Attributes - Non Sensitive
bcdc_search("CDC BIOTICS", res_format = "wms")
# 1: Species and Ecosystems at Risk - Publicly Available Occurrences - CDC (other, wms, kml)
# ID: 0e035e55-f257-458f-9a96-80c01c69d389
# Name: species-and-ecosystems-at-risk-publicly-available-occurrences-cdc


EO_SAR_public <- bcdc_query_geodata("0e035e55-f257-458f-9a96-80c01c69d389") %>%
  filter(BBOX(st_bbox(aoi))) %>%
  collect()
aoi.EO_SAR_public <- EO_SAR_public %>% st_intersection(aoi)

as.data.frame(aoi.EO_SAR_public %>% group_by(ELCODE) %>% count(ENG_NAME) %>% st_drop_geometry())
aoi.EO_SAR_public <- aoi.EO_SAR_public %>% filter(ELCODE %in% sp.to.use_ELCODE)

# aoi.EO_SAR_public %>% filter(grepl("Great Blue Heron", ENG_NAME)) %>% count(ELCODE) # ABNGA04011
# aoi.EO_SAR_public %>% filter(grepl("Vole", ENG_NAME)) %>% count(ELCODE) # AMAFF0902B
as.data.frame(aoi.EO_SAR_public %>% count(ENG_NAME))


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

# clip to new aoi
aoi.utm <-  st_transform(aoi, crs = 26910) # first transform utm to ensure same projections
aoi.CH.EO.100m <- aoi.CH.EO.100m  %>% st_intersection(aoi.utm)
aoi.CH.EO.250m <- aoi.CH.EO.250m  %>% st_intersection(aoi.utm)

sp.map.250m <- ggplot()+
  geom_sf(data = aoi.CH.EO.250m, aes(fill=Source, col=Source))+
  geom_sf(data = FA_all %>% st_transform(crs=26910), colour = "black", lwd=1, fill = NA)+
  geom_sf(data = aoi.utm, fill=NA)+
  theme(legend.position = "bottom")

Cairo(file="out/species_values_mapped_250m_buffer.PNG", type="png", width=3000, height=2200,pointsize=15,bg="white",dpi=300)
sp.map.250m
dev.off()

sp.map.100m <- ggplot()+
  geom_sf(data = aoi.CH.EO.100m, aes(fill=Source, col=Source))+
  geom_sf(data = FA_all %>% st_transform(crs=26910), colour = "black", lwd=1, fill = NA)+
  geom_sf(data = aoi.utm, fill=NA)+
  theme(legend.position = "bottom")

Cairo(file="out/species_values_mapped_100m_buffer.PNG", type="png", width=3000, height=2200,pointsize=15,bg="white",dpi=300)
sp.map.100m
dev.off()

###--- aggregated summary of data points/polygons for each ELCODE / value, and the data sources
aoi.CH.EO.100m$Species_Common <- sp.list$Species_Common[match(aoi.CH.EO.100m$ELCODE, sp.list$ELCODE)]
aoi.CH.EO.100m$Species_Scientific <- sp.list$Species_Scientific[match(aoi.CH.EO.100m$ELCODE, sp.list$ELCODE)]
as.data.frame(aoi.CH.EO.100m %>% group_by(Species_Common) %>% count(ELCODE) %>% st_drop_geometry()) # 98 species

write.csv(aoi.CH.EO.100m %>% group_by(Species_Common, Species_Scientific, ELCODE) %>% count(Source) %>% st_drop_geometry(),
          "data/SCP_values_spatial_sources.csv", row.names = FALSE)

# (3b) Determine species richness per focal area
# use ELCODE, but first need to group painted turtle, peregrine falcon, western screech-owl (have 2 ELCODE for species and sub-species)
aoi.CH.EO.100m %>% filter(grepl("Falcon|Screech|Turtle", Species_Common)) %>% group_by(Species_Common) %>% count(ELCODE) %>% st_drop_geometry()

# aoi.CH.EO.100m$ELCODE <- case_when(grepl("Turtle", aoi.CH.EO.100m$Species_Common) ~ "ARAAD01015",
#                                    grepl("Falcon", aoi.CH.EO.100m$Species_Common) ~ "ABNKD06071",
#                                    grepl("Screech", aoi.CH.EO.100m$Species_Common) ~ "ABNSB01042",
#                                    TRUE ~ as.character(aoi.CH.EO.100m$ELCODE))

# bit hacky but replicating the steps here to keep spatial objects consistent
aoi.CH.EO.250m$Species_Common <- sp.list$Species_Common[match(aoi.CH.EO.250m$ELCODE, sp.list$ELCODE)]
aoi.CH.EO.250m$Species_Scientific <- sp.list$Species_Scientific[match(aoi.CH.EO.250m$ELCODE, sp.list$ELCODE)]

aoi.CH.EO.250m %>% filter(grepl("Falcon|Screech|Turtle", Species_Common)) %>% group_by(Species_Common) %>% count(ELCODE) %>% st_drop_geometry()
# aoi.CH.EO.250m$ELCODE <- case_when(grepl("Turtle", aoi.CH.EO.250m$Species_Common) ~ "ARAAD01015",
#                                    grepl("Falcon", aoi.CH.EO.250m$Species_Common) ~ "ABNKD06071",
#                                    grepl("Screech", aoi.CH.EO.250m$Species_Common) ~ "ABNSB01042",
#                                    TRUE ~ as.character(aoi.CH.EO.250m$ELCODE))

unique(aoi.CH.EO.100m$ELCODE); unique(aoi.CH.EO.250m$ELCODE) # now only 101 ELCODE values

ggplot()+
  geom_sf(data=FA_all)

# using 100  m buffer
FA_value.dist <- st_nn(aoi.CH.EO.100m, FA_all %>% st_transform(crs=26910), k=1, returnDist = T)
aoi.CH.EO.100m$Value_dist <- unlist(FA_value.dist$dist)
aoi.CH.EO.100m$Focal_Area <- unlist(FA_value.dist$nn)
aoi.CH.EO.100m$Focal_Area <- FA_all$Focl_Ar[match(aoi.CH.EO.100m$Focal_Area,rownames(FA_all))]

FA_sp.richness.100m <- aoi.CH.EO.100m %>% filter(Value_dist==0) %>% group_by(Focal_Area) %>% count(ELCODE) %>% st_drop_geometry()
FA_sp.richness.100m %>% count(Focal_Area)
FA_sp.richness.100m$Species_Common <- sp.list$Species_Common[match(FA_sp.richness.100m$ELCODE, sp.list$ELCODE)]
FA_sp.richness.100m$Tax_Class <- taxon$Tax_Class[match(FA_sp.richness.100m$ELCODE, taxon$ELCODE)]
write.csv(FA_sp.richness.100m, "data/FA_sprich_100m.csv", row.names = FALSE)

st_write(aoi.CH.EO.100m,  paste0(getwd(),"/out/aoi.CH.EO.100m.shp"), delete_layer = TRUE)

# using 250 m buffer
FA_value.dist.250 <- st_nn(aoi.CH.EO.250m, FA_all %>% st_transform(crs=26910), k=1, returnDist = T)
aoi.CH.EO.250m$Value_dist <- unlist(FA_value.dist.250$dist)
aoi.CH.EO.250m$Focal_Area <- unlist(FA_value.dist.250$nn)
aoi.CH.EO.250m$Focal_Area <- FA_all$Focal_Area[match(aoi.CH.EO.250m$Focal_Area,rownames(FA_all))]

FA_sp.richness.250m <- aoi.CH.EO.250m %>% filter(Value_dist==0) %>% group_by(Focal_Area) %>% count(ELCODE) %>% st_drop_geometry()
FA_sp.richness.250m %>% count(Focal_Area)
FA_sp.richness.250m$Species_Common <- sp.list$Species_Common[match(FA_sp.richness.250m$ELCODE, sp.list$ELCODE)]
# unique(FA_sp.richness.250m$Species_Common); unique(FA_sp.richness.100m$Species_Common)
FA_sp.richness.250m$Tax_Class <- taxon$Tax_Class[match(FA_sp.richness.250m$ELCODE, taxon$ELCODE)]
write.csv(FA_sp.richness.250m, "data/FA_sprich_250m.csv", row.names = FALSE)
st_write(aoi.CH.EO.250m,  paste0(getwd(),"/out/aoi.CH.EO.250m.shp"), delete_layer = TRUE)


FA_Tax.Richness <- FA_sp.richness.250m %>% group_by(Focal_Area)%>% count(Tax_Class)
FA_Tax.Richness <- FA_Tax.Richness %>% pivot_wider(names_from = Tax_Class, values_from = n)
FA_Tax.Richness[is.na(FA_Tax.Richness)] <- 0
FA_Tax.Richness <- FA_Tax.Richness %>% rowwise() %>% mutate(Sp.Richness = sum(c_across(Amphibians:Fungi)))
FA_Sp.Richness <- left_join(FA_all, FA_Tax.Richness)
st_write(FA_Sp.Richness,  paste0(getwd(),"/out/Focal_Area.shp"), delete_layer = TRUE) # for the species richness widget


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

save.image("data/spatialfiles_for_SCP_values.RData")

#####################################################################################
###--- Indigenous Values
#####################################################################################

# fgdb = "/STUP/p12/srrmc_gis.gdb"
fgdb = "/STUP/p20/srrmc_gis.gdb"

# List all feature classes in a file geodatabase
st_layers(paste(GISDir,fgdb,sep=""))
# For STUP
# Available layers:
#                 layer_name             geometry_type features fields
# 1         ProtectWatershed             Multi Polygon       12      4
# 2             Sub_Alpine20 3D Measured Multi Polygon    15635      4
# 3             HeritagePoly             Multi Polygon        1      3
# 4                Sanctuary             Multi Polygon       27      4
# 5 CulturalLandscapeFeature             Multi Polygon       76      3
# 6    CulturallySensHabitat             Multi Polygon        7      3
# 7                SensWater             Multi Polygon       52      3
# 8       SolhTemexwUTM_250k             Multi Polygon        1      5 # outline boundary
# 9             SXTASOI_250k         Multi Line String        1      2 # not quite sure, possibly also admin/jurisdictional


STUP_layers <- list(STUP_Wtrshd, STUP_Hrtg, STUP_Snctry, STUP_CLF, STUP_CSH, STUP_SensWtr, STUP_SubAlp)
STUP_names <- c("ProtectWatershed", "HeritagePoly", "Sanctuary", "CulturalLandscapeFeature","CulturallySensHabitat", "SensWater", "Sub_Alpine20")

# load in the layers as a list of sf objects
# for the 06-Aug-2021 output, the p20 gdb was imported
for(i in 1:length(STUP_layers)){
  STUP_layers[[i]] <- st_read(dsn=paste(GISDir,fgdb,sep=""),layer=STUP_names[i]) %>%
    st_transform(crs=3005) %>% st_intersection(aoi) %>% st_zm(drop = TRUE, what = "ZM")
}

# reformat layers the same for merging into one sf object
for(i in 1:length(STUP_layers)){
  STUP_layers[[i]]$Source <- STUP_names[i]
  STUP_layers[[i]] <- rename(STUP_layers[[i]], ID = 1)
  STUP_layers[[i]]$ID <- as.character(STUP_layers[[i]]$ID)
  STUP_layers[[i]] <- STUP_layers[[i]] %>% dplyr::select("ID","Source")
  STUP_layers[[i]] <- rename(STUP_layers[[i]], Shape = 3)
}

# merge into one sf object
STUP_values <- do.call(rbind, STUP_layers)

STUP_values.map <- ggplot()+
  geom_sf(data = aoi)+
  geom_sf(data = STUP_values, aes(fill=Source), lwd=0)+
  scale_fill_manual(values = rev(pnw_palette("Bay",7)))+
  theme(legend.title = element_blank(), legend.position = "bottom")

Cairo(file="out/STUP_values.map.PNG", type="png", width=3000, height=2200,pointsize=15,bg="white",dpi=300)
STUP_values.map
dev.off()

STUP_values <- st_cast(STUP_values, "MULTIPOLYGON")

r100m <- raster(ext=extent(aoi), crs=26910, res=c(100,100))
r250m <- raster(ext=extent(aoi), crs=26910, res=c(250,250))

r100m_STUP_values <- fasterize(STUP_values, r100m, by="Source")
r100_STUP_values_stack <- stackApply(r100m_STUP_values, indices=1, fun=sum)
writeRaster(r100_STUP_values_stack, file="out/r100_STUP_values_stack.tif", bylayer=TRUE, overwrite=TRUE)

r250m_STUP_values <- fasterize(STUP_values, r250m, by="Source")
r250_STUP_values_stack <- stackApply(r250m_STUP_values, indices=1, fun=sum)
writeRaster(r250_STUP_values_stack, file="out/r250_STUP_values_stack.tif", bylayer=TRUE, overwrite=TRUE)

Cairo(file="out/r100_STUP_values_stack.PNG", type="png", width=3000, height=2200,pointsize=15,bg="white",dpi=300)
plot(r100_STUP_values_stack)
dev.off()

Cairo(file="out/r250_STUP_values_stack.PNG", type="png", width=3000, height=2200,pointsize=15,bg="white",dpi=300)
plot(r250_STUP_values_stack)
dev.off()

###---
# fgdb = "/TTML_PlantInventory/p12/bcsarga.gdb"
fgdb = "/TTML_PlantInventory/p20/bcsarga.gdb"

# List  feature classes in a file geodatabase
st_layers(paste(GISDir,fgdb,sep=""))

# for the 06-Aug-2021 output, the p20 gdb was imported
# Available layers:
#   layer_name geometry_type features fields
# 1 TTML_PlantData_Confidential         Point      728     18

# just importing one layer
TTML_values <- st_read(dsn=paste(GISDir,fgdb,sep=""),layer="TTML_PlantData_Confidential") %>%
    st_transform(crs=3005) %>% st_intersection(aoi) %>% st_zm(drop = TRUE, what = "ZM")

TTML_plants <- as.data.frame(TTML_values %>% group_by(Common_nam) %>% count(Latin_name) %>% st_drop_geometry())
write.csv(TTML_plants,"data/TTML_plants.csv", row.names = FALSE)

as.data.frame(TTML_values %>% count(Common_nam) %>% st_drop_geometry())

TTML_values %>% filter(grepl("leth", Latin_name, ignore.case = TRUE)) %>% group_by(Common_nam) %>% count(Latin_name)

TTML_values <- TTML_values %>% mutate(Common_name_clean = case_when(grepl("Alder", Common_nam) ~ "red alder",
                                                              grepl("Hazelnut", Common_nam, ignore.case = TRUE) ~ "beaked hazelnut",
                                                              grepl("goose", Common_nam, ignore.case = TRUE) ~ "swamp gooseberry",
                                                              grepl("Pteridum Aquilinum", Latin_name, ignore.case = TRUE) ~ "bracken fern",
                                                              grepl("Maid", Common_nam, ignore.case = TRUE) ~ "northern maidenhair fern",
                                                              grepl("Sword", Common_nam, ignore.case = TRUE) ~ "sword fern",
                                                              grepl("Bleed", Common_nam, ignore.case = TRUE) ~ "bleeding heart",
                                                              grepl("Alderberry", Common_nam, ignore.case = TRUE) ~ "red elderberry",
                                                              grepl("Fungus", Common_nam, ignore.case = TRUE) ~ "bracket fungus",
                                                              grepl("Fungi", Common_nam, ignore.case = TRUE) ~ "bracket fungus",
                                                              grepl("Agaricus", Latin_name, ignore.case = TRUE) ~ "Agaricus fungus",
                                                              grepl("Letharia", Latin_name, ignore.case = TRUE) ~ "wolf lichen",
                                                              grepl("Moss", Common_nam, ignore.case = TRUE) ~ "moss",
                                                              grepl("Vine", Common_nam, ignore.case = TRUE) ~ "vine maple",
                                                              grepl("Clover", Common_nam, ignore.case = TRUE) ~ "red clover",
                                                              grepl("Horsetail", Common_nam, ignore.case = TRUE) ~ "horsetail",
                                                              grepl("Club", Common_nam, ignore.case = TRUE) ~ "devil's club",
                                                              grepl("Himalayan", Common_nam, ignore.case = TRUE) ~ "Himalyan blackberry",
                                                              grepl("Huckleberry", Common_nam, ignore.case = TRUE) ~ "red huckleberry",
                                                              grepl("Plum", Common_nam, ignore.case = TRUE) ~ "Indian / June plum",
                                                              grepl("Kinni", Common_nam, ignore.case = TRUE) ~ "kinnikinnick",
                                                              grepl("Nurse", Common_nam, ignore.case = TRUE) ~ "nurse tree",
                                                              grepl("Oregon", Common_nam, ignore.case = TRUE) ~ "Oregon grape",
                                                              grepl("Prince", Common_nam, ignore.case = TRUE) ~ "pipsissewa / prince's pine",
                                                              grepl("Salmon", Common_nam, ignore.case = TRUE) ~ "salmonberry",
                                                              grepl("Sorrel", Common_nam, ignore.case = TRUE) ~ "sheep sorrel",
                                                              grepl("Snow", Common_nam, ignore.case = TRUE) ~ "snowberry / waxberry",
                                                              grepl("Trillium", Common_nam, ignore.case = TRUE) ~ "western / white trillium",
                                                              grepl("Cedar", Common_nam, ignore.case = TRUE) ~ "western red cedar",
                                                              grepl("Ginger", Common_nam, ignore.case = TRUE) ~ "wild ginger",
                                                              TRUE ~ as.character(tolower(Common_nam))))


# Alder for both Alder and Red Alder (same latin name)
# Hazelnut for both Hazelnut and Becked Hazelnut (same latin name)
# All gooseberries are the same (although one has latin name for himalayan blackberry so may want to check)
# Black / Bracken Fern all the same
# Maiden Hair and Maidenhair the same
# Group all Sword Ferns
# group Bleeding Hearts
# Keep Blue and Red Elderberry separate
# Red Alderberry should be Red Elderberry
# Group Bracket Fungus together
# check on entry for Vine Maple with Acer Macrophyllum as latin name (is it broadleaf or vine?)
# Group Clover and Red Clover
# Group Common, Giant and Horsetail all as Horsetail
# Group Devel and Devil club - check on 'Devel's Club' with Vaccinium Parvifolium as latin name (red huckleberry)
# Group the two "Dock" as likely Western Dock (Rumex occidentalis)
# 3 blackberry: Evergreen, Himalayan, Trailing (use these prefixes to find the records)
# Group fungi based on Latin name
# Group 3 versions of fungus into Bracket Fungus
# Group all huckleberry as one (Vaccinium parvifolium)
# Group all plum as "Indian Plum or June Plum - Osmaronia cerasiformis)
# Group Kinnikinnicks together
# Group mosses together, except keep "Wold Lichen" or more likely wolf lichen separate
# Group all nurse trees together
# Group (Short) Oregon Grape / Berry together
# Group Prince and Princess pipsissewa together
# Group all salmonberries together
# Group sorrels
# Group snowberries and/or waxberry as Snowberry or Waxberry
# Check on Stinging nettle with latin name Osmaronia cerasiformis (Indian plum)
# what is "test site"
# Group Trilliums together
# Group cedars (Western Red Cedar)
# Group Gingers - check that Asarum caudatum is correct and Asarum circinatum is a typo


TTML_values$Latin_name_clean <- trimws(TTML_values$Latin_name, "both")
TTML_values$Latin_name_clean <- taxize_capwords(TTML_values$Latin_name_clean, onlyfirst = TRUE)

as.data.frame(TTML_values %>% group_by(Common_name_clean) %>% count(Latin_name_clean))
as.data.frame(TTML_values %>% count(Common_name_clean))

write.csv(TTML_values %>% group_by(Common_name_clean, Latin_name_clean, Common_nam) %>% count(Latin_name) %>% st_drop_geometry(),
          "data/TTML_plants_clean.csv", row.names = FALSE)
write.csv(TTML_values %>% st_drop_geometry(),
          "data/TTML_plants_clean_ALL.csv", row.names = FALSE)

TTML_values %>% count(Common_nam)
TTML_values %>% count(Common_name_clean)

ggplot()+
  geom_sf(data = TTML_values, aes(col=Common_name_clean))+
  theme(legend.position = "none")
