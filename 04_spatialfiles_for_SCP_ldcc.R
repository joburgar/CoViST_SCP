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

###--- Feature to aoi retrieval function

retrieve_geodata_aoi <- function (ID=ID){
  aoi.geodata <- bcdc_query_geodata(ID) %>%
    filter(BBOX(st_bbox(aoi))) %>%
    collect()
  aoi.geodata <- aoi.geodata %>% st_intersection(aoi)
  aoi.geodata$Area_km2 <- st_area(aoi.geodata)*1e-6
  aoi.geodata <- drop_units(aoi.geodata)
  return(aoi.geodata)
}

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
###--- also checked to be consistent with LDCC+ layer (see "LDCC_ResourceReport_CoViST_comparison_09Aug2021.xlsx" for comparison)
# LDCC = Land Designations that Contribute to Conservation in BC: https://www.env.gov.bc.ca/soe/indicators/land/land-designations.html

###--- Federal Protected Areas (Canadian Wildlife Service owned)
# National Wildlife Areas, Migratory Bird Sanctuaries, Nature Reserves & Conservation Areas
# download only needs to happen once
# download.file("https://cws-scf.ca/CPCAD-BDCAPC_Dec2020.gdb.zip", "CPCAD-BDCAPC_Dec2020.gdb.zip")
# unzip("CPCAD-BDCAPC_Dec2020.gdb.zip", exdir = ".")
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
glimpse(aoi.CPCAD)

aoi.CPCAD$LndDsgntn <- aoi.CPCAD$TYPE_E
aoi.CPCAD$LndDsgntn_Name <- aoi.CPCAD$NAME_E
aoi.CPCAD$Tier <- "Tier1"

# BC Parks, Ecological Reserves, and Protected Areas : https://catalogue.data.gov.bc.ca/dataset/bc-parks-ecological-reserves-and-protected-areas;
bcdc_search("BC Parks, Ecological Reserves, and Protected Areas", res_format = "wms")
# BC Parks, Ecological Reserves, and Protected Areas (wms, kml, other)
# ID: 1130248f-f1a3-4956-8b2e-38d29d3e4af7
# Name: bc-parks-ecological-reserves-and-protected-areas

aoi.BCPA <- retrieve_geodata_aoi(ID="1130248f-f1a3-4956-8b2e-38d29d3e4af7")

as.data.frame(aoi.BCPA %>% count(PROTECTED_LANDS_NAME)%>% st_drop_geometry())
as.data.frame(aoi.BCPA %>% count(PROTECTED_LANDS_DESIGNATION)%>% st_drop_geometry())
as.data.frame(aoi.BCPA %>% group_by(PROTECTED_LANDS_DESIGNATION) %>% count(PROTECTED_LANDS_NAME) %>% st_drop_geometry())

aoi.BCPA$LndDsgntn <- aoi.BCPA$PROTECTED_LANDS_DESIGNATION
aoi.BCPA$LndDsgntn_Name <- aoi.BCPA$PROTECTED_LANDS_NAME
aoi.BCPA$Tier <- "Tier1"

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

aoi.NGOCA <- retrieve_geodata_aoi(ID="a306e21b-58d6-4b71-bac7-f3b1c8a4c779")

ggplot()+
  geom_sf(data=aoi)+
  geom_sf(data=aoi.NGOCA)
names(aoi.NGOCA)

as.data.frame(aoi.NGOCA %>% count(LEAD_SECUREMENT_ORG)%>% st_drop_geometry())

# CPCAD_aoi %>% group_by(OWNER_E,TYPE_E) %>% summarise(area = sum(Area_km2)) %>% st_drop_geometry()
# aoi.NGOCA %>% group_by(LEAD_SECUREMENT_ORG) %>% summarise(area = sum(Area_km2)) %>% st_drop_geometry()
# aoi.NGOCA contains same # and area of NGO lands as CPCAD, stick with NGOCA (likely updated more frequently)

glimpse(aoi.NGOCA)
aoi.NGOCA$LndDsgntn <- aoi.NGOCA$GENERAL_SECUREMENT_TYPE
aoi.NGOCA$LndDsgntn_Name <- aoi.NGOCA$PROJECT_NAME
aoi.NGOCA$Tier <- "Tier1"

# Local and Regional Greenspaces : https://catalogue.data.gov.bc.ca/dataset/local-and-regional-greenspaces (decision made to filter to Conservation Areas and Parks only and to exclude the Green Space category as it included non conservation areas such as Right-of-Ways);
bcdc_search("Local and Regional Greenspaces", res_format = "wms")
# Local and Regional Greenspaces (wms, kml, xlsx, other)
# ID: 6a2fea1b-0cc4-4fc2-8017-eaf755d516da
# Name: local-and-regional-greenspaces

aoi.LRG <- retrieve_geodata_aoi(ID="6a2fea1b-0cc4-4fc2-8017-eaf755d516da")

aoi.LRG %>% group_by(PARK_TYPE) %>% count(PARK_PRIMARY_USE) %>% st_drop_geometry()
aoi.LRG %>% group_by(PARK_TYPE, PARK_PRIMARY_USE) %>% summarise(area = sum(Area_km2)) %>% st_drop_geometry()
aoi.LRG %>% group_by(PARK_PRIMARY_USE, PARK_TYPE) %>% summarise(area = sum(Area_km2)) %>% st_drop_geometry()

aoi.LRG %>% filter(PARK_TYPE=="Federal") %>% dplyr::select(PARK_NAME, Area_km2)# Widgeon Marsh - 6.08 km2 # must include as extension of federal (CPCAD)
aoi.CPCAD %>% filter(grepl("Widgeon",NAME_E)) %>% dplyr::select(NAME_E, Area_km2)# Widgeon Valley National Wildlife Area - 1.3 km2 # must include as extension of federal (CPCAD)

ggplot() +
  geom_sf(data=aoi.LRG %>% filter(PARK_TYPE=="Federal"), col="blue")+
  geom_sf(data=aoi.CPCAD %>% filter(grepl("Widgeon",NAME_E)), col="red")

aoi.LRG %>% filter(PARK_PRIMARY_USE %in% c("Conservation Area","Park")) %>% summarise(area = sum(Area_km2)) %>% st_drop_geometry()
264 / sum(aoi.LRG$Area_km2) # Conservation Areas & Parks comprise 78% of the local and regional greenspaces
# LDCC + uses "regional" parks which includes Green Space and Water Access - in these cases Green Spaces are regional parks and water access is dyke/narrows
# seems that only "local" Green Space includes ROW, Greenbelts, etc.
aoi.LRG %>% filter(PARK_TYPE=="Regional" & PARK_PRIMARY_USE %in% c("Green Space","Water Access")) %>% group_by(PARK_PRIMARY_USE) %>% count(PARK_NAME)
aoi.LRG %>% filter(PARK_TYPE=="Private" & PARK_PRIMARY_USE %in% c("Green Space","Water Access")) %>% group_by(PARK_PRIMARY_USE) %>% count(PARK_NAME)
aoi.LRG %>% filter(PARK_TYPE=="Local" & PARK_PRIMARY_USE %in% c("Green Space","Water Access")) %>%
  filter(grepl("ROW", PARK_NAME, ignore.case = TRUE)) %>% count(PARK_NAME) %>% st_drop_geometry()
# 44 ROWs in local Green Space, also includes Greenbelts, Detention Ponds, Reserves and Parks...
aoi.LRG %>% filter(PARK_TYPE=="Local" & PARK_PRIMARY_USE %in% c("Water Access")) %>% count(PARK_NAME) %>% st_drop_geometry()

# instead of omitting all other greenspace, RECOMMEND splitting into 2
# Federal and Regional: all parcels = "locked in" lands that contribute to conservation
# Private: Green Space and Park = "locked in" and Golf Course tier 2
# Local: Conservation Area, Park, Water Access and grepl("Park") within Green Space = locked in, all other tier 2
# all other parcels = remove the "cost" of the land (i.e., add to the cost raster layer and change cost to 0)

aoi.LRG$Tier <- ifelse(aoi.LRG$PARK_TYPE=="Federal", "Tier1",
                          ifelse(aoi.LRG$PARK_TYPE=="Regional", "Tier1",
                                 ifelse(aoi.LRG$PARK_TYPE=="Private" & aoi.LRG$PARK_PRIMARY_USE %in% c("Green Space", "Park"), "Tier1",
                                        ifelse(aoi.LRG$PARK_TYPE=="Local" & aoi.LRG$PARK_PRIMARY_USE %in% c("Conservation Area","Park","Water Access"), "Tier1", "Tier2"))))

# aoi.LRG %>% filter(PARK_TYPE=="Local" & PARK_PRIMARY_USE=="Green Space") %>% filter(grepl("Park",PARK_NAME, ignore.case = TRUE)) %>% summarise(area = sum(Area_km2)) %>% st_drop_geometry()
# 185 'parks' covering 7.76 km2

aoi.LRG$Tier <- case_when(aoi.LRG$PARK_TYPE=="Local" & aoi.LRG$PARK_PRIMARY_USE=="Green Space" & grepl("Park", aoi.LRG$PARK_NAME, ignore.case = TRUE) ~ "Tier1",
                              TRUE ~ as.character(aoi.LRG$Tier))

# LRG.Tier <- as.data.frame(aoi.LRG %>% group_by(Tier,PARK_TYPE,PARK_PRIMARY_USE) %>% summarise(n=n(), area = sum(Area_km2)) %>% st_drop_geometry())
# write.csv(LRG.Tier, "data/LRG.Tier.csv", row.names = FALSE)

# aoi.LRG %>% group_by(Tier) %>% summarise(area = sum(Area_km2)) %>% st_drop_geometry()
#
# ggplot() +
#   geom_sf(data=aoi)+
#   geom_sf(data=aoi.LRG, aes(fill=Tier), lwd=0)+
#   scale_fill_manual(values = pnw_palette("Bay",2))+
#   theme(legend.title = element_blank(), legend.position = "bottom")
#
#
# ggplot() +
#   geom_sf(data=aoi)+
#   geom_sf(data=aoi.LRG, aes(fill=PARK_TYPE), lwd=0)+
#   scale_fill_manual(values = pnw_palette("Bay",4))+
#   theme(legend.title = element_blank(), legend.position = "bottom")

glimpse(aoi.LRG)
aoi.LRG$LndDsgntn <- paste(aoi.LRG$PARK_TYPE, aoi.LRG$PARK_PRIMARY_USE, sep=" - ")
aoi.LRG$LndDsgntn_Name <- aoi.LRG$PARK_NAME

# Conservation Lands  (Wildlife Management Areas and other administered conservation lands): https://catalogue.data.gov.bc.ca/dataset/conservation-lands;
bcdc_search("Wildlife Management Areas", res_format = "wms")
# TANTALIS - Wildlife Management Areas (other, wms, kml)
# ID: f3ece977-aa7f-4cb2-b7d0-de64155f6c83
# Name: tantalis-wildlife-management-areas
aoi.WMA <- retrieve_geodata_aoi(ID="f3ece977-aa7f-4cb2-b7d0-de64155f6c83")

aoi.WMA %>% group_by(WILDLIFE_MANAGEMENT_AREA_NAME) %>% summarise(area=sum(Area_km2)) %>% st_drop_geometry()

glimpse(aoi.WMA)
aoi.WMA$LndDsgntn <- "Wildlife Management Area"
aoi.WMA$LndDsgntn_Name <- aoi.WMA$WILDLIFE_MANAGEMENT_AREA_NAME
aoi.WMA$Tier <- "Tier1"


# Ungulate Winter Range - Approved : https://catalogue.data.gov.bc.ca/dataset/ungulate-winter-range-approved;
bcdc_search("Ungulate Winter Range", res_format = "wms")
# Ungulate Winter Range - Approved (other, wms, kml)
# ID: 712bd887-7763-4ed3-be46-cdaca5640cc1
# Name: ungulate-winter-range-approved

aoi.UWR <- retrieve_geodata_aoi(ID="712bd887-7763-4ed3-be46-cdaca5640cc1")

aoi.UWR %>% group_by(UWR_NUMBER) %>% summarise(area=sum(Area_km2)) %>% st_drop_geometry()
aoi.UWR %>% group_by(TIMBER_HARVEST_CODE) %>% summarise(area=sum(Area_km2)) %>% st_drop_geometry()

aoi.UWR$Tier <- ifelse(aoi.UWR$TIMBER_HARVEST_CODE=="NO HARVEST ZONE", "Tier1",
                           ifelse(aoi.UWR$TIMBER_HARVEST_CODE=="CONDITIONAL HARVEST ZONE", "Tier2", NA))

aoi.UWR %>% group_by(TIMBER_HARVEST_CODE) %>% count(Tier) %>% st_drop_geometry()

glimpse(aoi.UWR)
aoi.UWR$LndDsgntn <- paste("Ungulate Winter Range - ",aoi.UWR$TIMBER_HARVEST_CODE,sep="")
aoi.UWR$LndDsgntn_Name <- aoi.UWR$UWR_NUMBER

# Wildlife Habitat Areas – Approved (non-secured) : https://catalogue.data.gov.bc.ca/dataset/wildlife-habitat-areas-approved (there is also a secured version of this layer, as some polygons are identified as “data sensitive; the contact is Don Philip, Spatial Information Specialist (Don.Philip@gov.bc.ca): https://catalogue.data.gov.bc.ca/dataset/wildlife-habitat-areas-approved-secure-
bcdc_search("Wildlife habitat area", res_format = "wms")
# Wildlife Habitat Areas - Approved (other, wms, kml)
# ID: b19ff409-ef71-4476-924e-b3bcf26a0127
# Name: wildlife-habitat-areas-approved

aoi.WHA <- retrieve_geodata_aoi(ID="b19ff409-ef71-4476-924e-b3bcf26a0127")

aoi.WHA %>% group_by(TAG) %>% summarise(area=sum(Area_km2)) %>% st_drop_geometry()
aoi.WHA %>% group_by(TIMBER_HARVEST_CODE) %>% summarise(area=sum(Area_km2)) %>% st_drop_geometry()

aoi.WHA$Tier <- ifelse(aoi.WHA$TIMBER_HARVEST_CODE=="NO HARVEST ZONE", "Tier1",
                           ifelse(aoi.WHA$TIMBER_HARVEST_CODE=="CONDITIONAL HARVEST ZONE", "Tier2", NA))

aoi.WHA %>% group_by(TIMBER_HARVEST_CODE) %>% count(Tier) %>% st_drop_geometry()

glimpse(aoi.WHA)
aoi.WHA$LndDsgntn <- paste("Wildlife Habitat Area - ",aoi.WHA$TIMBER_HARVEST_CODE,sep="")
aoi.WHA$LndDsgntn_Name <- aoi.WHA$TAG

# Legal Planning Objectives (for Biodiversity, Mining and Tourism Areas & Wildland Areas)
bcdc_search("WHSE_LAND_USE_PLANNING.RMP_PLAN_LEGAL_POLY_SVW", res_format = "wms")
# 1: Legal Planning Objectives - Current - Polygon (other, wms, kml)
# ID: 2c02040c-d7c5-4960-8d04-dea01d6d3e9f
# Name: legal-planning-objectives-current-polygon

# aoi.BMTA <- retrieve_geodata_aoi(ID="2c02040c-d7c5-4960-8d04-dea01d6d3e9f")
# aoi.BMTA %>% count(LEGAL_FEAT_OBJECTIVE) %>% st_drop_geometry()
# sum(aoi.BMTA$Area_km2) # 6.7 km2
# the only applicable objective in aoi is "Cultural Management Objective"
# no need to include

# Old Growth Management Areas – Legal – Current ; https://catalogue.data.gov.bc.ca/dataset/old-growth-management-areas-legal-current
bcdc_search("Old Growth Management Areas", res_format = "wms")
# Old Growth Management Areas - Legal - Current (other, wms, kml)
# ID: 1b30f3bd-0ad0-4128-916b-66c6dd91dea4
# Name: old-growth-management-areas-legal-current

aoi.OGMA <- retrieve_geodata_aoi(ID="1b30f3bd-0ad0-4128-916b-66c6dd91dea4")
sum(aoi.OGMA$Area_km2) # 290 km2

ggplot()+
  geom_sf(data=aoi)+
  geom_sf(data=aoi.OGMA)

glimpse(aoi.OGMA)
aoi.OGMA$LndDsgntn <- "Old Growth Management Area"
aoi.OGMA$LndDsgntn_Name <- aoi.OGMA$LEGAL_OGMA_INTERNAL_ID
aoi.OGMA$Tier <- "Tier2"

###--- Managed Lands (as per the LDCC layer)
# includes: Recreation Sites, VQO_retain, Land Act Reserves, UWR & WHA conditional harves, and community watersheds

# Forest Recreation Site - High Restricted
# Recreation Sites (pull from Recreation Polygon layer)
bcdc_search("Forest Recreation Sites", res_format = "wms")
# Recreation Polygons (other, wms, kml)
# ID: 263338a7-93ee-49c1-83e8-13f0bde70833
# Name: recreation-polygons

# PROJECT_TYPE in ('Recreation Reserve', 'Interpretative Forest') and FILE_STATUS_CODE = 'HI' and RETIREMENT_DATE IS NULL
aoi.FRS <- retrieve_geodata_aoi(ID="263338a7-93ee-49c1-83e8-13f0bde70833")

aoi.FRS %>% group_by(PROJECT_TYPE) %>% summarise(area = sum(Area_km2)) %>% st_drop_geometry()
aoi.FRS %>% group_by(PROJECT_TYPE) %>% count(FILE_STATUS_CODE) %>% st_drop_geometry()
aoi.FRS <- aoi.FRS %>% filter(FILE_STATUS_CODE=="HI" & is.na(RETIREMENT_DATE))
aoi.FRS %>% group_by(PROJECT_TYPE) %>% summarise(area = sum(Area_km2)) %>% st_drop_geometry()

aoi.FRS$LndDsgntn <- aoi.FRS$PROJECT_TYPE
aoi.FRS$LndDsgntn_Name <- aoi.FRS$PROJECT_NAME
aoi.FRS$Tier <- "Tier2"

# Visual Quality (not sure about this one - need confirmation on the filtering field)
bcdc_search("Visual Quality", res_format = "wms")
# Visual Landscape Inventory (other, wms, kml)
# ID: 4e941067-20ec-4b5d-bca3-8831c9b2e4db
# Name: visual-landscape-inventory
aoi.VQO <- retrieve_geodata_aoi(ID="4e941067-20ec-4b5d-bca3-8831c9b2e4db")

# REC_MADE_KNOWN_CODE = 'Y' AND REC_EVQO_CODE = 'P' 'R' 'MM'
# REC_MADE_KNOWN_CODE = 'Y' AND REC_RVQC_CODE = 'pr', 'm'
aoi.VQO %>% group_by(REC_MADE_KNOWN_CODE, REC_EVQO_CODE, REC_RVQC_CODE) %>% summarise(area = sum(Area_km2)) %>% st_drop_geometry()
aoi.VQO %>% group_by(REC_RVQC_CODE, REC_MADE_KNOWN_CODE) %>% summarise(area = sum(Area_km2)) %>% st_drop_geometry()


aoi.VQO <- aoi.VQO %>% filter(REC_EVQO_CODE %in% c("P","R","MM") & REC_MADE_KNOWN_CODE=="Y" |
                                REC_RVQC_CODE %in% c("pr","m") & REC_MADE_KNOWN_CODE=="Y")
aoi.VQO %>% group_by(REC_EVQO_CODE, REC_RVQC_CODE, REC_MADE_KNOWN_CODE) %>% summarise(area = sum(Area_km2)) %>% st_drop_geometry()

ggplot()+
  geom_sf(data = aoi)+
  geom_sf(data = aoi.VQO, fill="darkgreen")

glimpse(aoi.VQO)
aoi.VQO$LndDsgntn <- ifelse(aoi.VQO$REC_EVQO_CODE=="P", "Visual Quality Objective - Preservation",
                            ifelse(aoi.VQO$REC_EVQO_CODE=="R", "Visual Quality Objective - Retention",
                                   ifelse(aoi.VQO$REC_RVQC_CODE=="pr", "Visual Quality Objective - Partial Retention",
                                          ifelse(aoi.VQO$REC_RVQC_CODE=="m", "Visual Quality Objective - Modification",
                                                 ifelse(aoi.VQO$REC_EVQO_CODE=="MM", "Visual Quality Objective = Maximum Modificaiton", NA)))))
aoi.VQO$LndDsgntn_Name <- aoi.VQO$VLI_POLYGON_NO
aoi.VQO$Tier <- "Tier2"

# Land Act Recreation Reserve (UREP) # WHSE_TANTALIS.TA_CROWN_TENURES_SVW
# redundant with the LAR below
# bcdc_search("TANTALIS CROWN TENURES", res_format = "wms")
# # 9: TANTALIS - Crown Tenures (other, wms, kml)
# # ID: 3544ad91-0cf2-4926-a08a-bfe42d9a031d
# # Name: tantalis-crown-tenures
# aoi.LARR <- retrieve_geodata_aoi(ID="3544ad91-0cf2-4926-a08a-bfe42d9a031d")
#
# # TENURE_STAGE = 'TENURE' AND TENURE_SUBPURPOSE = 'UREP/RECREATION RESERVE'
# aoi.LARR <- aoi.LARR %>% filter(TENURE_STAGE=="TENURE" & TENURE_SUBPURPOSE=="UREP/RECREATION RESERVE")
# aoi.LARR %>% group_by(TENURE_STAGE, TENURE_SUBPURPOSE) %>% summarise(area=sum(Area_km2)) %>% st_drop_geometry() # 5.86 km2
# aoi.LARR %>% group_by(TENURE_STAGE, TENURE_SUBPURPOSE,TENURE_TYPE) %>% count(TENURE_SUBTYPE) %>% st_drop_geometry() # 5.86 km2
#
#
# glimpse(aoi.LARR)
# aoi.LARR$LndDsgntn <- paste(aoi.LARR$TENURE_SUBPURPOSE, aoi.LARR$TENURE_SUBTYPE, sep="_")
# aoi.LARR$LndDsgntn_Name <- aoi.LARR$INTRID_SID
# aoi.LARR$Tier <- "Tier2"

# Land Act Reserves
# might also include UREP/RECREATION RESERVES, so will want to check that we want to include these extra parcels
bcdc_search("Land Act Reserves", res_format = "wms")
# TANTALIS - Crown Land Reserves and Notations (other, wms, kml)
# ID: 589cb979-731f-4151-bb2b-10ed66278099
# Name: tantalis-crown-land-reserves-and-notations
aoi.LAR <- retrieve_geodata_aoi(ID="589cb979-731f-4151-bb2b-10ed66278099")
as.data.frame(aoi.LAR %>% group_by(TENURE_SUBTYPE, TENURE_PURPOSE) %>% summarise(area = sum(Area_km2)) %>% st_drop_geometry())
aoi.LAR %>% filter(TENURE_PURPOSE=="ENVIRONMENT, CONSERVATION, & RECR") %>% group_by(TENURE_SUBTYPE) %>% summarise(area = sum(Area_km2)) %>% st_drop_geometry()
aoi.LAR <- aoi.LAR %>% filter(TENURE_PURPOSE=="ENVIRONMENT, CONSERVATION, & RECR" & TENURE_SUBTYPE!="NOTATION OF INTEREST")

glimpse(aoi.LAR)
aoi.LAR$LndDsgntn <- paste(aoi.LAR$TENURE_SUBPURPOSE, aoi.LAR$TENURE_SUBTYPE, sep=" - ")
aoi.LAR$LndDsgntn_Name <- aoi.LAR$INTRID_SID
aoi.LAR$Tier <- "Tier2"

as.data.frame(aoi.LAR %>% group_by(LndDsgntn) %>% summarise(n=n(), area=sum(Area_km2)) %>% st_drop_geometry())
aoi.LAR %>% filter(grepl("UREP", LndDsgntn)) %>% group_by(LndDsgntn) %>% summarise(area=sum(Area_km2)) %>% st_drop_geometry()

# 6: Conservation Lands (other, wms, kml)
# ID: 68327529-c0d5-4fcb-b84e-f8d98a7f8612
# Name: conservation-lands
# WHSE_LEGAL_ADMIN_BOUNDARIES.WCL_CONSERVATION_LANDS_SP

aoi.CRL <- retrieve_geodata_aoi(ID="68327529-c0d5-4fcb-b84e-f8d98a7f8612")

aoi.CRL %>% group_by(CONSERVATION_LAND_TYPE, TENURE_TYPE) %>% summarise(n=n(), area = sum(Area_km2)) %>% st_drop_geometry()
aoi.CRL <- aoi.CRL %>% filter(CONSERVATION_LAND_TYPE=="Reserve Lands")
glimpse(aoi.CRL)
aoi.CRL$LndDsgntn <- aoi.CRL$CONSERVATION_LAND_TYPE
aoi.CRL$LndDsgntn_Name <- aoi.CRL$SITE_NAME
aoi.CRL$Tier <- "Tier2"


# Community Watersheds
# WHSE_WATER_MANAGEMENT.WLS_COMMUNITY_WS_PUB_SVW
bcdc_search("Community watershed", res_format = "wms")
# 2: Community Watersheds - Current (other, wms, kml)
# ID: bc57faf7-23e4-43fe-918a-e999936dbafa
# Name: community-watersheds-current
aoi.CWtr <- retrieve_geodata_aoi(ID="bc57faf7-23e4-43fe-918a-e999936dbafa")
aoi.CWtr %>% group_by(CW_USE) %>% summarise(area = sum(Area_km2)) %>% st_drop_geometry()
aoi.CWtr %>% group_by(CW_STATUS, ORGANIZATION_TYPE) %>% count(CW_DATE_CREATED)

ggplot()+
  geom_sf(data = aoi)+
  geom_sf(data = aoi.CWtr, col="blue")

glimpse(aoi.CWtr)
aoi.CWtr$LndDsgntn <- "Community Watershed"
aoi.CWtr$LndDsgntn_Name <- aoi.CWtr$CW_NAME
aoi.CWtr$Tier <- "Tier2"

# to do a check and calculate area for each cost group

##############################################################################################
# need to create 'id' for CPCAD and use the 'id' as a source field for all BCGW data
# add in Tier 1 and Tier 2 for each record
# simplify sf objects and then merge into one sf object for Tier 1 and Tier 2
# use fasterize and stackapply to create one single raster with overlapping (summed) conservation values of Tier 1
# only need a "flat" Tier 2 although might be interesting to create summed Tier 2 to find any areas with overlap
# then do an overlap between Tier 1 and Tier 2? See if any of the Tier 2 are locked out from different layers

LDCC_layers <- list(aoi.CPCAD, aoi.BCPA, aoi.NGOCA, aoi.LRG, aoi.WMA, aoi.UWR, aoi.WHA, aoi.OGMA, aoi.FRS, aoi.VQO, aoi.LAR, aoi.CRL, aoi.CWtr)
LDCC_source <- c("CPCAD", "BCPA", "NGOCA", "LRG", "WMA", "UWR", "WHA", "OGMA", "FRS", "VQO", "LAR", "CRL","CWtr")


# reformat layers the same for merging into one sf object
for(i in 1:length(LDCC_layers)){
  LDCC_layers[[i]]$Source <- LDCC_source[i]
  LDCC_layers[[i]] <- rename(LDCC_layers[[i]], ID = 1)
  LDCC_layers[[i]]$ID <- as.character(LDCC_layers[[i]]$ID)
  LDCC_layers[[i]] <- LDCC_layers[[i]] %>% dplyr::select("ID","Source","LndDsgntn","LndDsgntn_Name","Tier","Area_km2")
  LDCC_layers[[i]] <- rename(LDCC_layers[[i]], geometry = 7)
}

# merge into one sf object
aoi.LDCC <- do.call(rbind, LDCC_layers)
aoi.LDCC$LndDsgntn <-str_to_title(aoi.LDCC$LndDsgntn)

# calculate area and number of parcels per layer (before unioning overlaps within designations)
aoi.LDCC %>% group_by(Tier) %>% summarise(n=n(), area=sum(Area_km2)) %>% st_drop_geometry()
#   Tier      n  area
# 1 Tier1  2408 1985.
# 2 Tier2  2982 2219.

write.csv(as.data.frame(aoi.LDCC %>% group_by(Tier, Source) %>% summarise(n=n(), area=sum(Area_km2)) %>% st_drop_geometry()),
          "out/LDCC_Source_Area_overlap.csv", row.names = FALSE)

write.csv(as.data.frame(aoi.LDCC %>% group_by(Tier, Source, LndDsgntn) %>% summarise(n=n(), area=sum(Area_km2)) %>% st_drop_geometry()),
          "out/LDCC_Area_overlap.csv", row.names = FALSE)

# combine and union within Land Designation groupings for area without internal overlaps
aoi.LDCC_union <- aoi.LDCC %>% group_by(Tier, LndDsgntn) %>%
  summarise(across(geometry, ~ st_union(.)), .groups = "keep") %>%
  summarise(across(geometry, ~ st_combine(.)))
aoi.LDCC_union$Area_km2 <- st_area(aoi.LDCC_union)*1e-6
aoi.LDCC_union <- drop_units(aoi.LDCC_union)

aoi.LDCC_union %>% group_by(Tier) %>% summarise(n=n(), area=sum(Area_km2)) %>% st_drop_geometry()
#   Tier      n  area
# 1 Tier1    22 1960.
# 2 Tier2    44 2198.
# very little overlap within designation
options(scipen = 999)

write.csv(as.data.frame(aoi.LDCC_union %>% group_by(Tier, LndDsgntn) %>% summarise(area=sum(Area_km2)) %>% st_drop_geometry()),
          "out/LDCC_Area_nooverlap.csv", row.names = FALSE)

aoi.LDCC.map <- ggplot()+
  geom_sf(data = aoi)+
  geom_sf(data = aoi.LDCC, aes(fill=Source), lwd=0)+
  scale_fill_manual(values = rev(pnw_palette("Cascades",13)))+
  theme(legend.title = element_blank(), legend.position = "bottom")+
  labs(title="Land Designations that Contribute to Conservation\nTiers 1 and 2")

Cairo(file="out/aoi.LDCC.map.PNG", type="png", width=3000, height=2200,pointsize=15,bg="white",dpi=300)
aoi.LDCC.map
dev.off()

aoi.LDCC.Tier1.map <- ggplot()+
  geom_sf(data = aoi)+
  geom_sf(data = aoi.LDCC %>% filter(Tier=="Tier1"), aes(fill=Source), lwd=0)+
  scale_fill_manual(values = rev(pnw_palette("Cascades",7)))+
  theme(legend.title = element_blank(), legend.position = "bottom")+
  labs(title="Land Designations that Contribute to Conservation\nTier 1")

Cairo(file="out/aoi.LDCC.Tier1.map.PNG", type="png", width=3000, height=2200,pointsize=15,bg="white",dpi=300)
aoi.LDCC.Tier1.map
dev.off()

aoi.LDCC.Tier2.map <- ggplot()+
  geom_sf(data = aoi)+
  geom_sf(data = aoi.LDCC %>% filter(Tier=="Tier2"), aes(fill=Source), lwd=0)+
  scale_fill_manual(values = rev(pnw_palette("Cascades",9)))+
  theme(legend.title = element_blank(), legend.position = "bottom")+
  labs(title="Land Designations that Contribute to Conservation\nTier 2")

Cairo(file="out/aoi.LDCC.Tier2.map.PNG", type="png", width=3000, height=2200,pointsize=15,bg="white",dpi=300)
aoi.LDCC.Tier2.map
dev.off()

###--- polygon to raster tif and map function

poly_to_raster <- function (aoi=aoi, crs=26910, res=100, poly=poly, by=feature, output_name=output_name){
  rres <- raster(ext=extent(aoi), crs=crs, res=c(res,res))
  rres_poly <- fasterize(poly, rres, by=feature)
  rres_poly_stack <- stackApply(rres_poly, indices=1, fun=sum)
  writeRaster(rres_poly_stack, file=paste("out/r",res,"_",output_name,"_stack.tif",sep=""), bylayer=TRUE, overwrite=TRUE)
  Cairo(file=paste("out/r",res,"_",output_name,"_stack.PNG",sep=""), type="png", width=3000, height=2200,pointsize=15,bg="white",dpi=300)
  plot(rres_poly_stack)
  dev.off()
  return(rres_poly_stack)
}


# to be able to "fasterize" must be all same geometry types
aoi.LDCC <- st_cast(aoi.LDCC, "MULTIPOLYGON")

# Tier 1
r100_LDCC_Tier1_stack <- poly_to_raster(aoi=aoi, crs=26910, res=100, poly=aoi.LDCC %>% filter(Tier=="Tier1"),
                                        by="LndDsgntn", output_name="LDCC_Tier1")

r250_LDCC_Tier1_stack <- poly_to_raster(aoi=aoi, crs=26910, res=250, poly=aoi.LDCC %>% filter(Tier=="Tier1"),
                                        by="LndDsgntn", output_name="LDCC_Tier1")


# Tier 2
r100_LDCC_Tier2_stack <- poly_to_raster(aoi=aoi, crs=26910, res=100, poly=aoi.LDCC %>% filter(Tier=="Tier2"),
                                        by="LndDsgntn", output_name="LDCC_Tier2")

r250_LDCC_Tier2_stack <- poly_to_raster(aoi=aoi, crs=26910, res=250, poly=aoi.LDCC %>% filter(Tier=="Tier2"),
                                        by="LndDsgntn", output_name="LDCC_Tier2")


aoi
# Both Tiers
r100_LDCC_stack <- poly_to_raster(aoi=aoi, crs=26910, res=100, poly=aoi.LDCC,
                                        by="LndDsgntn", output_name="LDCC_Tiers1_2")

r250_LDCC_stack <- poly_to_raster(aoi=aoi, crs=26910, res=250, poly=aoi.LDCC,
                                        by="LndDsgntn", output_name="LDCC_Tiers1_2")
