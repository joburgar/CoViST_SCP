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
list.of.packages <- c("tidyverse", "lubridate","chron","bcdata", "bcmaps","sf", "rgdal", "readxl", "Cairo", "OpenStreetMap", "ggmap","PNWColors")
# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
#####################################################################################

GISDir <- "//spatialfiles.bcgov/work/wlap/sry/Workarea/jburgar/CoViST"

# load relevant spatial files
aoi <- st_read(dsn=GISDir, layer="CoViST_Study_Boundary_July2021") # updated to reflect decision at June 22 STSA meeting
aoi.CH.EO.250m <- st_read(dsn=paste(getwd(),"/out", sep=""), layer="aoi.CH.EO.250m")
FA_SpRich <- st_read(dsn=paste(getwd(),"/out", sep=""), layer="Focal_Area")

ggplot()+
  geom_sf(data = aoi, fill=NA) +
  geom_sf(data = FA_SpRich)+
  geom_sf(data = aoi.CH.EO.250m %>% filter(Focl_Ar=="Chilliwack East"), aes(fill=ELCODE))+
  geom_sf(data = aoi.CH.EO.250m %>% filter(Focl_Ar=="Vedder"), aes(fill=ELCODE))


# load species list
sp.list <- read_excel("data/species_list_Jul-06-21.xlsx",sheet = 1, trim_ws = TRUE, col_types = c("text")) %>% type.convert() # updated to 101 included species

# create a species list for each focal area
FA_Value <- aoi.CH.EO.250m %>% filter(Val_dst==0) %>% group_by(Focl_Ar) %>% count(ELCODE, Spcs_Cm, Spcs_Sc) %>% st_drop_geometry()
FA_Value %>% filter(Focl_Ar=="Chilliwack East") %>% count(ELCODE)

# load attribute table
threats <- read_excel("data/threats_list_May-06-21.xlsx",sheet = 2, trim_ws = TRUE, col_types = c("text")) %>% type.convert()
threats %>% group_by(Species_Common, Severity, Impact) %>% count(threat_desc, sub_threat_desc)

as.data.frame(threats %>% group_by(threat_desc) %>% count(Impact))
threats %>% count(Impact)

# create a consolidated Impact field where a range of impact values becomes the high end, very high becomes high, and unknown is the field if NA, unknown or not calculated
threats$Impact2 <- recode(threats$Impact, `High-Low`="High", `High-Medium`="High", `Medium-Low`="Medium", `Very High-High`="High", `Very High`="High", `Not Calculated` = "Unknown")
threats$Impact2 <- threats$Impact2 %>% replace_na("Unknown")

threats %>% count(Impact2)
threats %>% count(ELCODE)

nrow(threats %>% filter(!is.na(sub_threat_desc)))
threats_sub <- threats %>% filter(!is.na(sub_threat_desc))
threats_sub %>% count(Impact)
threats_sub %>% group_by(Species_Common, Severity, Impact) %>% count(threat_desc)
as.data.frame(threats_sub %>% filter(Impact!="Low") %>% group_by(Species_Common) %>% count(threat_desc))
as.data.frame(threats_sub %>% filter(Impact!="Low") %>% count(threat_desc))
as.data.frame(threats %>% group_by(Impact2) %>% count(threat_desc))

FA_Value %>% count(ELCODE)

FA_Threats <- left_join(FA_Value %>% dplyr::select(Focl_Ar, ELCODE),
                        threats %>% dplyr::select(ELCODE, Species_Common, Species_Scientific, threat_num, threat_desc, Impact2),
                        by="ELCODE")

FA_Threats$Impact2 <- factor(FA_Threats$Impact2, levels = c("High","Medium","Low","Unknown"))
# only the highest impact rating per species - need to arrange dataset first
FA_Threats <- FA_Threats %>% arrange(Focl_Ar, ELCODE, threat_num, Impact2)

# remove duplicated rows (i.e., the extra rows because of sub-categories)
FA_Threats <- FA_Threats %>% distinct(Focl_Ar, ELCODE, threat_num, .keep_all = TRUE)
FA_Threats <- FA_Threats[complete.cases(FA_Threats),]

FA_Threats %>% count(ELCODE)

FA.threat.impact <-FA_Threats %>% group_by(Focl_Ar, threat_num, threat_desc) %>% count(Impact2)
FA.threat.impact %>% filter(is.na(threat_desc))

threat.impact <- as.data.frame(FA_Threats %>% count(threat_desc))
threat.impact <- threat.impact %>% arrange(-n, threat_desc)
threat.Order <- fct_reorder(threat.impact$threat_desc, threat.impact$n, max)

FA.threat.impact$threat_desc <- factor(FA.threat.impact$threat_desc, levels = threat.Order)

FA.threat.impact$Impact2 <- factor(FA.threat.impact$Impact2, levels = c("High","Medium","Low","Unknown"))

levels(FA.threat.impact$threat_desc)
FA.threat.impact$threat_abr <- recode(FA.threat.impact$threat_desc,
                             `Agriculture & aquaculture`="AA",
                             `Biological resource use`="BR",
                             `Climate change & severe weather`="CC",
                             `Energy production & mining`= "EM",
                             `Geological events`="GE",
                             `Human intrusions & disturbance` = "HD",
                             `Invasive & other problematic species, genes & diseases` = "IS",
                             `Natural system modifications` = "NM",
                             `Pollution` = "PO",
                             `Residential & commercial development` = "RD",
                             `Transportation & service corridors` = "TC")

# a figure showing the number of values (species and ecological communities)
# associated with each of the IUCN threat categories
# caveat = not all values have been assessed against the IUCN threats so a subset of values
# the colour coding depicts the impact of the threat (if calculated)
# threats ordered by highest to lowest number of associated values overall (same per graph for consistency)

col <- rev(pnw_palette("Bay"))

value.threat.hist <- ggplot(data = FA.threat.impact, # %>% filter(Impact2 %in% c("High", "Medium")),
                            aes(x = threat_abr, y = n, fill=Impact2)) +
  geom_bar(stat = "identity") +
  #scale_fill_manual(values = (pnw_palette("Bay",2)))+
  scale_fill_manual(values = rev(pnw_palette("Bay")))+
  theme_classic() + ylab("Number of Values with Associated Threat") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 10))+
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.title.x = element_blank())+
  theme(legend.title = element_blank()) +
  facet_wrap(~Focl_Ar, ncol=3)

Cairo(file="out/value.threat.hist.PNG", type="png", width=3000, height=2200,pointsize=15,bg="white",dpi=300)
value.threat.hist
dev.off()

Cairo(file="out/value.top.threat.hist.PNG", type="png", width=3000, height=2200,pointsize=15,bg="white",dpi=300)
value.top.threat.hist
dev.off()

FA_Threats <- left_join(FA_SpRich %>% select(Focl_Ar, Fcl_Grp), FA.threat.impact)
colnames(FA_Threats)[3:6] <- c("Thrt_No","Threat","Impact","Count")
st_write(FA_Threats, paste0(getwd(),"/out/Focal_Area_Threats.shp"), delete_layer = TRUE) # for the threat widget
FA_Threats %>% group_by(Focl_Ar, Threat) %>% summarise(sum(Count)) %>% st_drop_geometry()
####################################################################################
###--- incorporate PP threat assessment
# MasterList_SAR_PriorityPlaces_Threats_RollUp_Final_Share
# load Priority Place threats list
pp.threats <- read_excel("data/MasterList_SAR_PriorityPlaces_Threats_RollUp_Final_Share.xlsx",sheet = 10, trim_ws = TRUE, col_types = c("text")) %>% type.convert()
head(pp.threats)
unique(pp.threats$`Scientific Name`)

# create a threat num category for simplicity when joining
glimpse(pp.threats)

pp.threats$threat_num <- as.numeric(substr(pp.threats$`Threat Category`,1,1))
pp.threats$threat_num <- case_when(pp.threats$`Threat Category`=="10 Geological events" ~ 10,
                                   pp.threats$`Threat Category`=="11 Climate Change & severe weather" ~ 11,
                                   TRUE ~ as.numeric(pp.threats$threat_num))
pp.threats$threat_num <- as.integer(pp.threats$threat_num)

# check that both threat num and threat categories are the same
pp.threats %>% group_by(threat_num) %>% count(`Threat Category`)
FA_Threats %>% group_by(threat_num) %>% count(threat_desc)
orig_threat_splist <- unique(FA_Threats$ELCODE)

glimpse(FA_Threats)
glimpse(pp.threats)
colnames(pp.threats)[1] <-c("Species_Common")
pp.threats %>% filter(ELCODE %in% orig_threat_splist) %>% count(Species_Common)

# now create a similar FA_Threats object to compare
FA_PP_Threats <- left_join(FA_Value %>% dplyr::select(Focl_Ar, ELCODE),
                        pp.threats %>% dplyr::select(ELCODE, Species_Common, `Scientific Name`, threat_num, `Threat Category`, `Threat Impact`, Rating),
                        by="ELCODE")


FA_PP_Threats <- FA_PP_Threats %>% group_by(Focl_Ar, ELCODE, Species_Common, `Scientific Name`,threat_num) %>% summarise(Rating=sum(Rating))
FA_Threats %>% filter(ELCODE=="AAABH01021") %>% group_by(Focl_Ar, threat_num) %>% count(Impact2) # Northern Red-legged Frog
FA_PP_Threats %>% filter(ELCODE=="AAABH01021") %>% filter(!is.na(Rating)) %>% group_by(Focl_Ar, threat_num) %>% summarise(Rating) # Northern Red-legged Frog
glimpse(FA_PP_Threats)


# only the highest impact rating per species - need to arrange dataset first
FA.PP.threat.impact <-FA_PP_Threats %>% group_by(Focl_Ar, threat_num) %>% filter(!is.na(Rating))
FA.PP.threat.impact %>% filter(is.na(Rating))
FA.PP.threat.impact$threat_abr <- FA.threat.impact$threat_abr[match(FA.PP.threat.impact$threat_num, FA.threat.impact$threat_num)]

threat_abr_order <- levels(FA.threat.impact$threat_abr)
FA.PP.threat.impact$threat_abr <- factor(FA.PP.threat.impact$threat_abr, levels = threat_abr_order)


PP.value.threat.hist <- ggplot(data = FA.PP.threat.impact, # %>% filter(Impact2 %in% c("High", "Medium")),
                            aes(x = threat_abr, y = Rating, fill=Species_Common)) +
  geom_bar(stat = "identity") +
  #scale_fill_manual(values = (pnw_palette("Bay",2)))+
  scale_fill_manual(values = rev(pnw_palette("Bay",26)))+
  theme_classic() + ylab("Threat Rating (summed by value) with Associated Threat") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 10))+
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.title.x = element_blank())+
  theme(legend.title = element_blank(), legend.position = "bottom") +
  facet_wrap(~Focl_Ar, ncol=3)

Cairo(file="out/PP.value.threat.hist.PNG", type="png", width=4000, height=2400,pointsize=15,bg="white",dpi=300)
PP.value.threat.hist
dev.off()

####################################################################################

