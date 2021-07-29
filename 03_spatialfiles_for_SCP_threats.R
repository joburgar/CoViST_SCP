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

# create a conslidated Impact field where a range of impact values becomes the high end, very high becomes high, and unknown is the field if NA, unknown or not calculated
threats$Impact2 <- recode(threats$Impact, `High-Low`="High", `High-Medium`="High", `Medium-Low`="Medium", `Very High-High`="High", `Very High`="High", `Not Calculated` = "Unknown")
threats$Impact2 <- threats$Impact2 %>% replace_na("Unknown")

threats %>% count(Impact2)

threats %>% count(ELCODE)
nrow(threats)
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
