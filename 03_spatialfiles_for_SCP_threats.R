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
list.of.packages <- c("tidyverse", "lubridate","chron","bcdata", "bcmaps","sf", "rgdal", "readxl", "Cairo", "OpenStreetMap", "ggmap")
# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
#####################################################################################

GISDir <- "//spatialfiles.bcgov/work/wlap/sry/Workarea/jburgar/CoViST"

# load relevant spatial files
aoi <- st_read(dsn=GISDir, layer="CoViST_Study_Boundary_June2021") # updated to reflect decision at June 22 STSA meeting
aoi.CH.EO.250m <- st_read(dsn=paste(getwd(),"/out", sep=""), layer="aoi.CH.EO.250m")
FA_SpRich <- st_read(dsn=paste(getwd(),"/out", sep=""), layer="Focal_Area")


# load attribute table
threats <- read_excel("data/threats_list_May-06-21.xlsx",sheet = 2, trim_ws = TRUE, col_types = c("text")) %>% type.convert()
threats %>% group_by(Species_Common, Severity, Impact) %>% count(threat_desc, sub_threat_desc)
threats %>% count(ELCODE)
nrow(threats)
nrow(threats %>% filter(!is.na(sub_threat_desc)))
threats_sub <- threats %>% filter(!is.na(sub_threat_desc))
threats_sub %>% count(Impact)
threats_sub %>% group_by(Species_Common, Severity, Impact) %>% count(threat_desc)
as.data.frame(threats_sub %>% filter(Impact!="Low") %>% group_by(Species_Common) %>% count(threat_desc))

