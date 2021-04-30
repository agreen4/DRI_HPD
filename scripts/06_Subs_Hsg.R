library(tidyverse)
library(sf)
library(ggplot2)
library(tigris)
library(ipumsr)
library(readxl)

NHPD <- read_xlsx("data_raw/NHPD/All Properties.xlsx") %>% 
  filter(CountyCode %in% c("13089", "13121"))
NHPD<-NHPD %>% filter(LatestEndDate > "2000-01-01")
test<-st_as_sf(NHPD, coords = c("Longitude", "Latitude"))
test<-st_set_crs(test, 4269)

bg_limits<-st_read("data_raw/Geography/BG_Atlanta_City_Limits.shp") %>% select(GEOID)

test<-st_join(test, bg_limits)
test<-test %>% filter(!is.na(GEOID)) %>% select(GEOID, TotalUnits, EarliestStartDate, EarliestEndDate, LatestEndDate)
#ggplot()+geom_sf(data=bg_limits)+geom_sf(data=test, size = .5)

subsid96<-test %>% filter(EarliestStartDate <= "1996-01-01", EarliestEndDate >= "1996-01-01") %>% st_set_geometry(NULL) %>% select(GEOID, AHU_96 = TotalUnits) %>% group_by(GEOID) %>% summarise(AHU_96 = sum(AHU_96, na.rm=TRUE))
subsid00<-test %>% filter(EarliestStartDate <= "2000-01-01", EarliestEndDate >= "2000-01-01") %>% st_set_geometry(NULL)%>% select(GEOID, AHU_00 = TotalUnits) %>% group_by(GEOID) %>% summarise(AHU_00 = sum(AHU_00, na.rm=TRUE))
subsid07<-test %>% filter(EarliestStartDate <= "2007-01-01", EarliestEndDate >= "2007-01-01") %>% st_set_geometry(NULL)%>% select(GEOID, AHU_07 = TotalUnits) %>% group_by(GEOID) %>% summarise(AHU_07 = sum(AHU_07, na.rm=TRUE))
subsid11<-test %>% filter(EarliestStartDate <= "2011-01-01", EarliestEndDate >= "2011-01-01") %>% st_set_geometry(NULL)%>% select(GEOID, AHU_11 = TotalUnits) %>% group_by(GEOID) %>% summarise(AHU_11 = sum(AHU_11, na.rm=TRUE))
subsid12<-test %>% filter(EarliestStartDate <= "2011-01-01", EarliestEndDate >= "2011-01-01") %>% st_set_geometry(NULL)%>% select(GEOID, AHU_12 = TotalUnits) %>% group_by(GEOID) %>% summarise(AHU_12 = sum(AHU_12, na.rm=TRUE))
subsid16<-test %>% filter(EarliestStartDate <= "2012-01-01", EarliestEndDate >= "2012-01-01") %>% st_set_geometry(NULL)%>% select(GEOID, AHU_16 = TotalUnits) %>% group_by(GEOID) %>% summarise(AHU_16 = sum(AHU_16, na.rm=TRUE))

expire00<-test %>% filter(EarliestStartDate <= "2000-01-01", LatestEndDate <= "2005-01-01") %>% st_set_geometry(NULL) %>% select(GEOID, Expire_00 = TotalUnits) %>% group_by(GEOID) %>% summarise(Expire_00 = sum(Expire_00, na.rm=TRUE))
expire11<-test %>% filter(EarliestStartDate <= "2011-01-01", LatestEndDate <= "2016-01-01") %>% st_set_geometry(NULL) %>% select(GEOID, Expire_11 = TotalUnits) %>% group_by(GEOID) %>% summarise(Expire_11 = sum(Expire_11, na.rm=TRUE))
expire16<-test %>% filter(EarliestStartDate <= "2016-01-01", LatestEndDate <= "2021-01-01") %>% st_set_geometry(NULL) %>% select(GEOID, Expire_16 = TotalUnits) %>% group_by(GEOID) %>% summarise(Expire_16 = sum(Expire_16, na.rm=TRUE))




var_list <- list(subsid96, subsid00, subsid07, subsid11, subsid12, subsid16, expire00, expire11, expire16)

AH_All<-var_list %>%  reduce(full_join, by="GEOID")

AH_All<-left_join(bg_limits, AH_All, by="GEOID")

AH_All<-AH_All %>% replace(is.na(.), 0)

HU_00 <- read_csv("data_raw/Geolytics/BG_Pct_Renter_2000.csv", col_types = cols(AREAKEY = col_character())) %>% 
  mutate(HUs = H007001) %>% 
  select(AREAKEY, HU_00 = HUs)

HU_07_11 <-read_nhgis("data_raw/NHGIS/nhgis0016_ds184_20115_2011_blck_grp.csv") %>% 
  mutate(AREAKEY = paste0(STATEA, COUNTYA, TRACTA, BLKGRPA)) %>% 
  mutate(HU_07_11 = MS2E001) %>% 
  select(AREAKEY, HU_07_11)

HU_12_16 <-read_nhgis("data_raw/NHGIS/nhgis0017_ds225_20165_2016_blck_grp.csv") %>% 
  mutate(AREAKEY = paste0(STATEA, COUNTYA, TRACTA, BLKGRPA)) %>% 
  mutate(HU_12_16 = AF7PE001) %>% 
  select(AREAKEY, HU_12_16)

HUs<-left_join(HU_00, HU_07_11)
HUs<-left_join(HUs, HU_12_16)

AH_All<-left_join(AH_All, HUs, by=c("GEOID" = "AREAKEY"))
AH_All<-AH_All %>% mutate(
  PSU_00 = AHU_00 / HU_00,
  PSU_11 = AHU_11 / HU_07_11,
  PSU_16 = AHU_16 / HU_12_16,
  CHU_00 = (AHU_00-AHU_96)/HU_00,
  CHU_11 = (AHU_11-AHU_07)/HU_07_11,
  CHU_16 = (AHU_16-AHU_12)/HU_12_16,
  P_Expire_00 = Expire_00/AHU_00,
  P_Expire_11 = Expire_11/AHU_11,
  P_Expire_16 = Expire_16/AHU_16)

AH_All<-AH_All %>% select(GEOID, PSU_00, PSU_11, PSU_16, CHU_00, CHU_11, CHU_16, P_Expire_00, P_Expire_11, P_Expire_16)

AH_00<-AH_All %>% select(GEOID, PSU = PSU_00, CHU = CHU_00, P_Expire = P_Expire_00)%>% mutate_at(vars(PSU, CHU, P_Expire), ~replace(., is.nan(.), 0))
AH_11<-AH_All %>% select(GEOID, PSU = PSU_11, CHU = CHU_11, P_Expire = P_Expire_11)%>% mutate_at(vars(PSU, CHU, P_Expire), ~replace(., is.nan(.), 0))
AH_16<-AH_All %>% select(GEOID, PSU = PSU_16, CHU = CHU_16, P_Expire = P_Expire_16)%>% mutate_at(vars(PSU, CHU, P_Expire), ~replace(., is.nan(.), 0))

AH_00<-AH_00 %>% mutate_at(vars(P_Expire), ~replace(., !is.finite(.), NA))
AH_11<-AH_11 %>% mutate(P_Expire)%>% mutate_at(vars(P_Expire), ~replace(., !is.finite(.), NA))
AH_16<-AH_16 %>% mutate(P_Expire)%>% mutate_at(vars(P_Expire), ~replace(., !is.finite(.), NA))

rm(AH_All, HU_00, HU_07_11, HU_12_16, HUs, NHPD, expire00, expire11, expire16, subsid96, subsid00, subsid07, subsid11, subsid12, subsid16, test, var_list)




crime_00<-read_csv("data_raw/Crime/Crime Data_2000_Merged.csv")
crime_00<-st_as_sf(crime_00, coords = c("Longitude", "Latitude"))
crime_00<-st_set_crs(crime_00, 4269)
crime_00<-st_join(crime_00, bg_limits)
crime_00<-crime_00 %>% st_set_geometry(NULL)
crime_00<-crime_00 %>% group_by(GEOID) %>% summarise(crime_00 = n())
crime_00<-crime_00 %>% filter(!is.na(GEOID))

crime_11<-read_csv("data_raw/Crime/Crime Data_2011_21821_Lat_Long.csv")
crime_11<-st_as_sf(crime_11, coords = c("Longitude", "Latitude"))
crime_11<-st_set_crs(crime_11, 4269)
crime_11<-st_join(crime_11, bg_limits)
crime_11<-crime_11 %>% st_set_geometry(NULL)
crime_11<-crime_11 %>% group_by(GEOID) %>% summarise(crime_11 = n())
crime_11<-crime_11 %>% filter(!is.na(GEOID))

crime_16<-read_csv("data_raw/Crime/Crime Data_2016.csv")
crime_16<-crime_16 %>% filter(!is.na(Latitude), !is.na(Longitude))
crime_16<-st_as_sf(crime_16, coords = c("Longitude", "Latitude"))
crime_16<-st_set_crs(crime_16, 4269)
crime_16<-st_join(crime_16, bg_limits)
crime_16<-crime_16 %>% st_set_geometry(NULL)
crime_16<-crime_16 %>% group_by(GEOID) %>% summarise(crime_16 = n())
crime_16<-crime_16 %>% filter(!is.na(GEOID))

var_list <- list(crime_00, crime_11, crime_16)

crime_all<-var_list %>%  reduce(full_join, by="GEOID")

crime_all<-left_join(bg_limits, crime_all)
crime_all<-crime_all %>% replace(is.na(.), 0)

pop_00<-read_csv("data_raw/Geolytics/BG_Pct_Black_2000.csv", col_types = cols(AREAKEY = col_character())) %>% 
  mutate(pop_00 = P007001) %>% 
  select(AREAKEY, pop_00) 

pop_11<-read_nhgis("data_raw/NHGIS/nhgis0016_ds184_20115_2011_blck_grp.csv") %>% 
  mutate(AREAKEY = paste0(STATEA, COUNTYA, TRACTA, BLKGRPA)) %>% 
  mutate(pop_11 = MN2E001) %>% 
  select(AREAKEY, pop_11) 

pop_16<-read_nhgis("data_raw/NHGIS/nhgis0017_ds225_20165_2016_blck_grp.csv") %>% 
  mutate(AREAKEY = paste0(STATEA, COUNTYA, TRACTA, BLKGRPA)) %>% 
  mutate(pop_16 = AF2UE001) %>% 
  select(AREAKEY, pop_16)

crime_all<-left_join(crime_all, pop_00, by=c("GEOID" = "AREAKEY"))
crime_all<-left_join(crime_all, pop_11, by=c("GEOID" = "AREAKEY"))
crime_all<-left_join(crime_all, pop_16, by=c("GEOID" = "AREAKEY"))
crime_all<-crime_all %>% st_set_geometry(NULL)
crime_all<-crime_all %>% mutate(R_Crime_00 = crime_00/pop_00,
                                R_Crime_11 = crime_11/pop_11,
                                R_Crime_16 = crime_16/pop_16)
crime_all<-crime_all %>% select(GEOID, R_Crime_00, R_Crime_11, R_Crime_16) %>% 
  mutate_at(vars(R_Crime_00, R_Crime_11, R_Crime_16), ~replace(., is.nan(.), 0)) %>%  
  mutate(R_Crime_16 = ifelse(!is.finite(R_Crime_16), NA, R_Crime_16))

crime_00<-crime_all %>% select(GEOID, R_Crime = R_Crime_00)
crime_11<-crime_all %>% select(GEOID, R_Crime = R_Crime_11)
crime_16<-crime_all %>% select(GEOID, R_Crime = R_Crime_16)
rm(pop_00, pop_11, pop_16, var_list, crime_all)


evictions<-read_csv("data_raw/Evictions/Georgia Block Group Eviction Rate and Eviction Filing Rates.csv", col_types = cols(GEOID = col_character()))
evict_00<-evictions %>% filter(year == 2000) %>% select(GEOID, R_Evict = `eviction-rate`, R_File = `eviction-filing-rate`)
evict_11<-evictions %>% filter(year == 2011) %>% select(GEOID, R_Evict = `eviction-rate`, R_File = `eviction-filing-rate`)
evict_16<-evictions %>% filter(year == 2016) %>% select(GEOID, R_Evict = `eviction-rate`, R_File = `eviction-filing-rate`)
rm(evictions)


# Low Income Area Next to High-Income Area
bg_limits<-st_read("data_raw/Geography/BG_Atlanta_City_Limits.shp") %>% select(GEOID)

IAF <- 1.397390273 #2000 CPI to 2016
MHHI_00 <- read_csv("data_raw/Geolytics/BG_Med_HI_2000.csv",col_types = cols(AREAKEY = col_character())) %>%
  mutate(MHHI = P053001, MHHI_IA = MHHI*IAF) %>% 
  select(AREAKEY, MHHI = MHHI_IA)

IAF <- 1.069288956 #2011 CPI to 2016
vuln_0711<-read_nhgis("data_raw/NHGIS/nhgis0016_ds184_20115_2011_blck_grp.csv") %>% 
  mutate(AREAKEY = paste0(STATEA, COUNTYA, TRACTA, BLKGRPA))

MHHI_07_11 <- vuln_0711 %>% 
  mutate(MHHI = MP1E001, MHHI_IA = MHHI*IAF) %>% 
  select(AREAKEY, MHHI = MHHI_IA)

vuln_1216<-read_nhgis("data_raw/NHGIS/nhgis0017_ds225_20165_2016_blck_grp.csv") %>% 
  mutate(AREAKEY = paste0(STATEA, COUNTYA, TRACTA, BLKGRPA))

MHHI_12_16 <- vuln_1216 %>% 
  mutate(MHHI = AF49E001) %>% 
  select(AREAKEY, MHHI)

rm(vuln_0711, vuln_1216, IAF)

MHHI_00<-left_join(bg_limits, MHHI_00, by=c("GEOID" = "AREAKEY"))
MHHI_07_11<-left_join(bg_limits, MHHI_07_11, by=c("GEOID" = "AREAKEY"))
MHHI_12_16<-left_join(bg_limits, MHHI_12_16, by=c("GEOID" = "AREAKEY"))

MHHI_00<-MHHI_00 %>% mutate(MHHI_Q = ntile(MHHI, 5))
MHHI_07_11<-MHHI_07_11 %>% mutate(MHHI_Q = ntile(MHHI, 5))
MHHI_12_16<-MHHI_12_16 %>% mutate(MHHI_Q = ntile(MHHI, 5))

test<-spdep::poly2nb(MHHI_00, row.names = "GEOID")

test<-spdep::nb2mat(test, zero.policy=TRUE, style = "B")
rownames(test)<-MHHI_00$GEOID
colnames(test)<-MHHI_00$GEOID

test<-reshape2::melt(test) %>% 
  filter(value == 1, Var1 != Var2) %>% 
  arrange(Var1, Var2) %>% 
  select(Var1, Var2) %>% 
  mutate(Var1 = as.character(Var1), Var2 = as.character(Var2))

test<-left_join(test, MHHI_00 %>% select(GEOID, MHHI_Q) %>% st_set_geometry(NULL), by=c("Var1" = "GEOID"))
test<-left_join(test, MHHI_00 %>% select(GEOID, MHHI_Q) %>% st_set_geometry(NULL), by=c("Var2" = "GEOID"))

test<-test %>% 
  mutate(LIC = ifelse(MHHI_Q.x <= 2 & MHHI_Q.y >= 4, 1, 0))
test<-test %>% mutate(LIC = replace_na(LIC, 0))
test<-test %>% 
  group_by(Var1) %>% 
  summarise(LIC = sum(LIC)) %>% 
  mutate(LIC = ifelse(LIC >= 1, 1, 0))

MHHI_00<-left_join(MHHI_00, test, by=c("GEOID" = "Var1"))
rm(test)

test<-spdep::poly2nb(MHHI_07_11, row.names = "GEOID")

test<-spdep::nb2mat(test, zero.policy=TRUE, style = "B")
rownames(test)<-MHHI_07_11$GEOID
colnames(test)<-MHHI_07_11$GEOID

test<-reshape2::melt(test) %>% 
  filter(value == 1, Var1 != Var2) %>% 
  arrange(Var1, Var2) %>% 
  select(Var1, Var2) %>% 
  mutate(Var1 = as.character(Var1), Var2 = as.character(Var2))

test<-left_join(test, MHHI_07_11 %>% select(GEOID, MHHI_Q) %>% st_set_geometry(NULL), by=c("Var1" = "GEOID"))
test<-left_join(test, MHHI_07_11 %>% select(GEOID, MHHI_Q) %>% st_set_geometry(NULL), by=c("Var2" = "GEOID"))

test<-test %>% 
  mutate(LIC = ifelse(MHHI_Q.x <= 2 & MHHI_Q.y >= 4, 1, 0))
test<-test %>% mutate(LIC = replace_na(LIC, 0))
test<-test %>% 
  group_by(Var1) %>% 
  summarise(LIC = sum(LIC)) %>% 
  mutate(LIC = ifelse(LIC >= 1, 1, 0))

MHHI_07_11<-left_join(MHHI_07_11, test, by=c("GEOID" = "Var1"))
rm(test)

test<-spdep::poly2nb(MHHI_12_16, row.names = "GEOID")
#summary.nb(test)

test<-spdep::nb2mat(test, zero.policy=TRUE, style = "B")
rownames(test)<-MHHI_12_16$GEOID
colnames(test)<-MHHI_12_16$GEOID

test<-reshape2::melt(test) %>% 
  filter(value == 1, Var1 != Var2) %>% 
  arrange(Var1, Var2) %>% 
  select(Var1, Var2) %>% 
  mutate(Var1 = as.character(Var1), Var2 = as.character(Var2))

test<-left_join(test, MHHI_12_16 %>% select(GEOID, MHHI_Q) %>% st_set_geometry(NULL), by=c("Var1" = "GEOID"))
test<-left_join(test, MHHI_12_16 %>% select(GEOID, MHHI_Q) %>% st_set_geometry(NULL), by=c("Var2" = "GEOID"))

test<-test %>% 
  mutate(LIC = ifelse(MHHI_Q.x <= 2 & MHHI_Q.y >= 4, 1, 0))

test<-test %>% mutate(LIC = replace_na(LIC, 0))

test<-test %>% 
  group_by(Var1) %>% 
  summarise(LIC = sum(LIC)) %>% 
  mutate(LIC = ifelse(LIC >= 1, 1, 0))

MHHI_12_16<-left_join(MHHI_12_16, test, by=c("GEOID" = "Var1"))
rm(test)


MHHI_00<-MHHI_00 %>% select(GEOID, LIC) %>% st_set_geometry(NULL)
MHHI_07_11<-MHHI_07_11 %>% select(GEOID, LIC) %>% st_set_geometry(NULL)
MHHI_12_16<-MHHI_12_16 %>% select(GEOID, LIC) %>% st_set_geometry(NULL)
