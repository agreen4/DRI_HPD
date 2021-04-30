library(readr)
library(tidyverse)
library(sf)

IAF <- 1.397390273 #2000 CPI to 2016

black<-read_csv("data_raw/Geolytics/BG_Pct_Black_2000.csv", col_types = cols(AREAKEY = col_character())) %>% 
  mutate(P_Black = P007004/P007001) %>% 
  select(AREAKEY, P_Black) 

hispanic<-read_csv("data_raw/Geolytics/BG_Pct_Hispanic_2000.csv", col_types = cols(AREAKEY = col_character())) %>% 
  mutate(P_Hispanic = P007010 / P007001) %>% 
  select(AREAKEY, P_Hispanic)

aian <- read_csv("data_raw/Geolytics/BG_Pct_AI_AN_2000.csv", col_types = cols(AREAKEY = col_character())) %>% 
  mutate(P_AIAN = P007005/P007001) %>% 
  select(AREAKEY, P_AIAN)

asian <- read_csv("data_raw/Geolytics/BG_Pct_Asian_2000.csv", col_types = cols(AREAKEY = col_character())) %>% 
  mutate(P_ASIAN = P007006/P007001) %>% 
  select(AREAKEY, P_ASIAN)

nhpi <- read_csv("data_raw/Geolytics/BG_Pct_NH_OPI_2000.csv", col_types = cols(AREAKEY = col_character())) %>% 
  mutate(P_NHPI = P007007/P007001) %>% 
  select(AREAKEY, P_NHPI)

elderly <- read_csv("data_raw/Geolytics/BG_Pct_Elderly_2000.csv", col_types = cols(AREAKEY = col_character())) %>% 
  mutate(P_Elderly = (P008035+P008036+P008037+P008038+P008039+P008040+P008074+P008075+P008076+P008077+P008078+P008079)/P008001) %>% 
  select(AREAKEY, P_Elderly)

english <- read_csv("data_raw/Geolytics/BG_Pct_Eltvw_2000.csv", col_types = cols(AREAKEY = col_character())) %>% 
  mutate(P_LEP = (P019006+P019007+P019008+P019011+P019012+P019013+P019016+P019017+P019018+P019021+P019022+P019023+P019028+P019029+P019030+P019033+P019034+P019035+P019038+P019039+P019040+P019043+P019044+P019045+P019050+P019051+P019052+P019055+P019056+P019057+P019060+P019061+P019062+P019065+P019066+P019067)/P019001) %>% 
  select(AREAKEY, P_LEP)

# This one has a problem with the areakey field. Assuming it is in the same order as others, we will replace with that from another data frame
education <- read_csv("data_raw/Geolytics/BG_Pct_HS_Diploma.csv", col_types = cols(AREAKEY = col_character())) %>% 
  mutate(P_HSGreater = (P037003+P037004+P037005+P037006+P037007+P037008+P037009 +P037010+P037011+P037020+P037021+P037022+P037023+P037024+P037025+P037026+P037027+P037028)/P037001) %>% 
  select(P_HSGreater) 
GEOID <- black$AREAKEY
education <- bind_cols(GEOID, education) %>% 
  select(AREAKEY = ...1, P_HSGreater)
  
tenure <- read_csv("data_raw/Geolytics/BG_Pct_Renter_2000.csv", col_types = cols(AREAKEY = col_character())) %>% 
  mutate(P_Renter = H007003/H007001) %>% 
  select(AREAKEY, P_Renter)

# Messed up FIPS Codes - fix by re-joining lat lon to atlanta bgs
bg_limits<-st_read("data_raw/Geography/BG_Atlanta_City_Limits.shp") %>% select(GEOID)

cost_burden<-read_csv("data_raw/Geolytics/BG_2000_CBH.csv")
cost_burden<-sf::st_as_sf(cost_burden, coords = c("INTPTLON10", "INTPTLAT10"))
st_crs(cost_burden)<-st_crs(bg_limits)
cost_burden<-st_filter(cost_burden, bg_limits, join=st_within())
cost_burden<-cost_burden %>% select(-AREAKEY, -STATEFP10, -ALAND10, -AWATER10)
cost_burden<-st_join(cost_burden, bg_limits) %>% st_set_geometry(NULL) %>% mutate(AREAKEY = GEOID)
temp<-readxl::read_xlsx("data_raw/Geolytics/BG_SCBH_2000.xlsx", col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric")) %>% select(-H094001, -H069001)
cost_burden<-left_join(cost_burden, temp, by="AREAKEY")
rm(temp)
cost_burden<-cost_burden%>% 
  mutate(P_RentCostBurden = (H069007+H069008+H069009)/H069001,
         P_Own_Cost_Burden = (H094008+H094009+H094010+H094019+H094020+H094021)/H094001,
         P_Severe_RentCostBurden =H069010/H069001, 
         P_Severe_OwnCostBurden = (H094011+H094022)/H094001) %>%
  select(AREAKEY, P_RentCostBurden, P_Own_Cost_Burden, P_Severe_RentCostBurden, P_Severe_OwnCostBurden)
  
poverty<-read_csv("data_raw/Geolytics/BG_Pct_BPvty_2000.csv", col_types = cols(AREAKEY = col_character())) %>% 
  mutate(P_Poverty = P087002/P087001) %>% 
  select(AREAKEY, P_Poverty)

mhv <- read_csv("data_raw/Geolytics/BG_Med_HValue_2000.csv", col_types = cols(AREAKEY = col_character())) %>%
  mutate(MHV = H085001, MHV_IA = MHV*IAF) %>% 
  select(AREAKEY, MHV = MHV_IA)

mgr <-read_csv("data_raw/Geolytics/BG_Med_Rent_2000.csv", col_types = cols(AREAKEY = col_character())) %>%
  mutate(MGR = H063001, MGR_IA = MGR*IAF) %>% 
  select(AREAKEY, MGR = MGR_IA)

vacancy <- read_csv("data_raw/Geolytics/BG_Pct_Vacant_2000.csv",col_types = cols(AREAKEY = col_character())) %>%
  mutate(P_Vacant = H006003/H006001) %>% 
  select(AREAKEY, P_Vacant)

MHHI <- read_csv("data_raw/Geolytics/BG_Med_HI_2000.csv",col_types = cols(AREAKEY = col_character())) %>%
  mutate(MHHI = P053001, MHHI_IA = MHHI*IAF) %>% 
  select(AREAKEY, MHHI = MHHI_IA)

Single<- readxl::read_xlsx("data_raw/Geolytics/BG_Single-Parent_2000.xlsx") %>% 
  mutate(AREAKEY = as.character(AREAKEY)) %>% 
  mutate(P_Single = (P017010+P017016)/P017001) %>% 
  select(AREAKEY, P_Single)

var_list <- list(black, hispanic, aian, asian, nhpi, elderly, english, education, tenure, poverty, mhv, mgr, vacancy, MHHI, Single, cost_burden)

VI_2000<-var_list %>%  reduce(left_join, by="AREAKEY")

VI_2000<-VI_2000 %>% mutate(S_Year = "2000")

rm(IAF, GEOID, var_list, black, hispanic, aian, asian, nhpi, elderly, english, education, tenure, poverty, mhv, mgr, vacancy, MHHI, Single, cost_burden, bg_limits)
