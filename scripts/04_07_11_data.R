library(tidyverse)
library(ipumsr)

vuln_0711<-read_nhgis("data_raw/NHGIS/nhgis0016_ds184_20115_2011_blck_grp.csv") %>% 
  mutate(AREAKEY = paste0(STATEA, COUNTYA, TRACTA, BLKGRPA))

IAF <- 1.069288956 #2011 CPI to 2016

black<- vuln_0711 %>% 
  mutate(P_Black = MN2E004/MN2E001) %>% 
  select(AREAKEY, P_Black) 

hispanic<-vuln_0711 %>% 
  mutate(P_Hispanic = MN2E012/ MN2E001) %>% 
  select(AREAKEY, P_Hispanic)

aian <- vuln_0711 %>% 
  mutate(P_AIAN = MN2E005/MN2E001) %>% 
  select(AREAKEY, P_AIAN)

asian <- vuln_0711 %>% 
  mutate(P_ASIAN = MN2E006/MN2E001) %>% 
  select(AREAKEY, P_ASIAN)

nhpi <- vuln_0711 %>% 
  mutate(P_NHPI = MN2E007/MN2E001) %>% 
  select(AREAKEY, P_NHPI)

elderly <- vuln_0711 %>% 
  mutate(P_Elderly = (MNIE020+MNIE021+MNIE022+MNIE023+MNIE024+MNIE025+MNIE044+MNIE045+MNIE046+MNIE047+MNIE048+MNIE049)/MNIE001) %>% 
  select(AREAKEY, P_Elderly)

english <- vuln_0711 %>% 
  mutate(P_LEP = (MPUE006+MPUE007+MPUE008+MPUE011+MPUE012+MPUE013+MPUE016+MPUE017+MPUE018+MPUE021+MPUE022+MPUE023+MPUE028+MPUE029+MPUE030+MPUE033+MPUE034+MPUE035+MPUE038+MPUE039+MPUE040+MPUE043+MPUE044+MPUE045+MPUE050+MPUE051+MPUE052+MPUE055+MPUE056+MPUE057+MPUE060+MPUE061+MPUE062+MPUE065+MPUE066+MPUE067)/MPUE001) %>% 
  select(AREAKEY, P_LEP)

education <- vuln_0711 %>% 
  mutate(P_HSGreater = (MPSE003+MPSE004+MPSE005+MPSE006+MPSE007+MPSE008+MPSE009 +MPSE010+MPSE011+MPSE020+MPSE021+MPSE022+MPSE023+MPSE024+MPSE025+MPSE026+MPSE027+MPSE028)/MPSE001) %>% 
  select(AREAKEY, P_HSGreater) 

tenure <- vuln_0711 %>% 
  mutate(P_Renter = MS4E003/MS4E001) %>% 
  select(AREAKEY, P_Renter)

poverty<-vuln_0711 %>% 
  mutate(P_Poverty = MPYE002/MPYE001) %>% 
  select(AREAKEY, P_Poverty)

mhv <- vuln_0711 %>% 
  mutate(MHV = MU2E001, MHV_IA = MHV*IAF) %>% 
  select(AREAKEY, MHV = MHV_IA)

mgr <-vuln_0711 %>% 
  mutate(MGR = MUPE001, MGR_IA = MGR*IAF) %>% 
  select(AREAKEY, MGR = MGR_IA)

# Fix This Need HUs for Denominator (B25001)
vacancy <- vuln_0711 %>% 
  mutate(P_Vacant = MTEE001/MS2E001) %>% 
  select(AREAKEY, P_Vacant)

MHHI <- vuln_0711 %>% 
  mutate(MHHI = MP1E001, MHHI_IA = MHHI*IAF) %>% 
  select(AREAKEY, MHHI = MHHI_IA)

single<- vuln_0711 %>% 
  mutate(P_Single = MOOE004/MOOE001) %>% 
  select(AREAKEY, P_Single)

cost_burden<-vuln_0711 %>% 
mutate(
  P_RentCostBurden = (MUVE007+MUVE008+MUVE009)/MUVE001,
  P_Severe_RentCostBurden = MUVE010/MUVE001, 
  P_Own_Cost_Burden = (MVEE008 + MVEE009 + MVEE010+MVEE019+MVEE020+MVEE021)/MVEE001,
  P_Severe_OwnCostBurden = (MVEE011+MVEE022)/MVEE001
) %>% 
  select(AREAKEY, P_RentCostBurden, P_Own_Cost_Burden, P_Severe_RentCostBurden, P_Severe_OwnCostBurden)
  
var_list <- list(black, hispanic, aian, asian, nhpi, elderly, english, education, tenure, poverty, mhv, mgr, vacancy, MHHI, cost_burden, single)
VI_2011<-var_list %>%  reduce(left_join, by="AREAKEY")

VI_2011<-VI_2011 %>% mutate(S_Year = "2011")

rm(IAF, vuln_0711, var_list, black, hispanic, aian, asian, nhpi, elderly, english, education, tenure, poverty, mhv, mgr, vacancy, MHHI, cost_burden, single)
