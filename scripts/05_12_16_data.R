library(tidyverse)
library(ipumsr)

vuln_1216<-read_nhgis("data_raw/NHGIS/nhgis0017_ds225_20165_2016_blck_grp.csv") %>% 
  mutate(AREAKEY = paste0(STATEA, COUNTYA, TRACTA, BLKGRPA))

black<- vuln_1216 %>% 
  mutate(P_Black = AF2UE004/AF2UE001) %>% 
  select(AREAKEY, P_Black) 

hispanic<-vuln_1216 %>% 
  mutate(P_Hispanic = AF2UE012/ AF2UE001) %>% 
  select(AREAKEY, P_Hispanic)

aian <- vuln_1216 %>% 
  mutate(P_AIAN = AF2UE005/AF2UE001) %>% 
  select(AREAKEY, P_AIAN)

asian <- vuln_1216 %>% 
  mutate(P_ASIAN = AF2UE006/AF2UE001) %>% 
  select(AREAKEY, P_ASIAN)

nhpi <- vuln_1216 %>% 
  mutate(P_NHPI = AF2UE007/AF2UE001) %>% 
  select(AREAKEY, P_NHPI)

elderly <- vuln_1216 %>% 
  mutate(P_Elderly = (AF2AE020+AF2AE021+AF2AE022+AF2AE023+AF2AE024+AF2AE025+AF2AE044+AF2AE045+AF2AE046+AF2AE047+AF2AE048+AF2AE049)/AF2AE001) %>% 
  select(AREAKEY, P_Elderly)

english <- vuln_1216 %>% 
  mutate(P_LEP = (AF42E006+AF42E007+AF42E008+AF42E011+AF42E012+AF42E013+AF42E016+AF42E017+AF42E018+AF42E021+AF42E022+AF42E023+AF42E028+AF42E029+AF42E030+AF42E033+AF42E034+AF42E035+AF42E038+AF42E039+AF42E040+AF42E043+AF42E044+AF42E045+AF42E050+AF42E051+AF42E052+AF42E055+AF42E056+AF42E057+AF42E060+AF42E061+AF42E062+AF42E065+AF42E066+AF42E067)/AF42E001) %>% 
  select(AREAKEY, P_LEP)

# For Consistency, use B15002 for both ACS Years
education <- vuln_1216 %>% 
  mutate(P_HSGreater = (AF4NE003+AF4NE004+AF4NE005+AF4NE006+AF4NE007+AF4NE008+AF4NE009 +AF4NE010+AF4NE011+AF4NE020+AF4NE021+AF4NE022+AF4NE023+AF4NE024+AF4NE025+AF4NE026+AF4NE027+AF4NE028)/AF4NE001) %>% 
  select(AREAKEY, P_HSGreater) 

tenure <- vuln_1216 %>% 
  mutate(P_Renter = AF7PE003/AF7PE001) %>% 
  select(AREAKEY, P_Renter)

poverty<-vuln_1216 %>% 
  mutate(P_Poverty = AF46E002/AF46E001) %>% 
  select(AREAKEY, P_Poverty)

mhv <- vuln_1216 %>% 
  mutate(MHV = AF9LE001) %>% 
  select(AREAKEY, MHV)

mgr <-vuln_1216 %>% 
  mutate(MGR = AF89E001) %>% 
  select(AREAKEY, MGR)

# Fix This Need HUs for Denominator (B25001)
vacancy <- vuln_1216 %>% 
  mutate(P_Vacant = AF7ZE001/AF7NE001) %>% 
  select(AREAKEY, P_Vacant)

MHHI <- vuln_1216 %>% 
  mutate(MHHI = AF49E001) %>% 
  select(AREAKEY, MHHI)

single<- vuln_1216 %>% 
  mutate(P_Single = AF3JE004/AF3JE001) %>% 
  select(AREAKEY, P_Single)

cost_burden<-vuln_1216 %>% 
  mutate(
    P_RentCostBurden = (AF9FE007+AF9FE008+AF9FE009)/AF9FE001,
    P_Severe_RentCostBurden = AF9FE010/AF9FE001, 
    P_Own_Cost_Burden = (AF9XE008 + AF9XE009 + AF9XE010+AF9XE019+AF9XE020+AF9XE021)/AF9XE001,
    P_Severe_OwnCostBurden = (AF9XE011+AF9XE022)/AF9XE001
  ) %>% 
  select(AREAKEY, P_RentCostBurden, P_Own_Cost_Burden, P_Severe_RentCostBurden, P_Severe_OwnCostBurden)



var_list <- list(black, hispanic, aian, asian, nhpi, elderly, english, education, tenure, poverty, mhv, mgr, vacancy, MHHI, single, cost_burden)
VI_2016<-var_list %>%  reduce(left_join, by="AREAKEY")
VI_2016<-VI_2016 %>% mutate(S_Year = "2016")

rm(vuln_1216, var_list, black, hispanic, aian, asian, nhpi, elderly, english, education, tenure, poverty, mhv, mgr, vacancy, MHHI, single, cost_burden)
