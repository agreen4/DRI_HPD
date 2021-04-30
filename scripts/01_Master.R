library(pacman)
p_load(tidyverse, tigris,sf, scales, ggalluvial, ggthemes, ggpubr)
#census_api_key("6ed16ee8bb787c547084eaa835a2ccfba8abe586")

# Load and Prepare Data ----

source("scripts/02_GEOS.R")
source("scripts/03_2000_data.R")
source("scripts/04_07_11_data.R")
source("scripts/05_12_16_data.R")
source("scripts/06_Subs_Hsg.R")
source("scripts/07_Schools.R")
source("scripts/02_GEOS.R")

# Filter Down to Atlanta Block Groups
VI_2000<-left_join(bg_limits, VI_2000, by=c("GEOID" = "AREAKEY"))%>% st_set_geometry(NULL)
VI_2011<-left_join(bg_limits, VI_2011, by=c("GEOID"="AREAKEY"))%>% st_set_geometry(NULL)
VI_2016<-left_join(bg_limits, VI_2016, by=c("GEOID"="AREAKEY"))%>% st_set_geometry(NULL)

# Join the other datasets
dataset_00<-left_join(VI_2000, AH_00, by="GEOID")
dataset_00<-left_join(dataset_00, MHHI_00, by="GEOID")
dataset_00<-left_join(dataset_00, schools_00, by="GEOID")
dataset_00<-left_join(dataset_00, crime_00, by="GEOID")
dataset_00<-left_join(dataset_00, evict_00, by="GEOID")

dataset_11<-left_join(VI_2011, AH_11, by="GEOID")
dataset_11<-left_join(dataset_11, MHHI_07_11, by="GEOID")
dataset_11<-left_join(dataset_11, schools_11, by="GEOID")
dataset_11<-left_join(dataset_11, crime_11, by="GEOID")
dataset_11<-left_join(dataset_11, evict_11, by="GEOID")


dataset_16<-left_join(VI_2016, AH_16, by="GEOID")
dataset_16<-left_join(dataset_16, MHHI_12_16, by="GEOID")
dataset_16<-left_join(dataset_16, schools_16, by="GEOID")
dataset_16<-left_join(dataset_16, crime_16, by="GEOID")
dataset_16<-left_join(dataset_16, evict_16, by="GEOID")


# Replace any missing (NA) values with the mean
dataset_00<- dataset_00 %>% 
  mutate_all(funs(ifelse(is.na(.), mean(., na.rm = TRUE), .)))
dataset_11<- dataset_11 %>% 
  mutate_all(funs(ifelse(is.na(.), mean(., na.rm = TRUE), .)))
dataset_16<- dataset_16 %>% 
  mutate_all(funs(ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Data Descriptives ----
# Vulnerability Descriptives
dataset_00 %>% summarise_at(vars(P_Black, P_Hispanic, P_AIAN, P_ASIAN, P_NHPI, P_Elderly, P_Single, P_LEP, P_HSGreater, P_Renter, P_RentCostBurden, P_Own_Cost_Burden, P_Severe_RentCostBurden, P_Severe_OwnCostBurden, P_Poverty, MHHI), mean, na.rm=TRUE) %>% View()
dataset_11 %>% summarise_at(vars(P_Black, P_Hispanic, P_AIAN, P_ASIAN, P_NHPI, P_Elderly, P_Single, P_LEP, P_HSGreater, P_Renter, P_RentCostBurden, P_Own_Cost_Burden, P_Severe_RentCostBurden, P_Severe_OwnCostBurden, P_Poverty, MHHI), mean, na.rm=TRUE) %>% View()
dataset_16 %>% summarise_at(vars(P_Black, P_Hispanic, P_AIAN, P_ASIAN, P_NHPI, P_Elderly, P_Single, P_LEP, P_HSGreater, P_Renter, P_RentCostBurden, P_Own_Cost_Burden, P_Severe_RentCostBurden, P_Severe_OwnCostBurden, P_Poverty, MHHI), mean, na.rm=TRUE) %>% View()

# Housing Market Descriptives
dataset_00 %>% summarise_at(vars(PSU, CHU,P_Expire, MHV, MGR, R_Crime, R_Evict, R_File, Eligible_FR, P_Vacant, LIC), mean, na.rm=TRUE) %>% View()
dataset_11 %>% summarise_at(vars(PSU, CHU,P_Expire, MHV, MGR, R_Crime, R_Evict, R_File, Eligible_FR, P_Vacant, LIC), mean, na.rm=TRUE) %>% View()
dataset_16 %>% summarise_at(vars(PSU, CHU,P_Expire, MHV, MGR, R_Crime, R_Evict, R_File, Eligible_FR, P_Vacant, LIC), mean, na.rm=TRUE) %>% View()

# Vulnerability Descriptives (Belt)
dataset_00 %>% group_by(belt_flag)%>% summarise_at(vars(P_Black, P_Hispanic, P_AIAN, P_ASIAN, P_NHPI, P_Elderly, P_Single, P_LEP, P_HSGreater,P_Renter, P_RentCostBurden, P_Own_Cost_Burden, P_Severe_RentCostBurden, P_Severe_OwnCostBurden, P_Poverty, MHHI), mean, na.rm=TRUE) %>% View()
dataset_11 %>% group_by(belt_flag) %>% summarise_at(vars(P_Black, P_Hispanic, P_AIAN, P_ASIAN, P_NHPI, P_Elderly, P_Single, P_LEP, P_HSGreater,P_Renter, P_RentCostBurden, P_Own_Cost_Burden, P_Severe_RentCostBurden, P_Severe_OwnCostBurden, P_Poverty, MHHI), mean, na.rm=TRUE)  %>% View()
dataset_16 %>% group_by(belt_flag) %>% summarise_at(vars(P_Black, P_Hispanic, P_AIAN, P_ASIAN, P_NHPI, P_Elderly, P_Single, P_LEP, P_HSGreater,P_Renter, P_RentCostBurden, P_Own_Cost_Burden, P_Severe_RentCostBurden, P_Severe_OwnCostBurden, P_Poverty, MHHI), mean, na.rm=TRUE) %>% View()

# Housing Market Descriptives (Belt)
dataset_00 %>% group_by(belt_flag) %>% summarise_at(vars(PSU, CHU, P_Expire, MHV, MGR, R_Crime, R_Evict, R_File, Eligible_FR, P_Vacant, LIC), mean, na.rm=TRUE) %>% View()
dataset_11 %>% group_by(belt_flag)%>% summarise_at(vars(PSU, CHU, P_Expire, MHV, MGR, R_Crime, R_Evict, R_File, Eligible_FR, P_Vacant, LIC), mean, na.rm=TRUE)  %>% View()
dataset_16 %>% group_by(belt_flag)%>% summarise_at(vars(PSU, CHU, P_Expire, MHV, MGR, R_Crime, R_Evict, R_File, Eligible_FR, P_Vacant, LIC), mean, na.rm=TRUE)  %>% View()

# Variable Standardization and Scaling ----

# Z_Scores
dataset_00<-dataset_00 %>% mutate_at(vars(P_Black:R_File, -S_Year, -geometry), scale)
dataset_11<-dataset_11 %>% mutate_at(vars(P_Black:R_File, -S_Year, -geometry), scale)
dataset_16<-dataset_16 %>% mutate_at(vars(P_Black:R_File, -S_Year, -geometry), scale)

# Elderly +
# LEP +
# HSGreater +
# Renter +
# Poverty +
# MHV -
# MGR -
# P_Vacant +
# MHHI -

# Rescale 0-100

dataset_00<-dataset_00 %>% 
  mutate_at(vars(P_Black:P_Elderly, P_LEP, P_HSGreater, P_Renter, P_Poverty, P_Vacant, P_Single, P_RentCostBurden, P_Own_Cost_Burden, P_Severe_RentCostBurden, P_Severe_OwnCostBurden, LIC, Eligible_F, Eligible_R, R_Crime, R_Evict, R_File, P_Expire), rescale, to = c(0, 100)) %>% 
  mutate_at(vars(MHV, MGR, MHHI, CHU), rescale, to = c(100, 0)) %>%
  mutate(Index_Vuln = P_Black+ P_Hispanic+ P_AIAN+ P_ASIAN+ P_NHPI+ P_Elderly+ P_Single+ P_LEP+ P_Renter+ P_RentCostBurden+ P_Own_Cost_Burden+ P_Severe_RentCostBurden+ P_Severe_OwnCostBurden+ P_Poverty+ MHHI,
         Index_Housing = P_Expire+ CHU+ MHV+ MGR+ R_Crime+ R_Evict+ R_File+ Eligible_FR+ P_Vacant+ LIC+MHHI) %>% 
  mutate(Index_Q_Vuln = ntile(Index_Vuln, 4) %>% as.character(),
         Index_Q_Housing = ntile(Index_Housing, 4) %>% as.character()) %>% 
  mutate(Vuln_Cat = case_when(Index_Q_Vuln == "1" ~ "Low Vulnerability",
            Index_Q_Vuln %in% c("2", "3") ~ "Moderate Vulnerability",
            Index_Q_Vuln == "4" ~ "High Vulnerability"),
         House_Cat = case_when(Index_Q_Housing == "1" ~ "Low Vulnerability",
                               Index_Q_Housing %in% c("2", "3") ~ "Moderate Vulnerability",
                               Index_Q_Housing == "4" ~ "High Vulnerability")) %>% 
  mutate(Vuln_Cat = factor(Vuln_Cat, levels = c("High Vulnerability", "Moderate Vulnerability", "Low Vulnerability")),
         House_Cat = factor(House_Cat, levels = c("High Vulnerability", "Moderate Vulnerability", "Low Vulnerability")))

dataset_11<-dataset_11 %>% 
  mutate_at(vars(P_Black:P_Elderly, P_LEP, P_HSGreater, P_Renter, P_Poverty, P_Vacant, P_Single, P_RentCostBurden, P_Own_Cost_Burden, P_Severe_RentCostBurden, P_Severe_OwnCostBurden, LIC, Eligible_F, Eligible_R, R_Crime, R_Evict, R_File, P_Expire), rescale, to = c(0, 100)) %>% 
  mutate_at(vars(MHV, MGR, MHHI, CHU), rescale, to = c(100, 0)) %>%
  mutate(Index_Vuln = P_Black+ P_Hispanic+ P_AIAN+ P_ASIAN+ P_NHPI+ P_Elderly+ P_Single+ P_LEP+ P_Renter+ P_RentCostBurden+ P_Own_Cost_Burden+ P_Severe_RentCostBurden+ P_Severe_OwnCostBurden+ P_Poverty+ MHHI,
         Index_Housing = P_Expire+ CHU+ MHV+ MGR+ R_Crime+ R_Evict+ R_File+ Eligible_FR+ P_Vacant+ LIC+MHHI) %>% 
  mutate(Index_Q_Vuln = ntile(Index_Vuln, 4) %>% as.character(),
         Index_Q_Housing = ntile(Index_Housing, 4) %>% as.character()) %>% 
  mutate(Vuln_Cat = case_when(Index_Q_Vuln == "1" ~ "Low Vulnerability",
                              Index_Q_Vuln %in% c("2", "3") ~ "Moderate Vulnerability",
                              Index_Q_Vuln == "4" ~ "High Vulnerability"),
         House_Cat = case_when(Index_Q_Housing == "1" ~ "Low Vulnerability",
                               Index_Q_Housing %in% c("2", "3") ~ "Moderate Vulnerability",
                               Index_Q_Housing == "4" ~ "High Vulnerability")) %>% 
  mutate(Vuln_Cat = factor(Vuln_Cat, levels = c("High Vulnerability", "Moderate Vulnerability", "Low Vulnerability")),
         House_Cat = factor(House_Cat, levels = c("High Vulnerability", "Moderate Vulnerability", "Low Vulnerability")))


dataset_16<-dataset_16 %>% 
  mutate_at(vars(P_Black:P_Elderly, P_LEP, P_HSGreater, P_Renter, P_Poverty, P_Vacant, P_Single, P_RentCostBurden, P_Own_Cost_Burden, P_Severe_RentCostBurden, P_Severe_OwnCostBurden, LIC, Eligible_F, Eligible_R, R_Crime, R_Evict, R_File, P_Expire), rescale, to = c(0, 100)) %>% 
  mutate_at(vars(MHV, MGR, MHHI, CHU), rescale, to = c(100, 0)) %>%
  mutate(Index_Vuln = P_Black+ P_Hispanic+ P_AIAN+ P_ASIAN+ P_NHPI+ P_Elderly+ P_Single+ P_LEP+ P_Renter+ P_RentCostBurden+ P_Own_Cost_Burden+ P_Severe_RentCostBurden+ P_Severe_OwnCostBurden+ P_Poverty+ MHHI,
         Index_Housing = P_Expire+ CHU+ MHV+ MGR+ R_Crime+ R_Evict+ R_File+ Eligible_FR+ P_Vacant+ LIC+MHHI) %>% 
  mutate(Index_Q_Vuln = ntile(Index_Vuln, 4) %>% as.character(),
         Index_Q_Housing = ntile(Index_Housing, 4) %>% as.character()) %>% 
  mutate(Vuln_Cat = case_when(Index_Q_Vuln == "1" ~ "Low Vulnerability",
                              Index_Q_Vuln %in% c("2", "3") ~ "Moderate Vulnerability",
                              Index_Q_Vuln == "4" ~ "High Vulnerability"),
         House_Cat = case_when(Index_Q_Housing == "1" ~ "Low Vulnerability",
                               Index_Q_Housing %in% c("2", "3") ~ "Moderate Vulnerability",
                               Index_Q_Housing == "4" ~ "High Vulnerability")) %>% 
  mutate(Vuln_Cat = factor(Vuln_Cat, levels = c("High Vulnerability", "Moderate Vulnerability", "Low Vulnerability")),
         House_Cat = factor(House_Cat, levels = c("High Vulnerability", "Moderate Vulnerability", "Low Vulnerability")))

dataset_00<-dataset_00 %>% mutate(belt_flag = case_when(belt_flag == "Beltline" ~ "BeltLine", belt_flag == "Not Beltline" ~ "Not BeltLine"))
dataset_11<-dataset_11 %>% mutate(belt_flag = case_when(belt_flag == "Beltline" ~ "BeltLine", belt_flag == "Not Beltline" ~ "Not BeltLine"))
dataset_16<-dataset_16 %>% mutate(belt_flag = case_when(belt_flag == "Beltline" ~ "BeltLine", belt_flag == "Not Beltline" ~ "Not BeltLine"))

# Locator Map ----
beltline<-st_read("data_raw/Beltline/subareas_shapefiles/beltline_subareas.shp")

Locator_1<-ggplot()+
  geom_sf(data=bg_limits  %>% st_union(), color = "gray40", fill=NA)+
  geom_sf(data=beltline %>% st_union(), fill = "gray60", colour = "gray30")+
  coord_sf(datum=NA)+
  theme_minimal()+
  theme(legend.position = "none")

Locator_2<-ggplot()+
  geom_sf(data=bg_limits %>% group_by(belt_flag) %>% summarise(n()), color = "gray40", fill=NA)+
  geom_sf(data=bg_limits %>% group_by(belt_flag) %>% summarise(n()) %>% filter(belt_flag == "Beltline"), fill = "gray60", colour = "gray30")+
  geom_sf(data=bg_limits, color = "gray80", lwd = .15, fill=NA)+
  coord_sf(datum=NA)+
  theme_minimal()+
  theme(legend.position = "none")

ggarrange(Locator_1, Locator_2, nrow=1, ncol=2, labels = c("Atlanta BeltLine", "BeltLine Block Groups"), label.x = 0, label.y= .075)

ggsave("F0_Locator.png", plot = last_plot(), device = "png", path = "outputs", dpi = 600)

# Index Kernel Density Plots ----

Index_Density1<-ggplot()+
  geom_density(data=dataset_00, aes(x=Index_Vuln, colour = belt_flag))+
  lims(x= c(0, 800), y=c(0, .0045))+
  labs(x = "Vulnerability Index", colour = "Location")+
  theme_classic()

Index_Density2<-ggplot()+
  geom_density(data=dataset_11, aes(x=Index_Vuln, colour = belt_flag))+
  lims(x= c(0, 800), y=c(0, .0045))+
  labs(x = "Vulnerability Index", colour = "Location")+
  theme_classic()

Index_Density3<-ggplot()+
  geom_density(data=dataset_16, aes(x=Index_Vuln, colour = belt_flag))+
  lims(x= c(0, 800), y=c(0, .0045))+
  labs(x = "Vulnerability Index", colour = "Location")+
  theme_classic()

Index_Density4<-ggplot()+
  geom_density(data=dataset_00, aes(x=Index_Housing, colour = belt_flag))+
  lims(x= c(100, 600), y=c(0, .009))+
  labs(x = "Housing Market Index", colour = "Location")+
  theme_classic()

Index_Density5<-ggplot()+
  geom_density(data=dataset_11, aes(x=Index_Housing, colour = belt_flag))+
  lims(x= c(100, 600), y=c(0, .009))+
  labs(x = "Housing Market Index", colour = "Location")+
  theme_classic()

Index_Density6<-ggplot()+
  geom_density(data=dataset_16, aes(x=Index_Housing, colour = belt_flag))+
  lims(x= c(100, 600), y=c(0, .009))+
  labs(x = "Housing Market Index", colour = "Location")+
  theme_classic()

ggarrange(Index_Density1, Index_Density2, Index_Density3, Index_Density4, Index_Density5, Index_Density6, nrow=2, ncol = 3, labels = c("2000", "2007-2011", "2012-2016"), label.x = .17, label.y= 1, legend = "right", common.legend = TRUE)
ggsave("F1_Index_Density.png", plot = last_plot(), device = "png", path = "outputs", dpi = 600)

# Vulnerability Index Alluvial Diagrams ----
# Now Make Alluvial Diagram of Vulnerability Change
Allu_00<-dataset_00 %>% select(GEOID, belt_flag, S_Year, Vuln_Cat, House_Cat, Index_Vuln, Index_Housing, Index_Q_Vuln, Index_Q_Housing)
Allu_11<-dataset_11 %>% select(GEOID, belt_flag, S_Year, Vuln_Cat,  House_Cat, Index_Vuln, Index_Housing, Index_Q_Vuln, Index_Q_Housing)
Allu_16<-dataset_16 %>% select(GEOID, belt_flag, S_Year, Vuln_Cat,  House_Cat, Index_Vuln, Index_Housing, Index_Q_Vuln, Index_Q_Housing)
dataset<-bind_rows(Allu_00, Allu_11, Allu_16)
dataset_wide <- dataset %>% select(GEOID, belt_flag, S_Year, Vuln_Cat) %>% pivot_wider(names_from = S_Year, names_prefix = "S_",values_from = Vuln_Cat)
dataset_wide <- dataset_wide %>% group_by(S_2000, S_2011, S_2016) %>% summarize(Freq = n()) %>% 
  mutate(Trajectory = case_when(
    S_2000 == "High Vulnerability" & S_2011 == "High Vulnerability" & S_2016 == "High Vulnerability" ~ "Persistantly High",
    S_2000 == "Moderate Vulnerability" & S_2011 == "Moderate Vulnerability" & S_2016 == "Moderate Vulnerability" ~ "Persistantly Moderate",
    S_2000 == "Low Vulnerability" & S_2011 == "Low Vulnerability" & S_2016 == "Low Vulnerability" ~ "Persistantly Low",
    S_2000 == "Low Vulnerability" & S_2011 == "Low Vulnerability" & S_2016 == "High Vulnerability" ~ "Increasing Vulnerability",
    S_2000 == "Low Vulnerability" & S_2011 == "Moderate Vulnerability" & S_2016 == "Moderate Vulnerability" ~ "Increasing Vulnerability",
    S_2000 == "Low Vulnerability" & S_2011 == "Moderate Vulnerability" & S_2016 == "High Vulnerability" ~ "Increasing Vulnerability",
    S_2000 == "Low Vulnerability" & S_2011 == "High Vulnerability" & S_2016 == "High Vulnerability" ~ "Increasing Vulnerability",
    S_2000 == "Moderate Vulnerability" & S_2011 == "Moderate Vulnerability" & S_2016 == "High Vulnerability" ~ "Increasing Vulnerability",
    S_2000 == "Moderate Vulnerability" & S_2011 == "High Vulnerability" & S_2016 == "High Vulnerability" ~ "Increasing Vulnerability",
    S_2000 == "Low Vulnerability" & S_2011 == "Low Vulnerability" & S_2016 == "Moderate Vulnerability" ~ "Increasing Vulnerability",
    S_2000 == "High Vulnerability" & S_2011 == "High Vulnerability" & S_2016 == "Moderate Vulnerability" ~ "Decreasing Vulnerability",  
    S_2000 == "High Vulnerability" & S_2011 == "High Vulnerability" & S_2016 == "Low Vulnerability" ~ "Decreasing Vulnerability",  
    S_2000 == "High Vulnerability" & S_2011 == "Moderate Vulnerability" & S_2016 == "Low Vulnerability" ~ "Decreasing Vulnerability", 
    S_2000 == "High Vulnerability" & S_2011 == "Low Vulnerability" & S_2016 == "Low Vulnerability" ~ "Decreasing Vulnerability", 
    S_2000 == "Moderate Vulnerability" & S_2011 == "Low Vulnerability" & S_2016 == "Low Vulnerability" ~ "Decreasing Vulnerability", 
    S_2000 == "High Vulnerability" & S_2011 == "Moderate Vulnerability" & S_2016 == "Moderate Vulnerability" ~ "Decreasing Vulnerability", 
    S_2000 == "Moderate Vulnerability" & S_2011 == "Moderate Vulnerability" & S_2016 == "Low Vulnerability" ~ "Decreasing Vulnerability",
    S_2000 == "High Vulnerability" & S_2011 == "Moderate Vulnerability" & S_2016 == "High Vulnerability" ~ "Fluctuating Vulnerability",
    S_2000 == "High Vulnerability" & S_2011 == "Low Vulnerability" & S_2016 == "High Vulnerability" ~ "Fluctuating Vulnerability",
    S_2000 == "High Vulnerability" & S_2011 == "Low Vulnerability" & S_2016 == "Moderate Vulnerability" ~ "Fluctuating Vulnerability",
    S_2000 == "Moderate Vulnerability" & S_2011 == "High Vulnerability" & S_2016 == "Moderate Vulnerability" ~ "Fluctuating Vulnerability",
    S_2000 == "Low Vulnerability" & S_2011 == "High Vulnerability" & S_2016 == "Moderate Vulnerability" ~ "Fluctuating Vulnerability",
    S_2000 == "Low Vulnerability" & S_2011 == "High Vulnerability" & S_2016 == "Low Vulnerability" ~ "Fluctuating Vulnerability",
    S_2000 == "Moderate Vulnerability" & S_2011 == "Low Vulnerability" & S_2016 == "Moderate Vulnerability" ~ "Fluctuating Vulnerability",
    S_2000 == "Low Vulnerability" & S_2011 == "Moderate Vulnerability" & S_2016 == "Low Vulnerability" ~ "Fluctuating Vulnerability",
    )) %>% 
  mutate(Trajectory = factor(Trajectory, levels = c("Persistantly High", "Persistantly Moderate", "Persistantly Low", "Increasing Vulnerability", "Decreasing Vulnerability", "Fluctuating Vulnerability")))

# Visualize all Atlanta by Trajectories
Allu_Atl<-ggplot(data= dataset_wide, aes(axis1 = S_2000, axis2 = S_2011, axis3 = S_2016, y = Freq))+
  geom_alluvium(aes(fill = Trajectory))+geom_stratum()+
  geom_text(stat="stratum", aes(label = after_stat(c("Low", "Mod", "High"))))+
  scale_x_discrete(limits = c("2000", "2011", "2016"))+
  labs(x="Atlanta")+
  theme_minimal()

# Create Disaggregate for Beltline
dataset_wide2 <- dataset %>% select(GEOID, belt_flag, S_Year, Vuln_Cat) %>% pivot_wider(names_from = S_Year, names_prefix = "S_",values_from = Vuln_Cat)
dataset_wide2 <- dataset_wide2 %>% group_by(belt_flag, S_2000, S_2011, S_2016) %>% summarize(Freq = n()) %>% 
  mutate(Trajectory = case_when(
    S_2000 == "High Vulnerability" & S_2011 == "High Vulnerability" & S_2016 == "High Vulnerability" ~ "Persistantly High",
    S_2000 == "Moderate Vulnerability" & S_2011 == "Moderate Vulnerability" & S_2016 == "Moderate Vulnerability" ~ "Persistantly Moderate",
    S_2000 == "Low Vulnerability" & S_2011 == "Low Vulnerability" & S_2016 == "Low Vulnerability" ~ "Persistantly Low",
    S_2000 == "Low Vulnerability" & S_2011 == "Low Vulnerability" & S_2016 == "High Vulnerability" ~ "Increasing Vulnerability",
    S_2000 == "Low Vulnerability" & S_2011 == "Moderate Vulnerability" & S_2016 == "Moderate Vulnerability" ~ "Increasing Vulnerability",
    S_2000 == "Low Vulnerability" & S_2011 == "Moderate Vulnerability" & S_2016 == "High Vulnerability" ~ "Increasing Vulnerability",
    S_2000 == "Low Vulnerability" & S_2011 == "High Vulnerability" & S_2016 == "High Vulnerability" ~ "Increasing Vulnerability",
    S_2000 == "Moderate Vulnerability" & S_2011 == "Moderate Vulnerability" & S_2016 == "High Vulnerability" ~ "Increasing Vulnerability",
    S_2000 == "Moderate Vulnerability" & S_2011 == "High Vulnerability" & S_2016 == "High Vulnerability" ~ "Increasing Vulnerability",
    S_2000 == "Low Vulnerability" & S_2011 == "Low Vulnerability" & S_2016 == "Moderate Vulnerability" ~ "Increasing Vulnerability",
    S_2000 == "High Vulnerability" & S_2011 == "High Vulnerability" & S_2016 == "Moderate Vulnerability" ~ "Decreasing Vulnerability",  
    S_2000 == "High Vulnerability" & S_2011 == "High Vulnerability" & S_2016 == "Low Vulnerability" ~ "Decreasing Vulnerability",  
    S_2000 == "High Vulnerability" & S_2011 == "Moderate Vulnerability" & S_2016 == "Low Vulnerability" ~ "Decreasing Vulnerability", 
    S_2000 == "High Vulnerability" & S_2011 == "Low Vulnerability" & S_2016 == "Low Vulnerability" ~ "Decreasing Vulnerability", 
    S_2000 == "Moderate Vulnerability" & S_2011 == "Low Vulnerability" & S_2016 == "Low Vulnerability" ~ "Decreasing Vulnerability", 
    S_2000 == "High Vulnerability" & S_2011 == "Moderate Vulnerability" & S_2016 == "Moderate Vulnerability" ~ "Decreasing Vulnerability", 
    S_2000 == "Moderate Vulnerability" & S_2011 == "Moderate Vulnerability" & S_2016 == "Low Vulnerability" ~ "Decreasing Vulnerability",
    S_2000 == "High Vulnerability" & S_2011 == "Moderate Vulnerability" & S_2016 == "High Vulnerability" ~ "Fluctuating Vulnerability",
    S_2000 == "High Vulnerability" & S_2011 == "Low Vulnerability" & S_2016 == "High Vulnerability" ~ "Fluctuating Vulnerability",
    S_2000 == "High Vulnerability" & S_2011 == "Low Vulnerability" & S_2016 == "Moderate Vulnerability" ~ "Fluctuating Vulnerability",
    S_2000 == "Moderate Vulnerability" & S_2011 == "High Vulnerability" & S_2016 == "Moderate Vulnerability" ~ "Fluctuating Vulnerability",
    S_2000 == "Low Vulnerability" & S_2011 == "High Vulnerability" & S_2016 == "Moderate Vulnerability" ~ "Fluctuating Vulnerability",
    S_2000 == "Low Vulnerability" & S_2011 == "High Vulnerability" & S_2016 == "Low Vulnerability" ~ "Fluctuating Vulnerability",
    S_2000 == "Moderate Vulnerability" & S_2011 == "Low Vulnerability" & S_2016 == "Moderate Vulnerability" ~ "Fluctuating Vulnerability",
    S_2000 == "Low Vulnerability" & S_2011 == "Moderate Vulnerability" & S_2016 == "Low Vulnerability" ~ "Fluctuating Vulnerability",
  )) %>% 
  mutate(Trajectory = factor(Trajectory, levels = c("Persistantly High", "Persistantly Moderate", "Persistantly Low", "Increasing Vulnerability", "Decreasing Vulnerability", "Fluctuating Vulnerability")))


Allu_Belt<-ggplot(data= dataset_wide2 %>% filter(belt_flag == "BeltLine"), aes(axis1 = S_2000, axis2 = S_2011, axis3 = S_2016, y = Freq))+
  geom_alluvium(aes(fill = Trajectory))+geom_stratum()+
  geom_text(stat="stratum", aes(label = after_stat(c("Low", "Mod", "High"))))+
  scale_x_discrete(limits = c("2000", "2011", "2016"))+
  labs(x="BeltLine")+
  theme_minimal()

Allu_NoBelt<-ggplot(data= dataset_wide2 %>% filter(belt_flag == "Not BeltLine"), aes(axis1 = S_2000, axis2 = S_2011, axis3 = S_2016, y = Freq))+
  geom_alluvium(aes(fill = Trajectory))+geom_stratum()+
  geom_text(stat="stratum", aes(label = after_stat(c("Low", "Mod", "High"))))+
  scale_x_discrete(limits = c("2000", "2011", "2016"))+
  labs(x="Not BeltLine")+
  theme_minimal()

ggarrange(Allu_Atl, Allu_Belt, Allu_NoBelt, ncol=3, nrow=1, common.legend = TRUE)
ggsave("F2_All_ATL_Alluvial.png", plot = last_plot(), device = "png", path = "outputs", dpi = 600)


# Create Housing Alluvials
dataset_wide3 <- dataset %>% select(GEOID, belt_flag, S_Year, House_Cat) %>% pivot_wider(names_from = S_Year, names_prefix = "S_",values_from = House_Cat)
dataset_wide3 <- dataset_wide3 %>% group_by(belt_flag, S_2000, S_2011, S_2016) %>% summarize(Freq = n()) %>% 
  mutate(Trajectory = case_when(
    S_2000 == "High Vulnerability" & S_2011 == "High Vulnerability" & S_2016 == "High Vulnerability" ~ "Persistantly High",
    S_2000 == "Moderate Vulnerability" & S_2011 == "Moderate Vulnerability" & S_2016 == "Moderate Vulnerability" ~ "Persistantly Moderate",
    S_2000 == "Low Vulnerability" & S_2011 == "Low Vulnerability" & S_2016 == "Low Vulnerability" ~ "Persistantly Low",
    S_2000 == "Low Vulnerability" & S_2011 == "Low Vulnerability" & S_2016 == "High Vulnerability" ~ "Increasing Vulnerability",
    S_2000 == "Low Vulnerability" & S_2011 == "Moderate Vulnerability" & S_2016 == "Moderate Vulnerability" ~ "Increasing Vulnerability",
    S_2000 == "Low Vulnerability" & S_2011 == "Moderate Vulnerability" & S_2016 == "High Vulnerability" ~ "Increasing Vulnerability",
    S_2000 == "Low Vulnerability" & S_2011 == "High Vulnerability" & S_2016 == "High Vulnerability" ~ "Increasing Vulnerability",
    S_2000 == "Moderate Vulnerability" & S_2011 == "Moderate Vulnerability" & S_2016 == "High Vulnerability" ~ "Increasing Vulnerability",
    S_2000 == "Moderate Vulnerability" & S_2011 == "High Vulnerability" & S_2016 == "High Vulnerability" ~ "Increasing Vulnerability",
    S_2000 == "Low Vulnerability" & S_2011 == "Low Vulnerability" & S_2016 == "Moderate Vulnerability" ~ "Increasing Vulnerability",
    S_2000 == "High Vulnerability" & S_2011 == "High Vulnerability" & S_2016 == "Moderate Vulnerability" ~ "Decreasing Vulnerability",  
    S_2000 == "High Vulnerability" & S_2011 == "High Vulnerability" & S_2016 == "Low Vulnerability" ~ "Decreasing Vulnerability",  
    S_2000 == "High Vulnerability" & S_2011 == "Moderate Vulnerability" & S_2016 == "Low Vulnerability" ~ "Decreasing Vulnerability", 
    S_2000 == "High Vulnerability" & S_2011 == "Low Vulnerability" & S_2016 == "Low Vulnerability" ~ "Decreasing Vulnerability", 
    S_2000 == "Moderate Vulnerability" & S_2011 == "Low Vulnerability" & S_2016 == "Low Vulnerability" ~ "Decreasing Vulnerability", 
    S_2000 == "High Vulnerability" & S_2011 == "Moderate Vulnerability" & S_2016 == "Moderate Vulnerability" ~ "Decreasing Vulnerability", 
    S_2000 == "Moderate Vulnerability" & S_2011 == "Moderate Vulnerability" & S_2016 == "Low Vulnerability" ~ "Decreasing Vulnerability",
    S_2000 == "High Vulnerability" & S_2011 == "Moderate Vulnerability" & S_2016 == "High Vulnerability" ~ "Fluctuating Vulnerability",
    S_2000 == "High Vulnerability" & S_2011 == "Low Vulnerability" & S_2016 == "High Vulnerability" ~ "Fluctuating Vulnerability",
    S_2000 == "High Vulnerability" & S_2011 == "Low Vulnerability" & S_2016 == "Moderate Vulnerability" ~ "Fluctuating Vulnerability",
    S_2000 == "Moderate Vulnerability" & S_2011 == "High Vulnerability" & S_2016 == "Moderate Vulnerability" ~ "Fluctuating Vulnerability",
    S_2000 == "Low Vulnerability" & S_2011 == "High Vulnerability" & S_2016 == "Moderate Vulnerability" ~ "Fluctuating Vulnerability",
    S_2000 == "Low Vulnerability" & S_2011 == "High Vulnerability" & S_2016 == "Low Vulnerability" ~ "Fluctuating Vulnerability",
    S_2000 == "Moderate Vulnerability" & S_2011 == "Low Vulnerability" & S_2016 == "Moderate Vulnerability" ~ "Fluctuating Vulnerability",
    S_2000 == "Low Vulnerability" & S_2011 == "Moderate Vulnerability" & S_2016 == "Low Vulnerability" ~ "Fluctuating Vulnerability",
  )) %>% 
  mutate(Trajectory = factor(Trajectory, levels = c("Persistantly High", "Persistantly Moderate", "Persistantly Low", "Increasing Vulnerability", "Decreasing Vulnerability", "Fluctuating Vulnerability")))

Allu_Housing_Atl<-ggplot(data= dataset_wide3, aes(axis1 = S_2000, axis2 = S_2011, axis3 = S_2016, y = Freq))+
  geom_alluvium(aes(fill = Trajectory))+geom_stratum()+
  geom_text(stat="stratum", aes(label = after_stat(c("Low", "Mod", "High"))))+
  scale_x_discrete(limits = c("2000", "2011", "2016"))+
  labs(x="Atlanta")+
  theme_minimal()

Allu_Housing_Belt<-ggplot(data= dataset_wide3 %>% filter(belt_flag == "BeltLine"), aes(axis1 = S_2000, axis2 = S_2011, axis3 = S_2016, y = Freq))+
  geom_alluvium(aes(fill = Trajectory))+geom_stratum()+
  geom_text(stat="stratum", aes(label = after_stat(c("Low", "Mod", "High"))))+
  scale_x_discrete(limits = c("2000", "2011", "2016"))+
  labs(x="BeltLine")+
  theme_minimal()
Allu_Housing_Belt
Allu_Housing_NoBelt<-ggplot(data= dataset_wide3 %>% filter(belt_flag == "Not BeltLine"), aes(axis1 = S_2000, axis2 = S_2011, axis3 = S_2016, y = Freq))+
  geom_alluvium(aes(fill = Trajectory))+geom_stratum()+
  geom_text(stat="stratum", aes(label = after_stat(c("Low", "Mod", "High"))))+
  scale_x_discrete(limits = c("2000", "2011", "2016"))+
  labs(x="Not BeltLine")+
  theme_minimal()
Allu_Housing_NoBelt
ggarrange(Allu_Housing_Atl, Allu_Housing_Belt, Allu_Housing_NoBelt, ncol=3, nrow=1, common.legend = TRUE)
ggsave("F2_All_ATL_Housing_Alluvial.png", plot = last_plot(), device = "png", path = "outputs", dpi = 600)

# Maps ----
dataset_map<-dataset %>% 
  select(GEOID, S_Year, Index_Vuln, Vuln_Cat, Index_Housing) %>% 
  pivot_wider(names_from = S_Year, values_from = c(Index_Vuln, Vuln_Cat, Index_Housing)) %>% 
  mutate(Trajectory = case_when(
    Vuln_Cat_2000 == "High Vulnerability" & Vuln_Cat_2011 == "High Vulnerability" & Vuln_Cat_2016 == "High Vulnerability" ~ "Persistantly High",
    Vuln_Cat_2000 == "Moderate Vulnerability" & Vuln_Cat_2011 == "Moderate Vulnerability" & Vuln_Cat_2016 == "Moderate Vulnerability" ~ "Persistantly Moderate",
    Vuln_Cat_2000 == "Low Vulnerability" & Vuln_Cat_2011 == "Low Vulnerability" & Vuln_Cat_2016 == "Low Vulnerability" ~ "Persistantly Low",
    Vuln_Cat_2000 == "Low Vulnerability" & Vuln_Cat_2011 == "Low Vulnerability" & Vuln_Cat_2016 == "High Vulnerability" ~ "Increasing Vulnerability",
    Vuln_Cat_2000 == "Low Vulnerability" & Vuln_Cat_2011 == "Moderate Vulnerability" & Vuln_Cat_2016 == "Moderate Vulnerability" ~ "Increasing Vulnerability",
    Vuln_Cat_2000 == "Low Vulnerability" & Vuln_Cat_2011 == "Moderate Vulnerability" & Vuln_Cat_2016 == "High Vulnerability" ~ "Increasing Vulnerability",
    Vuln_Cat_2000 == "Low Vulnerability" & Vuln_Cat_2011 == "High Vulnerability" & Vuln_Cat_2016 == "High Vulnerability" ~ "Increasing Vulnerability",
    Vuln_Cat_2000 == "Moderate Vulnerability" & Vuln_Cat_2011 == "Moderate Vulnerability" & Vuln_Cat_2016 == "High Vulnerability" ~ "Increasing Vulnerability",
    Vuln_Cat_2000 == "Moderate Vulnerability" & Vuln_Cat_2011 == "High Vulnerability" & Vuln_Cat_2016 == "High Vulnerability" ~ "Increasing Vulnerability",
    Vuln_Cat_2000 == "Moderate Vulnerability" & Vuln_Cat_2011 == "High Vulnerability" & Vuln_Cat_2016 == "Low Vulnerability" ~ "Fluctuating Vulnerability",
    Vuln_Cat_2000 == "Moderate Vulnerability" & Vuln_Cat_2011 == "Low Vulnerability" & Vuln_Cat_2016 == "High Vulnerability" ~ "Fluctuating Vulnerability",
    Vuln_Cat_2000 == "Low Vulnerability" & Vuln_Cat_2011 == "Low Vulnerability" & Vuln_Cat_2016 == "Moderate Vulnerability" ~ "Increasing Vulnerability",
    Vuln_Cat_2000 == "High Vulnerability" & Vuln_Cat_2011 == "High Vulnerability" & Vuln_Cat_2016 == "Moderate Vulnerability" ~ "Decreasing Vulnerability",  
    Vuln_Cat_2000 == "High Vulnerability" & Vuln_Cat_2011 == "High Vulnerability" & Vuln_Cat_2016 == "Low Vulnerability" ~ "Decreasing Vulnerability",  
    Vuln_Cat_2000 == "High Vulnerability" & Vuln_Cat_2011 == "Moderate Vulnerability" & Vuln_Cat_2016 == "Low Vulnerability" ~ "Decreasing Vulnerability", 
    Vuln_Cat_2000 == "High Vulnerability" & Vuln_Cat_2011 == "Low Vulnerability" & Vuln_Cat_2016 == "Low Vulnerability" ~ "Decreasing Vulnerability", 
    Vuln_Cat_2000 == "Moderate Vulnerability" & Vuln_Cat_2011 == "Low Vulnerability" & Vuln_Cat_2016 == "Low Vulnerability" ~ "Decreasing Vulnerability", 
    Vuln_Cat_2000 == "High Vulnerability" & Vuln_Cat_2011 == "Moderate Vulnerability" & Vuln_Cat_2016 == "Moderate Vulnerability" ~ "Decreasing Vulnerability", 
    Vuln_Cat_2000 == "Moderate Vulnerability" & Vuln_Cat_2011 == "Moderate Vulnerability" & Vuln_Cat_2016 == "Low Vulnerability" ~ "Decreasing Vulnerability",
    Vuln_Cat_2000 == "High Vulnerability" & Vuln_Cat_2011 == "Moderate Vulnerability" & Vuln_Cat_2016 == "High Vulnerability" ~ "Fluctuating Vulnerability",
    Vuln_Cat_2000 == "High Vulnerability" & Vuln_Cat_2011 == "Low Vulnerability" & Vuln_Cat_2016 == "High Vulnerability" ~ "Fluctuating Vulnerability",
    Vuln_Cat_2000 == "High Vulnerability" & Vuln_Cat_2011 == "Low Vulnerability" & Vuln_Cat_2016 == "Moderate Vulnerability" ~ "Fluctuating Vulnerability",
    Vuln_Cat_2000 == "Moderate Vulnerability" & Vuln_Cat_2011 == "High Vulnerability" & Vuln_Cat_2016 == "Moderate Vulnerability" ~ "Fluctuating Vulnerability",
    Vuln_Cat_2000 == "Low Vulnerability" & Vuln_Cat_2011 == "High Vulnerability" & Vuln_Cat_2016 == "Moderate Vulnerability" ~ "Fluctuating Vulnerability",
    Vuln_Cat_2000 == "Low Vulnerability" & Vuln_Cat_2011 == "High Vulnerability" & Vuln_Cat_2016 == "Low Vulnerability" ~ "Fluctuating Vulnerability",
    Vuln_Cat_2000 == "Moderate Vulnerability" & Vuln_Cat_2011 == "Low Vulnerability" & Vuln_Cat_2016 == "Moderate Vulnerability" ~ "Fluctuating Vulnerability",
    Vuln_Cat_2000 == "Low Vulnerability" & Vuln_Cat_2011 == "Moderate Vulnerability" & Vuln_Cat_2016 == "Low Vulnerability" ~ "Fluctuating Vulnerability",
  )) %>% 
  mutate(Trajectory = factor(Trajectory, levels = c("Persistantly High", "Persistantly Moderate", "Persistantly Low", "Increasing Vulnerability", "Decreasing Vulnerability", "Fluctuating Vulnerability")))

dataset_map<-left_join(dataset_map, bg_limits, by="GEOID")
dataset_map<-st_as_sf(dataset_map)
ATL_Trajectory<-ggplot()+geom_sf(data=dataset_map, aes(fill = Trajectory, color = Trajectory))+
  scale_color_brewer(palette = "Paired", guide = FALSE)+  
  scale_fill_brewer(palette = "Paired")+
  labs(fill = "Vulnerability Trajectory")+
  coord_sf(datum=NA)+
  theme_minimal()

ATL_Trajectory_Facet<-ggplot()+
  geom_sf(data=bg_limits, color = "gray80", lwd=.15, fill=NA)+
  geom_sf(data=dataset_map, aes(fill = Trajectory, color = Trajectory), alpha = .95)+
  geom_sf(data=bg_limits %>% group_by(belt_flag) %>% summarise(n()) %>% filter(belt_flag == "Beltline"), lwd = .4, fill = "gray50", colour = "gray30", alpha = .3)+
  geom_sf(data=bg_limits %>% st_union(), color = "gray40", fill=NA)+
  scale_color_brewer(palette = "Paired", guide=FALSE)+  
  scale_fill_brewer(palette = "Paired")+
  coord_sf(datum=NA)+
  theme_minimal()+
  theme(legend.position = "none")+
  facet_wrap(~Trajectory)

ggarrange(ATL_Trajectory, ATL_Trajectory_Facet, nrow=1, ncol=2, common.legend=TRUE)
ggsave("F3_Map_ATL_Vulnerability_Trajectory.png", plot = last_plot(), device = "png", path = "outputs", dpi = 600)

# Vulnerability Trajectory 
Vuln_Trajectory<-ggplot()+
  geom_sf(data=dataset_map %>% filter(belt_flag == "Beltline"), aes(fill = Trajectory))+
  labs(fill = "Vulnerability Trajectory")+
  scale_fill_brewer(palette = "Paired")+
  coord_sf(datum=NA)+
  theme_minimal()

Beltline_Trajectory_Facet<-ggplot()+
  geom_sf(data=bg_limits %>% filter(belt_flag == "Beltline"), color = "gray80", fill=NA)+
  geom_sf(data=dataset_map %>% filter(belt_flag == "Beltline"), aes(fill = Trajectory, color = Trajectory))+
  scale_color_brewer(palette = "Paired")+  
  scale_fill_brewer(palette = "Paired")+
  coord_sf(datum=NA)+
  theme_minimal()+
  theme(legend.position = "none")+
  facet_wrap(~Trajectory)

ggarrange(Vuln_Trajectory, Beltline_Trajectory_Facet, common.legend = TRUE)
ggsave("F5_Beltline_Trajectory.png", plot = last_plot(), device = "png", path = "outputs", dpi = 600)


# Vulnerability Index
VIndex_1<-ggplot()+
  geom_sf(data=dataset_map %>% filter(belt_flag == "Beltline"), aes(fill = Index_Vuln_2000), color = "gray60")+
  scale_fill_distiller(palette = "RdBu", breaks = c(0, 250, 500, 750), labels = c("0 Lowest", "250", "500", "750 Highest"), limits =c(0, 750))+
  labs(fill = "Vulnerability Index")+
  coord_sf(datum=NA)+
  theme_minimal()+
  theme(legend.position = "none")

VIndex_2<-ggplot()+
  geom_sf(data=dataset_map %>% filter(belt_flag == "Beltline"), aes(fill = Index_Vuln_2011), color = "gray60")+
  scale_fill_distiller(palette = "RdBu", breaks = c(0, 250, 500, 750), labels = c("0 Lowest", "250", "500", "750 Highest"), limits =c(0, 750))+
  labs(fill = "Vulnerability Index")+
  coord_sf(datum=NA)+
  theme_minimal()+
  theme(legend.position = "none")


VIndex_3<-ggplot()+
  geom_sf(data=dataset_map %>% filter(belt_flag == "Beltline"), aes(fill = Index_Vuln_2016), color = "gray60")+
  scale_fill_distiller(palette = "RdBu", breaks = c(0, 250, 500, 750), labels = c("0 Lowest", "250", "500", "750 Highest"), limits =c(0, 750))+
  labs(fill = "Vulnerability Index")+
  coord_sf(datum=NA)+
  theme_minimal()

# Vulnerability Categories
VCAT_1<-ggplot()+
  geom_sf(data=dataset_map %>% filter(belt_flag == "Beltline"), aes(fill = Vuln_Cat_2000), color = "gray60")+
  scale_fill_brewer(palette = "Pastel1")+
  labs(fill = "Vulnerability Category")+
  coord_sf(datum=NA)+
  theme_minimal()+
  theme(legend.position = "none")

VCAT_2<-ggplot()+
  geom_sf(data=dataset_map %>% filter(belt_flag == "Beltline"), aes(fill = Vuln_Cat_2011), color = "gray60")+
  scale_fill_brewer(palette = "Pastel1")+
  labs(fill = "Vulnerability Category")+
  coord_sf(datum=NA)+
  theme_minimal()+
  theme(legend.position = "none")

VCAT_3<-ggplot()+
  geom_sf(data=dataset_map %>% filter(belt_flag == "Beltline"), aes(fill = Vuln_Cat_2016), color = "gray60")+
  scale_fill_brewer(palette = "Pastel1")+
  labs(fill = "Vulnerability Category")+
  coord_sf(datum=NA)+
  theme_minimal()

ggarrange(VIndex_1, VIndex_2, VIndex_3, VCAT_1, VCAT_2, VCAT_3, labels = c("2000", "2007-2011", "2012-2016"), label.y= 1, ncol = 3, widths = c(1, 1, 1.5), nrow = 2)
ggsave("F4_Beltline_Vuln.png", plot = last_plot(), device = "png", path = "outputs", dpi = 600)



# Housing Market Categories

dataset_wide_housing <- dataset %>% select(GEOID, belt_flag, S_Year, House_Cat) %>% pivot_wider(names_from = S_Year, names_prefix = "S_",values_from = House_Cat)
dataset_wide_housing <- dataset_wide_housing %>% ungroup() %>% group_by(S_2000, S_2011, S_2016) %>% summarise(Freq = n()) %>% 
  mutate(Housing_Trajectory = case_when(
    S_2000 == "High Vulnerability" & S_2011 == "High Vulnerability" & S_2016 == "High Vulnerability" ~ "Persistantly High",
    S_2000 == "Moderate Vulnerability" & S_2011 == "Moderate Vulnerability" & S_2016 == "Moderate Vulnerability" ~ "Persistantly Moderate",
    S_2000 == "Low Vulnerability" & S_2011 == "Low Vulnerability" & S_2016 == "Low Vulnerability" ~ "Persistantly Low",
    S_2000 == "Low Vulnerability" & S_2011 == "Low Vulnerability" & S_2016 == "High Vulnerability" ~ "Increasing Vulnerability",
    S_2000 == "Low Vulnerability" & S_2011 == "Moderate Vulnerability" & S_2016 == "Moderate Vulnerability" ~ "Increasing Vulnerability",
    S_2000 == "Low Vulnerability" & S_2011 == "Moderate Vulnerability" & S_2016 == "High Vulnerability" ~ "Increasing Vulnerability",
    S_2000 == "Low Vulnerability" & S_2011 == "High Vulnerability" & S_2016 == "High Vulnerability" ~ "Increasing Vulnerability",
    S_2000 == "Moderate Vulnerability" & S_2011 == "Moderate Vulnerability" & S_2016 == "High Vulnerability" ~ "Increasing Vulnerability",
    S_2000 == "Moderate Vulnerability" & S_2011 == "High Vulnerability" & S_2016 == "High Vulnerability" ~ "Increasing Vulnerability",
    S_2000 == "Low Vulnerability" & S_2011 == "Low Vulnerability" & S_2016 == "Moderate Vulnerability" ~ "Increasing Vulnerability",
    S_2000 == "High Vulnerability" & S_2011 == "High Vulnerability" & S_2016 == "Moderate Vulnerability" ~ "Decreasing Vulnerability",  
    S_2000 == "High Vulnerability" & S_2011 == "High Vulnerability" & S_2016 == "Low Vulnerability" ~ "Decreasing Vulnerability",  
    S_2000 == "High Vulnerability" & S_2011 == "Moderate Vulnerability" & S_2016 == "Low Vulnerability" ~ "Decreasing Vulnerability", 
    S_2000 == "High Vulnerability" & S_2011 == "Low Vulnerability" & S_2016 == "Low Vulnerability" ~ "Decreasing Vulnerability", 
    S_2000 == "Moderate Vulnerability" & S_2011 == "Low Vulnerability" & S_2016 == "Low Vulnerability" ~ "Decreasing Vulnerability", 
    S_2000 == "High Vulnerability" & S_2011 == "Moderate Vulnerability" & S_2016 == "Moderate Vulnerability" ~ "Decreasing Vulnerability", 
    S_2000 == "Moderate Vulnerability" & S_2011 == "Moderate Vulnerability" & S_2016 == "Low Vulnerability" ~ "Decreasing Vulnerability",
    S_2000 == "Moderate Vulnerability" & S_2011 == "High Vulnerability" & S_2016 == "Low Vulnerability" ~ "Fluctuating Vulnerability",
    S_2000 == "Moderate Vulnerability" & S_2011 == "Low Vulnerability" & S_2016 == "High Vulnerability" ~ "Fluctuating Vulnerability",
    S_2000 == "High Vulnerability" & S_2011 == "Moderate Vulnerability" & S_2016 == "High Vulnerability" ~ "Fluctuating Vulnerability",
    S_2000 == "High Vulnerability" & S_2011 == "Low Vulnerability" & S_2016 == "High Vulnerability" ~ "Fluctuating Vulnerability",
    S_2000 == "High Vulnerability" & S_2011 == "Low Vulnerability" & S_2016 == "Moderate Vulnerability" ~ "Fluctuating Vulnerability",
    S_2000 == "Moderate Vulnerability" & S_2011 == "High Vulnerability" & S_2016 == "Moderate Vulnerability" ~ "Fluctuating Vulnerability",
    S_2000 == "Low Vulnerability" & S_2011 == "High Vulnerability" & S_2016 == "Moderate Vulnerability" ~ "Fluctuating Vulnerability",
    S_2000 == "Low Vulnerability" & S_2011 == "High Vulnerability" & S_2016 == "Low Vulnerability" ~ "Fluctuating Vulnerability",
    S_2000 == "Moderate Vulnerability" & S_2011 == "Low Vulnerability" & S_2016 == "Moderate Vulnerability" ~ "Fluctuating Vulnerability",
    S_2000 == "Low Vulnerability" & S_2011 == "Moderate Vulnerability" & S_2016 == "Low Vulnerability" ~ "Fluctuating Vulnerability",
  )) %>% 
  mutate(Housing_Trajectory = factor(Housing_Trajectory, levels = c("Persistantly High", "Persistantly Moderate", "Persistantly Low", "Increasing Vulnerability", "Decreasing Vulnerability", "Fluctuating Vulnerability")))

ggplot(data= dataset_wide_housing, aes(axis1 = S_2000, axis2 = S_2011, axis3 = S_2016, y = Freq))+
  geom_alluvium(aes(fill = Housing_Trajectory))+geom_stratum()+
  geom_text(stat="stratum", aes(label = after_stat(c("Low", "Mod", "High"))))+
  scale_x_discrete(limits = c("2000", "2011", "2016"))+
  theme_minimal()

dataset_map_Housing<-dataset %>% 
  select(GEOID, S_Year, Index_Vuln, Vuln_Cat, Index_Housing, House_Cat) %>% 
  pivot_wider(names_from = S_Year, values_from = c(Index_Vuln, Vuln_Cat, Index_Housing, House_Cat)) %>% 
  mutate(Trajectory = case_when(
    Vuln_Cat_2000 == "High Vulnerability" & Vuln_Cat_2011 == "High Vulnerability" & Vuln_Cat_2016 == "High Vulnerability" ~ "Persistantly High",
    Vuln_Cat_2000 == "Moderate Vulnerability" & Vuln_Cat_2011 == "Moderate Vulnerability" & Vuln_Cat_2016 == "Moderate Vulnerability" ~ "Persistantly Moderate",
    Vuln_Cat_2000 == "Low Vulnerability" & Vuln_Cat_2011 == "Low Vulnerability" & Vuln_Cat_2016 == "Low Vulnerability" ~ "Persistantly Low",
    Vuln_Cat_2000 == "Low Vulnerability" & Vuln_Cat_2011 == "Low Vulnerability" & Vuln_Cat_2016 == "High Vulnerability" ~ "Increasing Vulnerability",
    Vuln_Cat_2000 == "Low Vulnerability" & Vuln_Cat_2011 == "Moderate Vulnerability" & Vuln_Cat_2016 == "Moderate Vulnerability" ~ "Increasing Vulnerability",
    Vuln_Cat_2000 == "Low Vulnerability" & Vuln_Cat_2011 == "Moderate Vulnerability" & Vuln_Cat_2016 == "High Vulnerability" ~ "Increasing Vulnerability",
    Vuln_Cat_2000 == "Low Vulnerability" & Vuln_Cat_2011 == "High Vulnerability" & Vuln_Cat_2016 == "High Vulnerability" ~ "Increasing Vulnerability",
    Vuln_Cat_2000 == "Moderate Vulnerability" & Vuln_Cat_2011 == "Moderate Vulnerability" & Vuln_Cat_2016 == "High Vulnerability" ~ "Increasing Vulnerability",
    Vuln_Cat_2000 == "Moderate Vulnerability" & Vuln_Cat_2011 == "High Vulnerability" & Vuln_Cat_2016 == "High Vulnerability" ~ "Increasing Vulnerability",
    Vuln_Cat_2000 == "Low Vulnerability" & Vuln_Cat_2011 == "Low Vulnerability" & Vuln_Cat_2016 == "Moderate Vulnerability" ~ "Increasing Vulnerability",
    Vuln_Cat_2000 == "High Vulnerability" & Vuln_Cat_2011 == "High Vulnerability" & Vuln_Cat_2016 == "Moderate Vulnerability" ~ "Decreasing Vulnerability",  
    Vuln_Cat_2000 == "High Vulnerability" & Vuln_Cat_2011 == "High Vulnerability" & Vuln_Cat_2016 == "Low Vulnerability" ~ "Decreasing Vulnerability",  
    Vuln_Cat_2000 == "High Vulnerability" & Vuln_Cat_2011 == "Moderate Vulnerability" & Vuln_Cat_2016 == "Low Vulnerability" ~ "Decreasing Vulnerability", 
    Vuln_Cat_2000 == "High Vulnerability" & Vuln_Cat_2011 == "Low Vulnerability" & Vuln_Cat_2016 == "Low Vulnerability" ~ "Decreasing Vulnerability", 
    Vuln_Cat_2000 == "Moderate Vulnerability" & Vuln_Cat_2011 == "Low Vulnerability" & Vuln_Cat_2016 == "Low Vulnerability" ~ "Decreasing Vulnerability", 
    Vuln_Cat_2000 == "High Vulnerability" & Vuln_Cat_2011 == "Moderate Vulnerability" & Vuln_Cat_2016 == "Moderate Vulnerability" ~ "Decreasing Vulnerability", 
    Vuln_Cat_2000 == "Moderate Vulnerability" & Vuln_Cat_2011 == "Moderate Vulnerability" & Vuln_Cat_2016 == "Low Vulnerability" ~ "Decreasing Vulnerability",
    Vuln_Cat_2000 == "High Vulnerability" & Vuln_Cat_2011 == "Moderate Vulnerability" & Vuln_Cat_2016 == "High Vulnerability" ~ "Fluctuating Vulnerability",
    Vuln_Cat_2000 == "High Vulnerability" & Vuln_Cat_2011 == "Low Vulnerability" & Vuln_Cat_2016 == "High Vulnerability" ~ "Fluctuating Vulnerability",
    Vuln_Cat_2000 == "High Vulnerability" & Vuln_Cat_2011 == "Low Vulnerability" & Vuln_Cat_2016 == "Moderate Vulnerability" ~ "Fluctuating Vulnerability",
    Vuln_Cat_2000 == "Moderate Vulnerability" & Vuln_Cat_2011 == "High Vulnerability" & Vuln_Cat_2016 == "Moderate Vulnerability" ~ "Fluctuating Vulnerability",
    Vuln_Cat_2000 == "Low Vulnerability" & Vuln_Cat_2011 == "High Vulnerability" & Vuln_Cat_2016 == "Moderate Vulnerability" ~ "Fluctuating Vulnerability",
    Vuln_Cat_2000 == "Low Vulnerability" & Vuln_Cat_2011 == "High Vulnerability" & Vuln_Cat_2016 == "Low Vulnerability" ~ "Fluctuating Vulnerability",
    Vuln_Cat_2000 == "Moderate Vulnerability" & Vuln_Cat_2011 == "Low Vulnerability" & Vuln_Cat_2016 == "Moderate Vulnerability" ~ "Fluctuating Vulnerability",
    Vuln_Cat_2000 == "Low Vulnerability" & Vuln_Cat_2011 == "Moderate Vulnerability" & Vuln_Cat_2016 == "Low Vulnerability" ~ "Fluctuating Vulnerability",
  )) %>% 
  mutate(Trajectory = factor(Trajectory, levels = c("Persistantly High", "Persistantly Moderate", "Persistantly Low", "Increasing Vulnerability", "Decreasing Vulnerability", "Fluctuating Vulnerability")))

dataset_map_Housing <- dataset_map_Housing %>% mutate(
  Housing_Trajectory = case_when( House_Cat_2000 == "High Vulnerability" & House_Cat_2011 == "High Vulnerability" & House_Cat_2016 == "High Vulnerability" ~ "Persistantly High",
                                                                                      House_Cat_2000 == "Moderate Vulnerability" & House_Cat_2011 == "Moderate Vulnerability" & House_Cat_2016 == "Moderate Vulnerability" ~ "Persistantly Moderate",
                                                                                      House_Cat_2000 == "Low Vulnerability" & House_Cat_2011 == "Low Vulnerability" & House_Cat_2016 == "Low Vulnerability" ~ "Persistantly Low",
                                                                                      House_Cat_2000 == "Low Vulnerability" & House_Cat_2011 == "Low Vulnerability" & House_Cat_2016 == "High Vulnerability" ~ "Increasing Vulnerability",
                                                                                      House_Cat_2000 == "Low Vulnerability" & House_Cat_2011 == "Moderate Vulnerability" & House_Cat_2016 == "Moderate Vulnerability" ~ "Increasing Vulnerability",
                                                                                      House_Cat_2000 == "Low Vulnerability" & House_Cat_2011 == "Moderate Vulnerability" & House_Cat_2016 == "High Vulnerability" ~ "Increasing Vulnerability",
                                                                                      House_Cat_2000 == "Low Vulnerability" & House_Cat_2011 == "High Vulnerability" & House_Cat_2016 == "High Vulnerability" ~ "Increasing Vulnerability",
                                                                                      House_Cat_2000 == "Moderate Vulnerability" & House_Cat_2011 == "Moderate Vulnerability" & House_Cat_2016 == "High Vulnerability" ~ "Increasing Vulnerability",
                                                                                      House_Cat_2000 == "Moderate Vulnerability" & House_Cat_2011 == "High Vulnerability" & House_Cat_2016 == "High Vulnerability" ~ "Increasing Vulnerability",
                                                                                      House_Cat_2000 == "Low Vulnerability" & House_Cat_2011 == "Low Vulnerability" & House_Cat_2016 == "Moderate Vulnerability" ~ "Increasing Vulnerability",
                                                                                      House_Cat_2000 == "High Vulnerability" & House_Cat_2011 == "High Vulnerability" & House_Cat_2016 == "Moderate Vulnerability" ~ "Decreasing Vulnerability",  
                                                                                      House_Cat_2000 == "High Vulnerability" & House_Cat_2011 == "High Vulnerability" & House_Cat_2016 == "Low Vulnerability" ~ "Decreasing Vulnerability",  
                                                                                      House_Cat_2000 == "High Vulnerability" & House_Cat_2011 == "Moderate Vulnerability" & House_Cat_2016 == "Low Vulnerability" ~ "Decreasing Vulnerability", 
                                                                                      House_Cat_2000 == "High Vulnerability" & House_Cat_2011 == "Low Vulnerability" & House_Cat_2016 == "Low Vulnerability" ~ "Decreasing Vulnerability", 
                                                                                      House_Cat_2000 == "Moderate Vulnerability" & House_Cat_2011 == "Low Vulnerability" & House_Cat_2016 == "Low Vulnerability" ~ "Decreasing Vulnerability", 
                                                                                      House_Cat_2000 == "High Vulnerability" & House_Cat_2011 == "Moderate Vulnerability" & House_Cat_2016 == "Moderate Vulnerability" ~ "Decreasing Vulnerability", 
                                                                                      House_Cat_2000 == "Moderate Vulnerability" & House_Cat_2011 == "Moderate Vulnerability" & House_Cat_2016 == "Low Vulnerability" ~ "Decreasing Vulnerability",
                                                                                      House_Cat_2000 == "Moderate Vulnerability" & House_Cat_2011 == "High Vulnerability" & House_Cat_2016 == "Low Vulnerability" ~ "Fluctuating Vulnerability",
                                                                                      House_Cat_2000 == "Moderate Vulnerability" & House_Cat_2011 == "Low Vulnerability" & House_Cat_2016 == "High Vulnerability" ~ "Fluctuating Vulnerability",
                                                                                      House_Cat_2000 == "High Vulnerability" & House_Cat_2011 == "Moderate Vulnerability" & House_Cat_2016 == "High Vulnerability" ~ "Fluctuating Vulnerability",
                                                                                      House_Cat_2000 == "High Vulnerability" & House_Cat_2011 == "Low Vulnerability" & House_Cat_2016 == "High Vulnerability" ~ "Fluctuating Vulnerability",
                                                                                      House_Cat_2000 == "High Vulnerability" & House_Cat_2011 == "Low Vulnerability" & House_Cat_2016 == "Moderate Vulnerability" ~ "Fluctuating Vulnerability",
                                                                                      House_Cat_2000 == "Moderate Vulnerability" & House_Cat_2011 == "High Vulnerability" & House_Cat_2016 == "Moderate Vulnerability" ~ "Fluctuating Vulnerability",
                                                                                      House_Cat_2000 == "Low Vulnerability" & House_Cat_2011 == "High Vulnerability" & House_Cat_2016 == "Moderate Vulnerability" ~ "Fluctuating Vulnerability",
                                                                                      House_Cat_2000 == "Low Vulnerability" & House_Cat_2011 == "High Vulnerability" & House_Cat_2016 == "Low Vulnerability" ~ "Fluctuating Vulnerability",
                                                                                      House_Cat_2000 == "Moderate Vulnerability" & House_Cat_2011 == "Low Vulnerability" & House_Cat_2016 == "Moderate Vulnerability" ~ "Fluctuating Vulnerability",
                                                                                      House_Cat_2000 == "Low Vulnerability" & House_Cat_2011 == "Moderate Vulnerability" & House_Cat_2016 == "Low Vulnerability" ~ "Fluctuating Vulnerability",
)) %>% 
  mutate(Housing_Trajectory = factor(Housing_Trajectory, levels = c("Persistantly High", "Persistantly Moderate", "Persistantly Low", "Increasing Vulnerability", "Decreasing Vulnerability", "Fluctuating Vulnerability")))


dataset_map_Housing<-left_join(dataset_map_Housing, bg_limits, by="GEOID")
dataset_map_Housing<-st_as_sf(dataset_map_Housing)


# Housing Market Index
HIndex_1<-ggplot()+
  geom_sf(data=dataset_map_Housing %>% filter(belt_flag == "Beltline"), aes(fill = Index_Housing_2000), color = "gray60")+
  scale_fill_distiller(palette = "RdBu", breaks = c(0, 200, 400, 600), labels = c("0 Lowest", "200", "400", "600 Highest"), limits =c(0, 600))+
  labs(fill = "Housing Index")+
  coord_sf(datum=NA)+
  theme_minimal()+
  theme(legend.position = "none")

HIndex_2<-ggplot()+
  geom_sf(data=dataset_map_Housing %>% filter(belt_flag == "Beltline"), aes(fill = Index_Housing_2011), color = "gray60")+
  scale_fill_distiller(palette = "RdBu", breaks = c(0, 200, 400, 600), labels = c("0 Lowest", "200", "400", "600 Highest"), limits =c(0, 600))+
  labs(fill = "Housing Index")+
  coord_sf(datum=NA)+
  theme_minimal()+
  theme(legend.position = "none")

HIndex_3<-ggplot()+
  geom_sf(data=dataset_map_Housing %>% filter(belt_flag == "Beltline"), aes(fill = Index_Housing_2016), color = "gray60")+
  scale_fill_distiller(palette = "RdBu", breaks = c(0, 200, 400, 600), labels = c("0 Lowest", "200", "400", "600 Highest"), limits =c(0, 600))+
  labs(fill = "Housing Index")+
  coord_sf(datum=NA)+
  theme_minimal()

HIndex_4<-ggplot()+
  geom_sf(data=dataset_map_Housing %>% filter(belt_flag == "Beltline"), aes(fill = House_Cat_2000), color = "gray60")+
  scale_fill_brewer(palette = "Pastel1")+
  labs(fill = "Housing Category")+
  coord_sf(datum=NA)+
  theme_minimal()+
  theme(legend.position = "none")

HIndex_5<-ggplot()+
  geom_sf(data=dataset_map_Housing %>% filter(belt_flag == "Beltline"), aes(fill = House_Cat_2011), color = "gray60")+
  scale_fill_brewer(palette = "Pastel1")+
  labs(fill = "Housing Category")+
  coord_sf(datum=NA)+
  theme_minimal()+
  theme(legend.position = "none")

HIndex_6<-ggplot()+
  geom_sf(data=dataset_map_Housing %>% filter(belt_flag == "Beltline"), aes(fill = House_Cat_2016), color = "gray60")+
  scale_fill_brewer(palette = "Pastel1")+
  labs(fill = "Housing Category")+
  coord_sf(datum=NA)+
  theme_minimal()


ggarrange(HIndex_1, HIndex_2, HIndex_3, HIndex_4, HIndex_5, HIndex_6, labels = c("2000", "2007-2011", "2012-2016"), label.y= 1.01, ncol = 3, nrow = 2, widths = c(1, 1, 2))
ggsave("F6_Beltline_Housing_Index.png", plot = last_plot(), device = "png", path = "outputs", dpi = 600)



ATL_Housing_Trajectory<-ggplot()+geom_sf(data=dataset_map_Housing, aes(fill = Housing_Trajectory, color= Housing_Trajectory))+
  scale_color_brewer(palette = "Paired", guide=FALSE)+  
  scale_fill_brewer(palette = "Paired")+
  labs(fill = "Housing Trajectory")+
  coord_sf(datum=NA)+
  theme_minimal()

ATL_Housing_Trajectory

ATL_Housing_Trajectory_Facet<-ggplot()+
  geom_sf(data=bg_limits, color = "gray80", lwd=.15, fill=NA)+
  geom_sf(data=dataset_map_Housing, aes(fill = Housing_Trajectory, color = Housing_Trajectory), alpha = .95)+
  geom_sf(data=bg_limits %>% group_by(belt_flag) %>% summarise(n()) %>% filter(belt_flag == "Beltline"), lwd = .4, fill = "gray50", colour = "gray30", alpha = .3)+
  geom_sf(data=bg_limits %>% st_union(), color = "gray40", fill=NA)+
  scale_color_brewer(palette = "Paired", guide = FALSE)+  
  scale_fill_brewer(palette = "Paired")+
  coord_sf(datum=NA)+
  theme_minimal()+
  theme(legend.position = "none")+
  facet_wrap(~Housing_Trajectory)
ATL_Housing_Trajectory_Facet

ggarrange(ATL_Housing_Trajectory, ATL_Housing_Trajectory_Facet, nrow=1, ncol=2, common.legend=TRUE)
ggsave("F3_Map_ATL_Housing_Trajectory.png", plot = last_plot(), device = "png", path = "outputs", dpi = 600)

# BeltLine Vulnerability Trajectory 
Vuln_Trajectory<-ggplot()+
  geom_sf(data=dataset_map %>% filter(belt_flag == "Beltline"), aes(fill = Trajectory))+
  labs(fill = "Vulnerability Trajectory")+
  scale_fill_brewer(palette = "Paired")+
  coord_sf(datum=NA)+
  theme_minimal()

Beltline_Trajectory_Facet<-ggplot()+
  geom_sf(data=bg_limits %>% filter(belt_flag == "Beltline"), color = "gray80", fill=NA)+
  geom_sf(data=dataset_map %>% filter(belt_flag == "Beltline"), aes(fill = Trajectory, color = Trajectory))+
  scale_color_brewer(palette = "Paired")+  
  scale_fill_brewer(palette = "Paired")+
  coord_sf(datum=NA)+
  theme_minimal()+
  theme(legend.position = "none")+
  facet_wrap(~Trajectory)

ggarrange(Vuln_Trajectory, Beltline_Trajectory_Facet, common.legend = TRUE)
ggsave("F5_Beltline_Trajectory.png", plot = last_plot(), device = "png", path = "outputs", dpi = 600)

# BeltLine Housing Trajectory 
Housing_Trajectory<-ggplot()+
  geom_sf(data=dataset_map_Housing %>% filter(belt_flag == "Beltline"), aes(fill = Housing_Trajectory))+
  labs(fill = "Housing Trajectory")+
  scale_fill_brewer(palette = "Paired")+
  coord_sf(datum=NA)+
  theme_minimal()

Beltline_Housing_Trajectory_Facet<-ggplot()+
  geom_sf(data=bg_limits %>% filter(belt_flag == "Beltline"), color = "gray80", fill=NA)+
  geom_sf(data=dataset_map_Housing %>% filter(belt_flag == "Beltline"), aes(fill = Housing_Trajectory, color = Housing_Trajectory))+
  scale_color_brewer(palette = "Paired")+  
  scale_fill_brewer(palette = "Paired")+
  coord_sf(datum=NA)+
  theme_minimal()+
  theme(legend.position = "none")+
  facet_wrap(~Housing_Trajectory)

ggarrange(Housing_Trajectory, Beltline_Housing_Trajectory_Facet, common.legend = TRUE)
ggsave("F5_Beltline_Housing_Trajectory.png", plot = last_plot(), device = "png", path = "outputs", dpi = 600)

# Relationship between Housing Index and Vulnerability Index

SP1<-ggplot(data=dataset_00, aes(x=Index_Vuln, y=Index_Housing, colour = belt_flag))+geom_point(alpha=.4)+
  labs(x="Vulnerability Index", y="Housing Market Index", colour = "Location")+
  ylim(c(0, 700))+
  geom_smooth(method = "lm", se=FALSE)+
  theme_minimal()

SP2<-ggplot(data=dataset_11, aes(x=Index_Vuln, y=Index_Housing, colour = belt_flag))+geom_point(alpha=.4)+
  labs(x="Vulnerability Index", y="Housing Market Index", colour = "Location")+
  ylim(c(0, 700))+
    geom_smooth(method = "lm", se=FALSE)+
  theme_minimal()

SP3<-ggplot(data=dataset_16, aes(x=Index_Vuln, y=Index_Housing, colour = belt_flag))+geom_point(alpha=.4)+
  labs(x="Vulnerability Index", y="Housing Market Index", colour = "Location")+
  ylim(c(0, 700))+
    geom_smooth(method = "lm", se=FALSE)+
  theme_minimal()

ggarrange(SP1, SP2, SP3, labels = c("2000", "2007-2011", "2012-2016"), label.y= 1, ncol = 3, nrow = 1, legend = "right", common.legend = TRUE)
ggsave("F7_IndexScatter.png", plot = last_plot(), device = "png", path = "outputs", dpi = 600)

# DRI
dataset_map_Housing %>% group_by(Trajectory, Housing_Trajectory) %>% count()

DRI_Map<-ggplot()+
  geom_sf(data=bg_limits, color = "gray80", lwd=.1, fill=NA)+
  geom_sf(data=dataset_map_Housing, fill = "blue", color = "blue")+
  geom_sf(data=bg_limits  %>% st_union(), color = "gray40", lwd = .4, fill=NA)+
  labs(x="Vulnerability", y="Housing")+
  coord_sf(datum=NA)+
  theme_minimal()+
  theme(strip.text.x = element_text(size = 6),
        strip.text.y = element_text(size = 6))+
  facet_grid(Trajectory ~ Housing_Trajectory)

DRI_Map_Belt<-ggplot()+
  geom_sf(data=bg_limits %>% filter(belt_flag == "Beltline"), color = "gray80", lwd=.1, fill=NA)+
  geom_sf(data=dataset_map_Housing %>% filter(belt_flag == "Beltline"), fill = "blue", color = "blue")+
  geom_sf(data=bg_limits %>% group_by(belt_flag) %>% summarise(n()) %>% filter(belt_flag == "Beltline"), lwd = .4, fill = NA, colour = "gray30")+
  labs(x="Vulnerability", y="Housing")+
  coord_sf(datum=NA)+
  theme_minimal()+
  theme(strip.text.x = element_text(size = 5),
        strip.text.y = element_text(size = 6))+
  facet_grid(Trajectory ~ Housing_Trajectory)

DRI_Map
ggsave("F9_DRI_MapATL.png", plot = last_plot(), device = "png", path = "outputs", dpi = 600)

DRI_Map_Belt
ggsave("F10_DRI_MapBelt.png", plot = last_plot(), device = "png", path = "outputs", dpi = 600)

# Composite Map
dataset_map_Housing<-dataset_map_Housing %>% mutate(DRI_00 = Index_Vuln_2000+Index_Housing_2000,
                                                    DRI_11 = Index_Vuln_2011+Index_Housing_2011,
                                                    DRI_16 = Index_Vuln_2016+Index_Housing_2016,
                                                    DRI_Cat_00 = ntile(DRI_00, 4),
                                                    DRI_Cat_11 = ntile(DRI_11, 4),
                                                    DRI_Cat_16 = ntile(DRI_16, 4),
                                                    DRI_Cat_00 = case_when(DRI_Cat_00 == "1"~ "Low Displacement Risk",
                                                                           DRI_Cat_00 %in% c("2", "3")~"Moderate Displacement Risk",
                                                                           DRI_Cat_00 %in% "4" ~ "High Displacement Risk"),
                                                    DRI_Cat_11 = case_when(DRI_Cat_11 == "1"~ "Low Displacement Risk",
                                                                           DRI_Cat_11 %in% c("2", "3")~"Moderate Displacement Risk",
                                                                           DRI_Cat_11 %in% "4" ~ "High Displacement Risk"),
                                                    DRI_Cat_16 = case_when(DRI_Cat_16 == "1"~ "Low Displacement Risk",
                                                                           DRI_Cat_16 %in% c("2", "3")~"Moderate Displacement Risk",
                                                                           DRI_Cat_16 %in% "4" ~ "High Displacement Risk")) %>% 
  mutate(DRI_Cat_00 = factor(DRI_Cat_00, levels = c("High Displacement Risk", "Moderate Displacement Risk", "Low Displacement Risk")),
         DRI_Cat_11 = factor(DRI_Cat_11, levels = c("High Displacement Risk", "Moderate Displacement Risk", "Low Displacement Risk")),
         DRI_Cat_16 = factor(DRI_Cat_16, levels = c("High Displacement Risk", "Moderate Displacement Risk", "Low Displacement Risk")))
                             


MDRI_1<-ggplot()+
  geom_sf(data=dataset_map_Housing %>% filter(belt_flag == "Beltline"), aes(fill = DRI_00), color = "gray60")+
  scale_fill_distiller(palette = "RdBu", breaks = c(0, 300, 600, 900, 1200, 1500), labels = c("Lowest","300", "600", "900", "1200", "Highest"), limits =c(0, 1500))+
  labs(fill = "Displacement Risk Index")+
  coord_sf(datum=NA)+
  theme_minimal()+
  theme(legend.position = "none")

MDRI_2<-ggplot()+
  geom_sf(data=dataset_map_Housing %>% filter(belt_flag == "Beltline"), aes(fill = DRI_11), color = "gray60")+
  scale_fill_distiller(palette = "RdBu", breaks = c(0, 300, 600, 900, 1200, 1500), labels = c("Lowest","300", "600", "900", "1200", "Highest"), limits =c(0, 1500))+
  labs(fill = "Displacement Risk Index")+
  coord_sf(datum=NA)+
  theme_minimal()+
  theme(legend.position = "none")

MDRI_3<-ggplot()+
  geom_sf(data=dataset_map_Housing %>% filter(belt_flag == "Beltline"), aes(fill = DRI_16), color = "gray60")+
  scale_fill_distiller(palette = "RdBu", breaks = c(0, 300, 600, 900, 1200, 1500), labels = c("Lowest","300", "600", "900", "1200", "Highest"), limits =c(0, 1500))+
  labs(fill = "Displacement Risk Index")+
  coord_sf(datum=NA)+
  theme_minimal()

MDRI_4<-ggplot()+
  geom_sf(data=dataset_map_Housing %>% filter(belt_flag == "Beltline"), aes(fill = DRI_Cat_00), color = "gray60")+
  scale_fill_brewer(palette = "Pastel1")+
  labs(fill = "DRI Category")+
  coord_sf(datum=NA)+
  theme_minimal()+
  theme(legend.position = "none")

MDRI_5<-ggplot()+
  geom_sf(data=dataset_map_Housing %>% filter(belt_flag == "Beltline"), aes(fill = DRI_Cat_11), color = "gray60")+
  scale_fill_brewer(palette = "Pastel1")+
  labs(fill = "DRI Category")+
  coord_sf(datum=NA)+
  theme_minimal()+
  theme(legend.position = "none")

MDRI_6<-ggplot()+
  geom_sf(data=dataset_map_Housing %>% filter(belt_flag == "Beltline"), aes(fill = DRI_Cat_16), color = "gray60")+
  scale_fill_brewer(palette = "Pastel1")+
  labs(fill = "DRI Category")+
  coord_sf(datum=NA)+
  theme_minimal()


ggarrange(MDRI_1, MDRI_2, MDRI_3, MDRI_4, MDRI_5, MDRI_6, labels = c("2000", "2007-2011", "2012-2016"), label.y= 1.01, ncol = 3, nrow = 2, widths = c(1, 1, 2))
ggsave("F11_DRI_Beltline.png", plot = last_plot(), device = "png", path = "outputs", dpi = 600)


# Save Out Datasets

write_csv(dataset_00 %>% select(-geometry), "outputs/dataset_00.csv")
write_csv(dataset_11%>% select(-geometry), "outputs/dataset_11.csv")
write_csv(dataset_16%>% select(-geometry), "outputs/dataset_16.csv")
write_csv(dataset_map_Housing, "outputs/vulnerability_index.csv")
