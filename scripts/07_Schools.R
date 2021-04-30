library(sf)
library(tidyverse)

# Get Schools Data (2000-2001)
bg_limits<-st_read("data_raw/Geography/BG_Atlanta_City_Limits.shp") %>% select(GEOID)
bg_cent<-st_centroid(bg_limits)

schools00<-st_read("data_raw/School Poverty/School_Poverty_00-01.shp") %>% filter(Latitude != 0)
st_crs(schools00)<-st_crs(bg_limits)

schools00<-st_filter(schools00, bg_limits, join = st_within)

test<-st_nearest_feature(bg_cent, schools00) %>% as.data.frame()
names(test)[1]<-"index"
test$GEOID<-bg_cent$GEOID
test$Eligible_R<-schools00$Eligible_R[test$index]
test$Eligible_F<-schools00$Eligible_F[test$index]
test$Total_Stud<-schools00$Total_Stud[test$index]
test<-test %>% mutate(Eligible_FR = (Eligible_F+Eligible_R)/Total_Stud, Eligible_R = Eligible_R/Total_Stud, Eligible_F = Eligible_F/Total_Stud)
test<-test %>% select(GEOID, Eligible_F, Eligible_R, Eligible_FR)

schools_00<-left_join(bg_limits, test, by="GEOID")
schools_00<-schools_00 %>% st_set_geometry(NULL)
rm(bg_cent, bg_limits, schools00, test)

# Get Schools Data (2010-2011)
bg_limits<-st_read("data_raw/Geography/BG_Atlanta_City_Limits.shp") %>% select(GEOID)
bg_cent<-st_centroid(bg_limits)

schools11<-st_read("data_raw/School Poverty/School_Poverty_10-11.shp") %>% filter(Latitude != 0)
st_crs(schools11)<-st_crs(bg_limits)

schools11<-st_filter(schools11, bg_limits, join = st_within)

test<-st_nearest_feature(bg_cent, schools11) %>% as.data.frame()
names(test)[1]<-"index"
test$GEOID<-bg_cent$GEOID
test$Eligible_R<-schools11$Eligible_R[test$index]
test$Eligible_F<-schools11$Eligible_F[test$index]
test$Total_Stud<-schools11$Total_Stud[test$index]
test<-test %>% mutate(Eligible_FR = (Eligible_F+Eligible_R)/Total_Stud, Eligible_R = Eligible_R/Total_Stud, Eligible_F = Eligible_F/Total_Stud)
test<-test %>% select(GEOID, Eligible_F, Eligible_R, Eligible_FR)

schools_11<-left_join(bg_limits, test, by="GEOID")
schools_11<-schools_11 %>% st_set_geometry(NULL)
rm(bg_cent, bg_limits, schools11, test)



# Get Schools Data (2015-2016)
bg_limits<-st_read("data_raw/Geography/BG_Atlanta_City_Limits.shp") %>% select(GEOID)
bg_cent<-st_centroid(bg_limits)

schools<-st_read("data_raw/School Poverty/School_Poverty_15-16.shp") %>% filter(Latitude != 0)
st_crs(schools)<-st_crs(bg_limits)

schools<-st_filter(schools, bg_limits, join = st_within)

test<-st_nearest_feature(bg_cent, schools) %>% as.data.frame()
names(test)[1]<-"index"
test$GEOID<-bg_cent$GEOID
test$Eligible_R<-schools$Eligible_R[test$index]
test$Eligible_F<-schools$Eligible_F[test$index]
test$Total_Stud<-schools$Total_Stud[test$index]
test$PvtyCount<-schools$PvtyCount[test$index]
test<-test %>% mutate(Eligible_FR = (Eligible_F+Eligible_R)/Total_Stud, Eligible_R = Eligible_R/Total_Stud, Eligible_F = Eligible_F/Total_Stud)
test<-test %>% select(GEOID, Eligible_F, Eligible_R, Eligible_FR)

schools_16<-left_join(bg_limits, test, by="GEOID")
schools_16<-schools_16 %>% st_set_geometry(NULL)
rm(bg_cent, bg_limits, schools, test)
