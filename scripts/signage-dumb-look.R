library(leaflet)
library(tidyverse)
library(geosphere)
library(ggmap)


setwd("C:/Users/Jordan/Documents/GitHub/Gefahrradtour/")

signs <- rgdal::readOGR("data/berlin-trafficsigns-v89-1_L7GSlZm6.geojson")

signs_df <- as.data.frame(signs)

accidents <- read.csv2("data/AfSBBB_BE_LOR_Strasse_Strassenverkehrsunfaelle_2019_Datensatz.csv", stringsAsFactors = FALSE)

roads <-  rgdal::readOGR("data/berlin-navigableroads-v89-1_3XjFApUX.geojson")

roads_df <-as.data.frame(roads)

##Getting accidents
accidents$dummy <- 1
signs_df$dummy <- 1

bike_accidents <- accidents %>% filter(IstRad == 1)
car_accidents <-accidents %>% filter(IstPKW == 1)
ped_accidents <-accidents %>% filter(IstFuss == 1)
gkfz_accidents <-accidents %>% filter(IstGkfz == 1)
other_accidents <-accidents %>% filter(IstSonstige == 1)

###Make the bike DF
bike_df <- signs_df %>% full_join(bike_accidents, by = c("dummy" = "dummy"))
bike_df <- bike_df %>% 
  mutate(distance = distHaversine(matrix(c(XGCSWGS84,YGCSWGS84),ncol=2),matrix(c(coords.x1,coords.x2),ncol=2))) %>%
  filter(distance <= 20) %>% unique()

###Make the car DF
car_df <- signs_df %>% full_join(car_accidents, by = c("dummy" = "dummy"))
car_df <- car_df %>% 
  mutate(distance = distHaversine(matrix(c(XGCSWGS84,YGCSWGS84),ncol=2),matrix(c(coords.x1,coords.x2),ncol=2))) %>%
  filter(distance <= 20) %>% unique()

###Make the ped DF
ped_df <- signs_df %>% full_join(ped_accidents, by = c("dummy" = "dummy"))
ped_df <- ped_df %>% 
  mutate(distance = distHaversine(matrix(c(XGCSWGS84,YGCSWGS84),ncol=2),matrix(c(coords.x1,coords.x2),ncol=2))) %>%
  filter(distance <= 20) %>% unique()

###Make the gfkz DF
gfkz_df <- signs_df %>% full_join(gkfz_accidents, by = c("dummy" = "dummy"))
gfkz_df <- gfkz_df %>% 
  mutate(distance = distHaversine(matrix(c(XGCSWGS84,YGCSWGS84),ncol=2),matrix(c(coords.x1,coords.x2),ncol=2))) %>%
  filter(distance <= 20) %>% unique()

###Make the gfkz DF
other_df <- signs_df %>% full_join(other_accidents, by = c("dummy" = "dummy"))
other_df <- other_df %>% 
  mutate(distance = distHaversine(matrix(c(XGCSWGS84,YGCSWGS84),ncol=2),matrix(c(coords.x1,coords.x2),ncol=2))) %>%
  filter(distance <= 20) %>% unique()
##combine and unique
df <- bind_rows(bike_df,car_df,ped_df,gfkz_df,other_df) %>% unique()


##Fix danger meaning
df$severity <- ifelse(df$UKATEGORIE == 1, 3,df$UKATEGORIE)
df$severity <- ifelse(df$UKATEGORIE == 3, 1,df$UKATEGORIE)


df %>% ggplot(aes(x = USTUNDE))+
  geom_histogram()

##summarized
sum_df <- df %>% group_by(signType) %>% 
  summarise(sum_accidents = sum(severity),count_accidents = n_distinct(OBJECTID) ,sign_accident_count = n_distinct(id)) %>%
  arrange(desc(sum_accidents))

signs_sum <- signs_df %>% group_by(signType) %>% summarise(sign_count = n())

sum_df <- sum_df %>% left_join(signs_sum)

##create BB from signs
##bbox <- c(13.35926,52.53962),c(13.44915,52.52878),c(13.36634,52.47072),c(13.42270,52.55859)

lat <- signs_df$coords.x1
lon <- signs_df$coords.x2

bbox <- ggmap::make_bbox(lat,lon,f=.05)
##Checking if accidents not near a sign were in BBOX

accidents_in_bbox <- accidents%>% 
  mutate(lat_ok = YGCSWGS84 > bbox[2] & YGCSWGS84 < bbox[4],
         lon_ok = XGCSWGS84 > bbox[1]& XGCSWGS84 < bbox[3] ) %>%
  filter(lat_ok & lon_ok)

accidents_no_sign <- accidents_in_bbox %>% filter(!(OBJECTID %in% df$OBJECTID))


##Checking if accidents not near a sign were in BBOX
accidents_in_bbox_signs <- df%>% 
  mutate(lat_ok = YGCSWGS84 > bbox[2] & YGCSWGS84 < bbox[4],
         lon_ok = XGCSWGS84 > bbox[1]& XGCSWGS84 < bbox[3] ) %>%
  filter(lat_ok & lon_ok)


length(unique(accidents_no_sign$OBJECTID))/length(unique(accidents_in_bbox$OBJECTID))

length(unique(accidents_no_sign$OBJECTID[accidents_no_sign$IstRad == 1]))/length(unique(accidents_in_bbox))

##Bike accidents in bbox
length(unique(accidents_in_bbox$OBJECTID[accidents_in_bbox$IstRad == 1]))
##Quick check on autos and bikes etc.
accidents_in_bbox %>% group_by(IstRad ==1) %>% summarise(autos = sum(IstPKW),
                                                         pedestrians = sum(IstFuss),
                                                         trucks = sum(IstGkfz))