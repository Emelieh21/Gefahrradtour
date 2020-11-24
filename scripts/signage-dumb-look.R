install.packages("tidyverse")
install.packages("leaflet")
install.packages("geosphere")
library(leaflet)
library(tidyverse)
library(geosphere)



setwd("C:/Users/Jordan/Documents/GitHub/Gefahrradtour/")

geo_dat <- jsonlite::read_json("data/berlin-trafficsigns-v89-1_L7GSlZm6.geojson")
signs <- rgdal::readOGR("data/berlin-trafficsigns-v89-1_L7GSlZm6.geojson")

signs_df <- as.data.frame(signs)

accidents <- read.csv2("data/AfSBBB_BE_LOR_Strasse_Strassenverkehrsunfaelle_2019_Datensatz.csv", stringsAsFactors = FALSE)

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


### Looks like 




leaflet(signs_df) %>%
  addTiles() %>%
  addMarkers(lng = ~coords.x1, lat = ~coords.x2, popup = ~paste("Bike: ",IstRad,"</br>",
                                                                "Car: ",IstPKW,"</br>",
                                                                "Pedestrian: ",IstFuss,"</br>",
                                                                "Krad: ",IstKrad,"</br>",
                                                                "Gkfz: ",IstGkfz,"</br>",
                                                                "Other: ",IstSonstige,"</br>"),
             clusterOptions = markerClusterOptions())