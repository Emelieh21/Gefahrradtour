library(leaflet)
library(tidyverse)
library(geosphere)


signs <- rgdal::readOGR("data/berlin-trafficsigns-v89-1_L7GSlZm6.geojson")

signs_df <- as.data.frame(signs)


signs_spread <- signs_df %>%
  select(id,signType,coords.x1,coords.x2) %>%
  mutate(values = 1)%>%
  pivot_wider(id_cols = c(id,coords.x1,coords.x2),names_from=signType,values_from = values)



leaflet(signs_spread) %>%
  addTiles() %>%
  addMarkers(lng = ~coords.x1, lat = ~coords.x2, popup = ~paste("Yield: ",Yield,"</br>",
                                                                "Stop: ",Stop,"</br>",
                                                                "SchoolZone: ",SchoolZone,"</br>",
                                                                "TramwayCrossing: ",TramwayCrossing,"</br>",
                                                                "RailwayCrossingUnprotected: ",RailwayCrossingUnprotected,"</br>",
                                                                "CrossingWithPriorityFromTheRight: ",CrossingWithPriorityFromTheRight,"</br>"),
             clusterOptions = markerClusterOptions())