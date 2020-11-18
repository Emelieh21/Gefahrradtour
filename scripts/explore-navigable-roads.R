library(leaflet)

setwd(gsub("/scripts","",dirname(rstudioapi::getActiveDocumentContext()$path)))

roads <- rgdal::readOGR("data/berlin-navigableroads-v89-1_3XjFApUX.geojson")
pal <- colorNumeric("viridis", NULL)

leaflet(roads) %>%
  addTiles() %>%
  addPolylines(stroke = TRUE, weight = 4, color = ~pal(speedCategory),
               popup = ~paste("category: ",category,"</br>",
                              "speedLimitFrom: ",speedLimitFrom,"</br>",
                              "speedLimitTo: ",speedLimitTo,"</br>",
                              "endNode: ", endNode,"</br>",
                              "startNode: ",startNode))
  
