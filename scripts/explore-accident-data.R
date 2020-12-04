library(leaflet)

setwd(gsub("/scripts","",dirname(rstudioapi::getActiveDocumentContext()$path)))

accidents <- read.csv2("data/AfSBBB_BE_LOR_Strasse_Strassenverkehrsunfaelle_2019_Datensatz.csv", stringsAsFactors = FALSE)

# For quick statistics
sum(accidents$IstRad)
check <- subset(accidents, IstRad == 1 | IstFuss == 1)

leaflet(accidents[1:1000,]) %>%
  addTiles() %>%
  addMarkers(lng = ~XGCSWGS84, lat = ~YGCSWGS84, popup = ~paste("Bike: ",IstRad,"</br>",
                                                                "Car: ",IstPKW,"</br>",
                                                                "Pedestrian: ",IstFuss,"</br>",
                                                                "Krad: ",IstKrad,"</br>",
                                                                "Gkfz: ",IstGkfz,"</br>",
                                                                "Other: ",IstSonstige,"</br>"),
             clusterOptions = markerClusterOptions())

