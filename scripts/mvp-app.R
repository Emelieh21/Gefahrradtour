library(leaflet)
library(shiny)

# setwd(gsub("/scripts","/data",dirname(rstudioapi::getActiveDocumentContext()$path)))

# Read in accident data
accidents <- read.csv2("AfSBBB_BE_LOR_Strasse_Strassenverkehrsunfaelle_2019_Datensatz.csv", stringsAsFactors = FALSE)

# Define categories of road users
categories <- c("IstRad", "IstPKW", "IstFuss", "IstKrad", "IstGkfz", "IstSonstige")
names(categories) <- c("Bikes", "Cars", "Pedestrians", "Krad", "Gkfz", "Other")

ui <- fluidPage(
  # ================ #
  # 1) Map output ####
  # ================ #
  # Allow map to be 100% of the screen 
  tags$style(type = "text/css", "#map {width:100%; height: calc(93vh) !important;}"),
  leafletOutput("map", width = "100%", height = "100%"),
  
  # ================== #
  # 1) Select Panel ####
  # ================== #
  absolutePanel(id= "selectPanel", top = 40, right = 15, draggable = TRUE,
                # Z-index modification 
                style = "opacity: 0.85; z-index: 1000; font-size:80%; width:400px; padding-left:16px; padding-top:16px; padding-bottom:16px; border-radius: 10px;", 
                # This explodes when trying to plot all in one shot, that's why I don't add multiple select for now...
                selectInput("include", 
                            label = "Show accidents that include...", 
                            choices = categories, 
                            multiple = TRUE)
  )
)

server <- function(input, output, session) {
  # ================ #
  # 1) Map output ####
  # ================ #
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles(group = "Standard.Map") %>%
      addProviderTiles("CartoDB.Positron", group = "CartoDB.Positron") %>%
      addProviderTiles("Stamen.TonerLite", group = "Stamen.TonerLite") %>%
      addProviderTiles("CartoDB.DarkMatter", group = "CartoDB.DarkMatter") %>%
      addLayersControl(position = 'bottomleft',
                       baseGroups = c("Standard.Map",
                                      "CartoDB.Positron",
                                      "Stamen.TonerLite" ,
                                      "CartoDB.DarkMatter"),
                       overlayGroups = c("accidents"),
                       options = layersControlOptions(collapsed = TRUE)) %>%
      setView(13.401011, 52.518913, zoom = 10)
  })
  
  # ============================ #
  # 2) Put selection on a map ####
  # ============================ #
  observeEvent(input$include, ignoreNULL = FALSE, {
    # 2a) Clear shapes ####
    proxy <- leafletProxy("map",session) %>%
      clearGroup(group = "accidents")
    
    # 2b) Filter to selection ####
    selection <- NULL
    for (category in input$include) {
      selection <- unique(c(selection, which(accidents[category] == 1, useNames = TRUE)))
    }
    accidents_sub <- accidents[selection, ]
    
    # 2c) Update map ####
    if (!is.null(selection)) {
      leafletProxy("map", session = session, data = accidents_sub) %>%
        addMarkers(lng = ~XGCSWGS84, lat = ~YGCSWGS84, popup = ~paste("Bike: ",IstRad,"</br>",
                                                                      "Car: ",IstPKW,"</br>",
                                                                      "Pedestrian: ",IstFuss,"</br>",
                                                                      "Krad: ",IstKrad,"</br>",
                                                                      "Gkfz: ",IstGkfz,"</br>",
                                                                      "Other: ",IstSonstige,"</br>"),
                   group = "accidents",
                   popupOptions = popupOptions(autoClose = FALSE, closeOnClick = FALSE),
                   clusterOptions = markerClusterOptions())
    }
  })
}

shinyApp(ui, server)
