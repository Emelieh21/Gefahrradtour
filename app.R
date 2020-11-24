# ========= #
# GLOBAL ####
# ========= #
library(leaflet)
library(shiny)
library(jsonlite)
library(curl)
# Jasmin's packages...
library(tidyverse)
library(lubridate)
library(leaflet)
library(ggplot2)
library(sf)
library(sp)
library(tmap)    # for static and interactive maps
library(stplanr)
library(geojsonR)
library(osmdata)
library(igraph)
library(tidygraph)
library(units)

# 1) Read in accident data ####
accidents <- read.csv2("data/AfSBBB_BE_LOR_Strasse_Strassenverkehrsunfaelle_2019_Datensatz.csv", stringsAsFactors = FALSE)
# source("scripts/routing.R")

# 2) Get an initial point to put on the map (the default here address) ####
token <- readLines("local/local.txt")
url <- paste0("https://geocode.search.hereapi.com/v1/geocode?q=Invalidenstr+117%2C+Berlin&apiKey=",token)
result <- fromJSON(url)

# 3) ColorNumeric for the accident markers ####
pal <- colorNumeric(
  palette = "YlOrRd",
  domain = accidents$UKATEGORIE)

# 4a) Read in roads as sp ####
roads <- rgdal::readOGR("data/berlin-navigableroads-v89-1_3XjFApUX.geojson")

# 4b) Convert to sf ####
road_data <- st_as_sf(roads)

# 4c) set factor for accidents (can be slider input ranging from 0-1) or can be meters that are added to edge length for each accident ####
influence_factor_accidents <- 0.9
add_meters <- 1000

# 4d) Create graph from edges & nodes sf-table ####
edges_nodes_list <- create_edges_nodes(road_data, influence_factor_accidents, add_meters)
edges <- edges_nodes_list[[1]]
nodes <- edges_nodes_list[[2]]
graph <- create_graph(edges, nodes)

# 5) This script uses the functions from 'routing.R' to calculate the shortest path between two input nodes ####
source("scripts/routing.R")

# 6) Initialize reactiveValues ####
values <- reactiveValues(result_a = result)

# ================= #
# USER INTERFACE ####
# ================= #
ui <- fluidPage(theme = "bootstrap-sketchy.css",
  # ================ #
  # 1) Map output ####
  # ================ #
  # Allow map to be 100% of the screen 
  tags$style(type = "text/css", "#map {width:100%; height: calc(93vh) !important;}"),
  leafletOutput("map", width = "100%", height = "100%"),

  # ========== #
  # 2) Logo ####
  # ========== #  
  absolutePanel(id= "selectPanel", top = 30, left = 50, draggable = TRUE,
                # Z-index modification 
                style = "opacity: 0.90; z-index: 1000;",
                # Add logo
                tags$img(src = "logo-name.png", width=200,
                         style = "border-radius: 100%; border: 5px solid #ff675b;")
  ),
  
  # ================== #
  # 3) Select Panel ####
  # ================== #
  absolutePanel(id= "selectPanel", top = 35, right = 40, draggable = TRUE,
                # Z-index modification, etc.
                style = "opacity: 0.85; z-index: 1000; font-size:16px; width:260px; padding-left:16px; 
                         padding-top:16px; padding-bottom:16px; border-radius: 10px; border-style: solid; 
                         border-width: 5px; border-color: #ff675b; background-color: white;",
                # This explodes when trying to plot all in one shot, that's why I don't add multiple select for now...
                textInput("point_a", label = "From", value = "Invalidenstraße 117, Berlin", width = 210),
                # Temporarily show coordinates for Jasmin for debugging
                div(style="width:210px;", verbatimTextOutput("warning_a")),
                textInput("point_b", label = "To", value = "...", width = 210),
                # Temporarily show coordinates for Jasmin for debugging
                div(style="width:210px;", verbatimTextOutput("warning_b")),
                actionButton("go", "Go", style = "background-color: #ff675b; font-size: 16px; font-weight:700; color: white;")
  )
)

# ========= #
# SERVER ####
# ========= #
server <- function(input, output, session) {
  # ================ #
  # 1) Map output ####
  # ================ #
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles("Stamen.TonerLite", group = "Stamen.TonerLite") %>%
      addTiles(group = "Standard.Map") %>%
      addProviderTiles("CartoDB.Positron", group = "CartoDB.Positron") %>%
      addProviderTiles("CartoDB.DarkMatter", group = "CartoDB.DarkMatter") %>%
      addLayersControl(position = 'bottomleft',
                       baseGroups = c("Stamen.TonerLite" ,
                                      "Standard.Map",
                                      "CartoDB.Positron",
                                      "CartoDB.DarkMatter"),
                       overlayGroups = c("accidents", "routePoints"),
                       options = layersControlOptions(collapsed = TRUE)) %>%
      setView(13.401011, 52.518913, zoom = 10) %>% 
      # Add accidents layer as extra information
      addCircleMarkers(group = "accidents", data = accidents, lng = ~XGCSWGS84, lat = ~YGCSWGS84, popup = ~paste("Bike: ",IstRad,"</br>",
                                                                                            "Car: ",IstPKW,"</br>",
                                                                                            "Pedestrian: ",IstFuss,"</br>",
                                                                                            "Krad: ",IstKrad,"</br>",
                                                                                            "Gkfz: ",IstGkfz,"</br>",
                                                                                            "Other: ",IstSonstige,"</br>",
                                                                                            "Category:",UKATEGORIE),
                       popupOptions = popupOptions(autoClose = FALSE, closeOnClick = FALSE),
                       radius = 1.5, color = ~pal(4-UKATEGORIE), weight = 0, fillOpacity = 1,
                       fillColor = ~pal(4-UKATEGORIE)) %>%
      hideGroup("accidents") %>% 
      # Initialize with default address
      addCircleMarkers(group = "routePoints", lng = result$items$position$lng, lat = result$items$position$lat,
                       popup = "You are here", fillColor = "#ff675b", color = "#4d4d4d",
                       weight = 3, fillOpacity = 1, opacity = 1)
  })
  
  # ============================ #
  # 2) Put selection on a map ####
  # ============================ #
  observeEvent(input$go, {
      # 2a) Get coordinates from point A address ####
      url <- paste0("https://geocode.search.hereapi.com/v1/geocode?q=",URLencode(input$point_a),"&apiKey=",token)
      result_a <- fromJSON(url)
      values$result_a <- result_a
      
      # 2b) Get coordinates from point A address ####
      url <- paste0("https://geocode.search.hereapi.com/v1/geocode?q=",URLencode(input$point_b),"&apiKey=",token)
      result_b <- fromJSON(url)
      values$result_b <- result_b
      
      # # Add bounding box to display search area ####
      # if (!is.null(result_a$items$position) & !is.null(result_b$items$position)) {
      #   xmin <- min(result_a$items$position$lng, result_b$items$position$lng)-0.005
      #   xmax <- max(result_a$items$position$lng, result_b$items$position$lng)+0.005
      #   ymin <- min(result_a$items$position$lat, result_b$items$position$lat)-0.005
      #   ymax <- max(result_a$items$position$lat, result_b$items$position$lat)+0.005
      #   
      #   leafletProxy("map", session = session) %>% 
      #     clearGroup(group = "box") %>%
      #     addRectangles(group = "box",
      #       lng1=xmax, lat1=ymax,
      #       lng2=xmin, lat2=ymin,
      #       color = "#ff675b",
      #       fillColor = "transparent"
      #     ) 
      # }
      
      # Set weight name to choose based on which weight shortest path should be calculated
      weight_name <- "length_weighted_exp" # exp() of accident count per street
      
      from_nodexy <- c(result_a$items$position$lng,result_a$items$position$lat)
      to_nodexy <- c(result_b$items$position$lng,result_b$items$position$lat)
      
      # Outputs tidygraph of all edges as list of nodeID pairs
      shortest_path <- get_shortest_path(from_nodexy,to_nodexy, graph, nodes, edges, weight_name)
      
      shortest_path <- shortest_path %>% activate(edges) %>% as.data.frame()
      names(shortest_path$geometry) = NULL
      
      leafletProxy("map", session = session) %>% 
        clearGroup(group = "path") %>%
        addPolylines(data = shortest_path$geometry, color = "#ff675b")
  })
  
  # 2c) React to the change in location A/B ####
  observeEvent({values$result_a
                values$result_b}, {
    # 2d) Clear shapes ####
    proxy <- leafletProxy("map",session) %>%
      clearGroup(group = "routePoints")
    
    # 2e) Put the new position on the map ####
    result_a <- values$result_a
    print(result_a$items$position) # TEST ####
    result_b <- values$result_b
    print(result_b$items$position) # TEST ####
    # Temporily showing coordinates for debugging
    output$warning_a <- renderText(paste0(result_a$items$position$lat,", ",
                                        result_a$items$position$lng))
    output$warning_b <- renderText(paste0(result_b$items$position$lat,", ",
                                        result_b$items$position$lng))
    
    if (!is.null(result_a$items$position)) {
      leafletProxy("map", session = session) %>%
        addCircleMarkers(group = "routePoints", lng = result_a$items$position$lng, lat = result_a$items$position$lat,
                         popup = "You are here", fillColor = "#ff675b", color = "#4d4d4d",
                         weight = 3, fillOpacity = 1, opacity = 1)
    }
    
    if (!is.null(result_b$items$position)) {
      leafletProxy("map", session = session) %>% 
        addCircleMarkers(group = "routePoints", lng = result_b$items$position$lng, lat = result_b$items$position$lat,
                         popup = "You are thinking about going here", fillColor = "#fdfd00", color = "#4d4d4d",
                         weight = 3, fillOpacity = 1, opacity = 1)
    }
  })
  
  # =============================== #
  # 3) React to click on the map ####
  # =============================== #
  observeEvent(input$map_click, {
    newPoint <- input$map_click
    print(newPoint)
    
    # 3b) Clear shapes ####
    proxy <- leafletProxy("map",session) %>%
      clearGroup(group = "newPoint")
    
    # Work in progress... ####
    leafletProxy("map", session = session, data = newPoint) %>%
      addCircleMarkers(group = "newPoint",
        lng = ~newPoint$lng,
        lat = ~newPoint$lat,
        fillColor = "#fdfd00",
        fillOpacity = 1,
        color = "#1c1c1c",
        weight = 3,
        label = "Click me!",
        popup = HTML("<button onclick='Shiny.onInputChange(\"button_click\",  Math.random())' id='add' type='button' class='btn btn-default action-button' style = 'background-color: #ff675b; font-size: 14px; font-weight:700; color: white;'>Go here</button>"))
  })
  
  # ================================ #
  # 4) Add destination from click ####
  # ================================ #
  observeEvent(input$button_click, {
    message("Adding destination")
    destination <- input$map_marker_click
    
    # 4a) Update the destination address that is displayed ####
    url = paste0("https://revgeocode.search.hereapi.com/v1/revgeocode?at=",
                 destination$lat,"%2C",destination$lng,
                 "&lang=en-US&apiKey=",token)
    result <- fromJSON(url)
    updateTextInput(session, "point_b", "To", value = result$items$address$label)
    
    # 4b) Clear shapes ####
    proxy <- leafletProxy("map",session) %>%
      clearGroup(group = "newPoint")
    
    # 4c) Overwrite the values$result_b ####
    values$result_b <- result
  })
}

shinyApp(ui, server)
