# ========= #
# GLOBAL ####
# ========= #
library(leaflet)
library(shiny)
library(jsonlite)
library(curl)
library(shinyWidgets)
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
library(geosphere)

# 1) Read in accident data ####
accidents <- read.csv2("data/AfSBBB_BE_LOR_Strasse_Strassenverkehrsunfaelle_2019_Datensatz.csv", stringsAsFactors = FALSE)

# 2) Get an initial point to put on the map (the default here address) ####
token <- readLines("local/local.txt")
url <- paste0("https://geocode.search.hereapi.com/v1/geocode?q=Invalidenstr+117%2C+Berlin&apiKey=",token)
result <- fromJSON(url)

# 3) ColorNumeric for the accident markers ####
pal <- colorNumeric(
  palette = "YlOrRd",
  domain = accidents$UKATEGORIE)

# 4) Read in routing functions ####
source("scripts/routing-app.R")

# 5) Berlin roads: create graph from edges & nodes sf-table ####
edges <- readRDS("data/edges.rds")
nodes <- readRDS("data/nodes.rds")
graph <- create_graph(edges, nodes)

# 6) Initialize reactiveValues ####
values <- reactiveValues(result_a = result)

# 7) Bbox to indicate the limits of our HERE roads data (roads@bbox) ####
xmin = 13.35697 
ymin = 52.47071 
xmax = 13.44962 
ymax = 52.56063

# 8) Flag to know when to hide a newPoint ####
pointVisible = FALSE

# 9) Make it clear that the app currently only works for a limited area ####
# Make a polygon for the whole world
s <- rbind(c(-90,-180), c(-90,180), c(90,180), c(90,-180), c(-90,-180))
s.sf <-st_sfc(st_polygon(list(s)))
s.pol = st_sf(ID = "sq", s.sf)

# Make a polygon for our HERE data boundaries
s2 <- rbind(c(xmin,ymin), c(xmin,ymax), c(xmax,ymax), c(xmax,ymin), c(xmin,ymin))
s2.sf <-st_sfc(st_polygon(list(s2)))
s2.pol = st_sf(ID = "tr", s2.sf)

# Find the 'difference', i.e. reverse of st_intersection
t <- st_difference(s.pol,s2.pol)

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
  
  # Set slider color to our coporate colors (see https://stackoverflow.com/questions/36906265/how-to-color-sliderbar-sliderinput)
  tags$head(tags$style(HTML('.js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
                                                background: #ff675b;
                                                border-top: 1px solid #ff675b;
                                                border-bottom: 1px solid #ff675b;}

                          /* changes the colour of the number tags */
                         .irs-from, .irs-to, .irs-single { background: #ff675b }'
  ))
  ),

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
                
                # 3a) Point A select ####
                textInput("point_a", label = "From", value = "Invalidenstraße 117, Berlin", width = 210),
                # Show warning for invalid address
                div(style="width:210px;", uiOutput("warning_a")),
                
                # 3b) Point B select ####
                textInput("point_b", label = "To", value = "...", width = 210),
                # Show warning for invalid address
                div(style="width:210px;", uiOutput("warning_b")),
                
                # 3c) How dangerous do you want it slider ####
                div(style="margin-top:15px;",
                    sliderTextInput("danger_range", width = "90%",
                                    label = "Route safety",
                                    choices = c("Very dangerous route", "Very safe route"),
                                    force_edges = TRUE
                                    )
                   ),
                
                # 3d) LET'S GO! ####
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
                       overlayGroups = c("accidents", "box", "path", "routePoints"),
                       options = layersControlOptions(collapsed = TRUE)) %>%
      setView(lat = 52.52082, lng = 13.39718, zoom = 14) %>% 
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
      addCircleMarkers(group = "routePoints", lng = result$items$position$lng[1], lat = result$items$position$lat[1],
                       popup = "You are here", fillColor = "#ff675b", color = "#4d4d4d",
                       weight = 3, fillOpacity = 1, opacity = 1) %>%
      addPolygons(data = st_geometry(t)[[1]], 
                  fillColor = "#7d7d7d", 
                  opacity = 0.3, 
                  stroke = FALSE,
                  popup = "Gefahrradtour is not available for this area yet")
  })
  
  # ==================== #
  # 2) Plot the route ####
  # ==================== #
  observeEvent(input$go, {
      # 2a) Get coordinates from point A address ####
      url <- paste0("https://geocode.search.hereapi.com/v1/geocode?q=",URLencode(input$point_a),"&apiKey=",token)
      result_a <- fromJSON(url)
      values$result_a <- result_a
      
      # 2b) Get coordinates from point A address ####
      url <- paste0("https://geocode.search.hereapi.com/v1/geocode?q=",URLencode(input$point_b),"&apiKey=",token)
      result_b <- fromJSON(url)
      values$result_b <- result_b
      
      # 2b1) Check if point_a is valid ####
      if (!length(result_a$items$position) > 0) {
        valid_a = FALSE
        output$warning_a <- renderText(HTML("<em style='color: #ba4a41;'>You location is invalid.</em>"))
      } else {
        # Check if it is within our tiny available area
        if (length(st_intersection(st_point(c(result_a$items$position$lng[1],result_a$items$position$lat[1])),s2.pol)) < 1) {
          valid_a = FALSE
          output$warning_a <- renderText(HTML("<em style='color: #ba4a41;'>You location is outside the searchable area.</em>"))
        } else {
          valid_a = TRUE
          output$warning_a <- renderUI({return(NULL)})
        }
      }
      
      # 2b2) Check if point_b is valid ####
      if (!length(result_b$items$position) > 0) {
        valid_b = FALSE
        output$warning_b <- renderUI(HTML("<em style='color: #ba4a41;'>You location is invalid.</em>"))
      } else {
        # Check if it is within our tiny available area
        if (length(st_intersection(st_point(c(result_b$items$position$lng[1],result_b$items$position$lat[1])),s2.pol)) < 1) {
          valid_b = FALSE
          output$warning_b <- renderUI(HTML("<em style='color: #ba4a41;'>You location is outside the searchable area.</em>"))
        } else {
          valid_b = TRUE
          output$warning_b <- renderUI({return(NULL)})
        }
      }
      shiny::validate(need(valid_a & valid_b, "Invalid position in input"))

      # Set weight name to choose based on which weight shortest path should be calculated
      if (input$danger_range == "Very dangerous route") {
        weight_name <- "length_edge" 
      } else {
        weight_name <- "length_weighted_m" 
      }
      
      from_nodexy <- c(result_a$items$position$lng[1],result_a$items$position$lat[1])
      to_nodexy <- c(result_b$items$position$lng[1],result_b$items$position$lat[1])
      
      # Outputs tidygraph of all edges as list of nodeID pairs
      shortest_path <- get_shortest_path(from_nodexy,to_nodexy, graph, nodes, edges, weight_name)
      shortest_path <- shortest_path %>% activate(edges) %>% as.data.frame()
      names(shortest_path$geometry) = NULL
      
      # In Copenhagen the average biking speed is apparently 15.5 km /h - I found that plausible
      minPerKm =  60/15.5
      routeDistance = as.numeric(round(sum(shortest_path$length_edge)/1000,1))
      routeTime = round(routeDistance * minPerKm, 0)
      accidentsOnRoute = sum(shortest_path$accidents_count)
      
      zoom_lat <- mean(result_a$items$position$lat[1],result_b$items$position$lat[1])
      zoom_lng <- mean(result_a$items$position$lng[1],result_b$items$position$lng[1])
      
      pal <- colorNumeric(c("#d6ff30","#fdfd00","#ff675b","#ba4a41"), shortest_path$accidents_count_weighted)
      
      leafletProxy("map", session = session) %>% 
        setView(lat = zoom_lat, lng = zoom_lng, zoom = 13) %>% 
        clearGroup(group = "path") %>%
        hideGroup("routePoints") %>%
        addPolylines(group = "path", data = shortest_path$geometry, 
                     color = pal(shortest_path$accidents_count_weighted),
                     opacity = 0.9,
                     popup = paste0("<b>Distance</b>: ",routeDistance," km</br>",
                                    "<b>Travel Time</b>: ",routeTime," min</br>",
                                    "<b>Total accidents</b>: ",accidentsOnRoute," (2019)</br>",
                                    "<b>Accidents at this point: </b>",shortest_path$accidents_count)) %>%
        # Together with hideGroup before, stupid work around to display the routePoints on top of the line...
        showGroup("routePoints")
  })
  
  # 2c) React to the change in location A/B ####
  observeEvent({values$result_a
                values$result_b
                # See this link to explain the 1: https://stackoverflow.com/questions/34731975/how-to-listen-for-more-than-one-event-expression-within-a-shiny-eventreactive-ha
                1}, {
    # 2d) Clear shapes ####
    proxy <- leafletProxy("map",session) %>%
      clearGroup(group = c("routePoints"))
    
    # 2e) Put the new position on the map ####
    result_a <- values$result_a
    print(result_a$items$position) 
    result_b <- values$result_b
    print(result_b$items$position)
    
    if (!is.null(result_a$items$position)) {
      leafletProxy("map", session = session) %>%
        addCircleMarkers(group = "routePoints", lng = result_a$items$position$lng[1], lat = result_a$items$position$lat[1],
                         popup = "You are here", fillColor = "#ff675b", color = "#4d4d4d",
                         weight = 3, fillOpacity = 1, opacity = 1)
    }
    
    if (!is.null(result_b$items$position)) {
      leafletProxy("map", session = session) %>% 
        addCircleMarkers(group = "routePoints", lng = result_b$items$position$lng[1], lat = result_b$items$position$lat[1],
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
    
    # Create a new point on the map if there isn't one, else remove the current point (otherwise you always have an annoying point on the map)
    if (pointVisible == FALSE) {
      # 3a) Set flag to TRUE
      pointVisible <<- TRUE
      
      # 3b) Clear shapes ####
      proxy <- leafletProxy("map",session) %>%
        clearGroup(group = "newPoint")
      
      # 3c) Add new point to the map ####
      leafletProxy("map", session = session, data = newPoint) %>%
        addCircleMarkers(group = "newPoint",
          lng = ~newPoint$lng,
          lat = ~newPoint$lat,
          fillColor = "#fdfd00",
          fillOpacity = 1,
          color = "#1c1c1c",
          weight = 3,
          label = "Click me!",
          popup = HTML(paste0("<button onclick='Shiny.onInputChange(\"add_origin\",  Math.random())' id='add1' type='button' class='btn btn-default action-button' style = 'background-color: #ff675b; font-size: 14px; font-weight:700; color: white; margin-bottom:5px;'>Add as origin</button></br>",
                              "<button onclick='Shiny.onInputChange(\"add_destination\",  Math.random())' id='add2' type='button' class='btn btn-default action-button' style = 'background-color: #ff675b; font-size: 14px; font-weight:700; color: white;'>Add as destination</button>")))
    } else {
      # 3a) Set flag to FALSE
      pointVisible <<- FALSE
      
      # 3b) Clear shapes ####
      proxy <- leafletProxy("map",session) %>%
        clearGroup(group = "newPoint")
      }
    })
  
  # ================================ #
  # 4) Add destination from click ####
  # ================================ #
  observeEvent(input$add_destination, {
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
      clearGroup(group = c("newPoint","path"))
    
    # 4c) Overwrite the values$result_b ####
    values$result_b <- result
  })
  
  # ================================ #
  # 5) Add origin from click ####
  # ================================ #
  observeEvent(input$add_origin, {
    message("Adding origin")
    origin <- input$map_marker_click
    
    # 5a) Update the origin address that is displayed ####
    url = paste0("https://revgeocode.search.hereapi.com/v1/revgeocode?at=",
                 origin$lat,"%2C",origin$lng,
                 "&lang=en-US&apiKey=",token)
    result <- fromJSON(url)
    updateTextInput(session, "point_a", "From", value = result$items$address$label)
    
    # 5b) Clear shapes ####
    proxy <- leafletProxy("map",session) %>%
      clearGroup(group = c("newPoint","path"))
    
    # 5c) Overwrite the values$result_a ####
    values$result_a <- result
  })
}

shinyApp(ui, server)
