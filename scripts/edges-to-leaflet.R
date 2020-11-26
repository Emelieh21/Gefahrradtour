## this script uses the functions from 'routing.R' to calculate the shortest path between two input nodes
setwd(gsub("/scripts","",dirname(rstudioapi::getActiveDocumentContext()$path)))
source("scripts/routing.R")

# Some examples for Berlin Mitte
# from_nodexy <- c( 13.425545 , 52.519306)
# from_nodexy <- c(13.399466,52.531503)
# from_nodexy <- c( 13.407660 ,52.521688)
#to_nodexy <- c(13.412344, 52.524003)
from_nodexy <- c(13.391048 , 52.516999)
from_node = c(13.391048 , 52.516999)
# to_nodexy <- c(  13.387454 ,52.516836)
# to_nodexy <- c( 13.381654 ,  52.509990)
# to_nodexy <- c( 13.402157 ,52.523745)
# to_nodexy <- c(13.394460,52.501298)
# from_nodexy <- c(9.154614,47.683042)
# to_nodexy <- c(9.159216,47.679538)
# from_nodexy <- c(13.38527,52.53041)
# to_nodexy <- c(13.39643,52.53814)
# to_nodexy <- c(13.39555, 52.51785)
# from_nodexy <- c(13.43744,52.50723)
to_nodexy <- c(13.43529,52.5085)
to_node = c(13.43529,52.5085)

# read in as sp
roads <- rgdal::readOGR("data/berlin-navigableroads-v89-1_3XjFApUX.geojson")

# convert to sf
road_data <- st_as_sf(roads)

## this script uses the functions from 'routing.R' to calculate the shortest path between two input nodes
source("scripts/routing.R")

# get road data from osm as graph  -------------------------------------------------------------------
#road_data <- get_osm_road_data("Friedrichshain, Berlin") 
## we are not doing this because graph is very disconnected and not routable, maybe clean up and revisit later

# create final graph of road dataset, used for shortest path search  ------------------------

# set factor for accidents (can be slider input ranging from 0-1)
# or can be meters that are added to edge length for each accident
influence_factor_accidents <- 0.9
add_meters <- 300

# create graph from edges & nodes sf-table
edges_nodes_list <- create_edges_nodes(road_data, influence_factor_accidents, add_meters)
edges <- edges_nodes_list[[1]]
nodes <- edges_nodes_list[[2]]

# so we don't need to do this each time in the app
saveRDS(edges, "data/edges.rds")
saveRDS(nodes, "data/nodes.rds")

glimpse(edges)

graph <- create_graph(edges, nodes)

# calculate shortest path ------------------------------------------------

# set weight name to choose based on which weight shortest path should be calculated
weight_name <- "length_weighted_exp" # exp() of accident count per street
# for this we need "influence_factor_accidents" --> value between 1-100 (percent), how much should safety count
weight_name <- "length_weighted_m" # add additional meters to street length for each accident
# for these we need "add_meters"
weight_name <- "length_edge" # normal length of a street

# from_node <- from_nodexy
# to_node <- to_nodexy
# caluclate shortest path
## outputs tidygraph of all edges as list of nodeID pairs
shortest_path <- get_shortest_path(from_nodexy,to_nodexy, graph, nodes, edges, weight_name)

shortest_path <- shortest_path %>% activate(edges) %>% as.data.frame()
names(shortest_path$geometry) = NULL

# In Copenhagen the average biking speed is apparently 15.5 km /h - I found that plausible
minPerKm =  60/15.5
routeDistance = as.numeric(round(sum(shortest_path$length_edge)/1000,1))
routeTime = round(routeDistance * minPerKm, 0)
accidentsOnRoute = sum(shortest_path$accidents_count)

shortest_path$geometry %>% 
  leaflet() %>% 
  addTiles() %>%
  addPolylines(popup = paste0("<b>Distance</b>: ",routeDistance," km</br>",
                              "<b>Travel Time</b>: ",routeTime," min</br>",
                              "<b>Accidents</b>: ",accidentsOnRoute, " (2019)"))

                  
                            
