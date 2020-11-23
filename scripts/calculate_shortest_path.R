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
# Note: This code is based on this tutorial: https://www.r-spatial.org/r/2019/09/26/spatial-networks.html

# get road data from HERE navigable roads for Berlin

# read in file
setwd(gsub("/scripts","",dirname(rstudioapi::getActiveDocumentContext()$path)))

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
glimpse(edges)

graph <- create_graph(edges, nodes)

# calculate shortest path ------------------------------------------------

# input from & to coordinates as numeric vector c(lon,lat)

# Some examples for Berlin Mitte
# from_nodexy <- c( 13.425545 , 52.519306)
# from_nodexy <- c(13.399466,52.531503)
from_nodexy <- c( 13.407660 ,52.521688)
#to_nodexy <- c(13.412344, 52.524003)
# from_nodexy <- c(13.391048 , 52.516999)
# to_nodexy <- c(  13.387454 ,52.516836)
# to_nodexy <- c( 13.381654 ,  52.509990)
 to_nodexy <- c( 13.402157 ,52.523745)
# to_nodexy <- c(13.394460,52.501298)
# 
# from_nodexy <- c( 13.390905 , 52.515808)
# to_nodexy <- c( 13.391495 ,52.512276)
# 
# # Examples for Friedrichshain mainstreet
# from_nodexy <- c(13.432853, 52.517991)
# to_nodexy <- c(13.466831, 52.514707)
# 
# from_nodexy <- c(9.157345,47.686012)
# #from_nodexy <- c(9.164507,47.688087)
# #from_nodexy <- c(9.154614,47.683042)
# to_nodexy <- c(9.159216,47.679538)

# set weight name to choose based on which weight shortest path should be calculated
weight_nameex <- "length_weighted_exp" # exp() of accident count per street
  # for this we need "influence_factor_accidents" --> value between 1-100 (percent), how much should safety count
weight_namem <- "length_weighted_m" # add additional meters to street length for each accident
  # for these we need "add_meters"
weight_name <- "length_edge" # normal length of a street

# from_node <- from_nodexy
# to_node <- to_nodexy
# caluclate shortest path
## outputs tidygraph of all edges as list of nodeID pairs
shortest_path <- get_shortest_path(from_nodexy,to_nodexy, graph, nodes, edges, weight_name )
shortest_pathex <- get_shortest_path(from_nodexy,to_nodexy, graph, nodes, edges, weight_nameex )
shortest_pathm <- get_shortest_path(from_nodexy,to_nodexy, graph, nodes, edges, weight_namem )



# ggplots  ------------------------------------------------------------------------
# some ggplots to check the data

fromto_df <- fromto_xy_tosf(from_nodexy, to_nodexy)

# crop edges to small area around paths to see better
box = c(xmin = min(fromto_df$lon)-0.03, 
        ymin = min(fromto_df$lat)-0.03, 
        xmax = max(fromto_df$lon)+0.03, 
        ymax = max(fromto_df$lat)+0.03)
edges_small <- st_crop(edges, box)
nodes_small <- st_crop(nodes, box)
  
# plot streets/edges in graph & all shortest_path variants
ggplot() +
  # geom_sf(data = graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), col = 'darkgrey') +
  #geom_sf(data = graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), col = 'red', size = 0.5) #+
  #geom_sf(data = edges_small, aes(geometry = geometry), col = 'darkgrey') +
  geom_sf(data = shortest_pathex %>% activate(edges) %>% as_tibble() %>% st_as_sf(), lwd = 1, col = 'orange') +
  geom_sf(data = shortest_pathm %>% activate(edges) %>% as_tibble() %>% st_as_sf(), lwd = 2, col = 'red') +
  geom_sf(data = shortest_path %>% activate(edges) %>% as_tibble() %>% st_as_sf(), lwd = 2, col = 'green')  +
  geom_sf(data = fromto_df, aes(geometry = geometry),col = 'blue') 



# plot all available streets in graph & start and stop points I want to connect
ggplot() +
  geom_sf(data = edges_small, aes(geometry = geometry), col = 'darkgrey') +
  geom_sf(data = nodes_small, aes(geometry = geometry), col = 'red') +
  #geom_sf(data = graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), aes(geometry = geometry), col = 'darkgrey') +
  #geom_sf(data = graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), aes(geometry = geometry), col = 'orange') +
  # geom_sf(data = nodes[,"nodeID" == 13246], aes(geometry = geometry),col = 'red')  + # this is the node that is closest to our input point
  # geom_sf(data = nodes[,"nodeID" == 10502], aes(geometry = geometry),col = 'red')  
  geom_sf(data = fromto_df, aes(geometry = geometry),col = 'blue', size = 2)  


# plot road data, from and to node, and shortest path
ggplot() +
  geom_sf(data = graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), col = 'darkgrey') +
  geom_sf(data = graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), col = 'red', size = 0.5) +
  geom_sf(data = shortest_path %>% activate(edges) %>% as_tibble() %>% st_as_sf(), lwd = 2, col = 'blue') #+
  #geom_sf(data = shortest_path %>% activate(nodes) %>% filter(nodeID %in% c(from_node, to_node)) %>% as_tibble() %>% st_as_sf(), size = 2)  +
  #geom_sf(data = fromto_df, color = 'red', size = 4)


# plot edges coloured by number of accidents
quantile(edges$accidents_count)
# very skewed distribution, therefore on map everything in same color, does not show much
ggplot() +
  geom_sf(data = edges, aes(geometry = geometry, color = accidents_count_weighted), show.legend = "line") +
  scale_color_continuous(low = "#ffa500",high = "#005f6a")

  
# plot edges coloured by accident "yes" or "no"
ggplot() +
  geom_sf(data = edges, aes(geometry = geometry, color = factor(accident_happened)), show.legend = "line") +
  scale_color_manual(values= c("#005f6a","#ffa500"))

  


# explanation influence factor of safety on shortest route ------------------------


# function: exp(influence_factor_accidents) * street_length in meters

# Why exp()?
perc_df <- data.frame("percent" = seq(0,1,0.01))
ggplot(perc_df) +
  geom_point(aes(x=percent, y= exp(percent)), color = "red") #+
  geom_point(aes(x=percent, y= exp(percent*4)), color = "orange") +
  geom_point(aes(x=percent, y= exp(percent*5)), color = "yellow") +
  geom_point(aes(x=percent, y= exp(percent*6)), color = "blue") 
# --> this why we have steepr increase in mulitplying factor and towards 100% safety it gets more and more important
# the result of exp(percentage) is multiplied with length of street, ranges from 1 to any value depending on exp function

# maybe interesting for later ------------------------
## possible preprocessing step if it gets too slow later
# matrix with all shortest paths from all vertices to all vertices
distances <- distances(
  graph = graph,
  weights = graph %>% activate(edges) %>% pull(length_edge) # set weight of graph as edge length
)

