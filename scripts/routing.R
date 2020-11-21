#install.packages("cyclestreets")
#library(cyclestreets) # https://cran.r-project.org/web/packages/cyclestreets/cyclestreets.pdf
# network analysis
library(data.table)
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

#https://rviews.rstudio.com/2019/03/06/intro-to-graph-analysis/
# 
# roads_js = FROM_GeoJson(url_file_string = "C:/Users/Jasmin/Documents/Projects/danger-ranger/data")
# 
# roads_js

# ----------------------------------------------------------------------------------
# get road data from osm
# ----------------------------------------------------------------------------------
get_osm_road_data <- function(place_name) {
  # use osmdata package to get all road types for a specific place name
  # input: place name as string, e.g. 'Berlin' or 'Moabit, Berlin'
  
  place_data <- opq(bbox = place_name) %>%
    add_osm_feature(key = 'highway') %>%
    osmdata_sf() %>% 
    osm_poly2line()
    
  
  roads <- place_data$osm_lines %>% 
    select(highway)
  
  return(roads)
}


# ----------------------------------------------------------------------------------
# create edges and nodes tables
# ----------------------------------------------------------------------------------
create_edges_nodes <- function(road_data) {
  
  edges <- road_data %>%
    mutate(edgeID = c(1:n()))
  
  nodes <- edges %>%
    st_coordinates() %>%
    as_tibble() %>%
    rename(edgeID = L1) %>%
    group_by(edgeID) %>%
    slice(c(1, n())) %>%
    ungroup() %>%
    mutate(start_end = rep(c('start', 'end'), times = n()/2))
  
  nodes <- nodes %>%
    mutate(xy = paste(.$X, .$Y)) %>% 
    mutate(nodeID = group_indices(., factor(xy, levels = unique(xy)))) %>%
    select(-xy)
  
  source_nodes <- nodes %>%
    filter(start_end == 'start') %>%
    pull(nodeID)
  
  target_nodes <- nodes %>%
    filter(start_end == 'end') %>%
    pull(nodeID)
  
  edges = edges %>%
    mutate(from = source_nodes, to = target_nodes)
  
  nodes <- nodes %>%
    distinct(nodeID, .keep_all = TRUE) %>%
    select(-c(edgeID, start_end)) %>%
    st_as_sf(coords = c('X', 'Y')) %>%
    st_set_crs(st_crs(edges))
  
  return(list(edges, nodes))
  
}



# ----------------------------------------------------------------------------------
# create graph
# ----------------------------------------------------------------------------------

create_graph <- function(edges, nodes)
  graph = tbl_graph(nodes = nodes, edges = as_tibble(edges), directed = FALSE)
  
  graph <- graph %>%
    activate(edges) %>%
    mutate(length = st_length(geometry)) # here we could modify and add safety factor
  
  return(graph)

# ----------------------------------------------------------------------------------
# calculate shortest bath between two nodes
# ----------------------------------------------------------------------------------

get_shortest_path <- function(from_node, to_node, graph, nodes, edges) {
  # function to calculate shortest path between two coordinates
  # note: currently it can only search between nodes in the streetnetwork
  # therefore, for input nodes, it first searches for nearest node in street (so an interseciton)
  # and starts calculating from there
  
  # input: from_node & to_node, numeric vectors c(lon,lat)
  
  # output: tidygraph of all edges (list of nodeID pairs)
  
  
  df <- data.frame("nodeID" = c(99999999, 99999998), "lon" = c(from_node[1],to_node[1]), "lat" = c(from_node[2],to_node[2]))
  df_sf <- df
  coordinates(df_sf) <- c("lon", "lat")
  df_sf <- st_as_sf(df_sf, coords = c("lon", "lat"))
  
  fromto_df <- df_sf %>%  st_set_crs(st_crs(nodes))
  print(fromto_df)
  nearest <- st_nearest_feature(fromto_df, nodes)
  print(nearest)
  print("get path")
  
  path <- shortest_paths(
    graph = graph,
    from = nearest[1],
    to = nearest[2],
    output = 'both',
    weights = graph %>% activate(edges) %>% pull(length)
  )
  
  # create graph from shortest path
  path_graph <- graph %>%
    subgraph.edges(eids = path$epath %>% unlist()) %>%
    as_tbl_graph()
  
  print("get path length")
  
  # get path length 
  path_length <- path_graph %>%
    activate(edges) %>%
    as_tibble() %>%
    summarise(length = sum(length))
  
  # path$vpath look at nodes in path
  print(paste0("Path length is ", path_length, " meters."))
  
  # create graph from shortest path
  shortest_path_graph <- graph %>%
    subgraph.edges(eids = path$epath %>% unlist()) %>%
    as_tbl_graph()
  
  return(shortest_path_graph)
}


  

