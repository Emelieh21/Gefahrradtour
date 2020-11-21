#install.packages("cyclestreets")
#library(cyclestreets) # https://cran.r-project.org/web/packages/cyclestreets/cyclestreets.pdf
# network analysis
library(ggmap)
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
get_osm_road_data <- function(place_name)

kn <- opq(bbox = 'Konstanz Fuerstenberg') %>%
  add_osm_feature(key = 'highway') %>%
  osmdata_sf() %>% 
  osm_poly2line()

kn_lines <- kn$osm_lines %>% 
  select(highway)

ggplot(data = kn_lines) + geom_sf()

# ----------------------------------------------------------------------------------
# create edges and nodes tables
# ----------------------------------------------------------------------------------

edges <- kn_lines %>%
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

# ----------------------------------------------------------------------------------
# create graph
# ----------------------------------------------------------------------------------

graph = tbl_graph(nodes = nodes, edges = as_tibble(edges), directed = FALSE)

graph <- graph %>%
  activate(edges) %>%
  mutate(length = st_length(geometry)) # here we could modify and add safety factor

# ----------------------------------------------------------------------------------
# calculate shortest paths
# ----------------------------------------------------------------------------------

# matrix with all shortest paths from all vertices to all vertices
distances <- distances(
  graph = graph,
  weights = graph %>% activate(edges) %>% pull(length) # set weight of graph as edge length
)

# ----------------------------------------------------------------------------------
# calculate shortest bath between two nodes
# ----------------------------------------------------------------------------------

from_node <- graph %>%
  activate(nodes) %>%
  filter(nodeID == 3) %>%
  pull(nodeID)

to_node <- graph %>%
  activate(nodes) %>%
  filter(nodeID == 60) %>%
  pull(nodeID)

#from_nodexy <- c(9.172404,47.688852)
from_nodexy <- c(9.157345,47.686012)
#from_nodexy <- c(9.164507,47.688087)
#from_nodexy <- c(9.154614,47.683042)
to_nodexy <- c(9.159216,47.679538)

nodes_test <- nodes[1:10,]

df <- data.frame("nodeID" = c(99999999, 99999998), "lon" = c(from_nodexy[1],to_nodexy[1]), "lat" = c(from_nodexy[2],to_nodexy[2]))
df_sf <- df
coordinates(df_sf) <- c("lon", "lat")
df_sf <- st_as_sf(df_sf, coords = c("lon", "lat"))

fromto_df <- df_sf %>%  st_set_crs(st_crs(nodes))
fromto_df

ggplot() +
  geom_sf(data = graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), col = 'darkgrey') +
  #geom_sf(data = graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), col = 'blue') +
  geom_sf(data = fromto_df, color = 'red', size = 4)
  

nearest <- st_nearest_feature(fromto_df, nodes)
nearest


path <- shortest_paths(
  graph = graph,
  from = nearest[1],
  to = nearest[2],
  output = 'both',
  weights = graph %>% activate(edges) %>% pull(length)
)

path$vpath

# create graph from shortest path
path_graph <- graph %>%
  subgraph.edges(eids = path$epath %>% unlist()) %>%
  as_tbl_graph()

path_graph

# get path length 
path_graph %>%
  activate(edges) %>%
  as_tibble() %>%
  summarise(length = sum(length))

ggplot() +
  geom_sf(data = graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), col = 'darkgrey') +
  geom_sf(data = graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), col = 'darkgrey', size = 0.5) +
  geom_sf(data = path_graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), lwd = 1, col = 'firebrick') +
  geom_sf(data = path_graph %>% activate(nodes) %>% filter(nodeID %in% c(from_node, to_node)) %>% as_tibble() %>% st_as_sf(), size = 2)  +
  geom_sf(data = fromto_df, color = 'red', size = 4)


