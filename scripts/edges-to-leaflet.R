## this script uses the functions from 'routing.R' to calculate the shortest path between two input nodes
setwd(gsub("/scripts","",dirname(rstudioapi::getActiveDocumentContext()$path)))
source("scripts/routing.R")

# from_nodexy <- c(9.154614,47.683042)
# to_nodexy <- c(9.159216,47.679538)
# from_nodexy <- c(13.38527,52.53041)
# to_nodexy <- c(13.39643,52.53814)
# tos_nodexy <- c(13.39555, 52.51785)
from_nodexy <- c(13.43744,52.50723)
to_nodexy <- c(13.43529,52.5085)

xmin <- min(from_nodexy[1], to_nodexy[1])-0.0005
xmax <- max(from_nodexy[1], to_nodexy[1])+0.0005
ymin <- min(from_nodexy[2], to_nodexy[2])-0.0005
ymax <- max(from_nodexy[2], to_nodexy[2])+0.0005

# xmin <- min(from_nodexy[1], to_nodexy[1])-0.005
# xmax <- max(from_nodexy[1], to_nodexy[1])+0.005
# ymin <- min(from_nodexy[2], to_nodexy[2])-0.005
# ymax <- max(from_nodexy[2], to_nodexy[2])+0.005

placename = c(xmin, ymin, xmax, ymax)

# get road data as graph --------------------------------------------------------------------
# road_data <- get_osm_road_data("Fuerstenberg, Konstanz")
road_data <- get_osm_road_data(place_name = placename)

# or can be meters that are added to edge length for each accident
influence_factor_accidents <- 0.9
add_meters <- 0

# create graph from edges & nodes sf-table
edges_nodes_list <- create_edges_nodes(road_data, influence_factor_accidents, add_meters)

# edges_nodes_list <- create_edges_nodes(road_data)
edges <- edges_nodes_list[[1]]
nodes <- edges_nodes_list[[2]]

# nodes <- subset(nodes, nodeID %in% unique(c(edges$from,edges$to)))

graph <- create_graph(edges, nodes)

# set weight name to choose based on which weight shortest path should be calculated
weight_nameex <- "length_weighted_exp" # exp() of accident count per street
# for this we need "influence_factor_accidents" --> value between 1-100 (percent), how much should safety count
weight_namem <- "length_weighted_m" # add additional meters to street length for each accident
# for these we need "add_meters"
weight_name <- "length_edge" # normal length of a street

# caluclate shortest path
## outputs tidygraph of all edges as list of nodeID pairs
shortest_path <- get_shortest_path(from_nodexy,to_nodexy, graph, nodes, edges, weight_name)

shortest_path <- shortest_path %>% activate(edges) %>% as.data.frame()
names(shortest_path$geometry) = NULL

shortest_path$geometry %>% 
  leaflet() %>% 
  addTiles() %>%
  addPolylines()

                  
                            
