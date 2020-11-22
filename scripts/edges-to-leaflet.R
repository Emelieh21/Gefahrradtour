## this script uses the functions from 'routing.R' to calculate the shortest path between two input nodes
setwd(gsub("/scripts","",dirname(rstudioapi::getActiveDocumentContext()$path)))
source("scripts/routing.R")


from_nodexy <- c(9.154614,47.683042)
to_nodexy <- c(9.159216,47.679538)

xmin <- min(from_nodexy[1], to_nodexy[1])
xmax <- max(from_nodexy[1], to_nodexy[1])
ymin <- min(from_nodexy[2], to_nodexy[2])
ymax <- max(from_nodexy[2], to_nodexy[2])

placename = c(xmin, ymin, xmax, ymax)

# get road data as graph --------------------------------------------------------------------
# road_data <- get_osm_road_data("Fuerstenberg, Konstanz") 
road_data <- get_osm_road_data(place_name = placename) 

edges_nodes_list <- create_edges_nodes(road_data)
edges <- edges_nodes_list[[1]]
nodes <- edges_nodes_list[[2]]

graph <- create_graph(edges, nodes)

# input from & to coordinates ----------------------------------------------------------------
# example coordinates in Konstanz, can be any lon/lat if you have corresponding street Network
#from_nodexy <- c(9.172404,47.688852)
# from_nodexy <- c(9.157345,47.686012)
#from_nodexy <- c(9.164507,47.688087)

# calculate shortest path between input nodes -----------------------------------------------
## outputs tidygraph of all edges as list of nodeID pairs
shortest_path <- get_shortest_path(from_nodexy,to_nodexy, graph, nodes, edges)

shortest_path <- shortest_path %>% activate(edges) %>% as.data.frame()
names(shortest_path$geometry) = NULL

shortest_path$geometry %>% 
  leaflet() %>% 
  addTiles() %>%
  addPolylines()

                  
                            
