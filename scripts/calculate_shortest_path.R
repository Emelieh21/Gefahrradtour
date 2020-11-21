
# ----------------------------------------------------------------------------------
# Calculate shortest path between two nodes
# ----------------------------------------------------------------------------------
# Note: This code is based on this tutorial: https://www.r-spatial.org/r/2019/09/26/spatial-networks.html

## this script uses the functions from 'routing.R' to calculate the shortest path between two input nodes
source("scripts/routing.R")

# get road data as graph --------------------------------------------------------------------
road_data <- get_osm_road_data("Fuerstenberg, Konstanz") 

edges_nodes_list <- create_edges_nodes(road_data)
edges <- edges_nodes_list[[1]]
nodes <- edges_nodes_list[[2]]

graph <- create_graph(edges, nodes)


# input from & to coordinates ----------------------------------------------------------------
# example coordinates in Konstanz, can be any lon/lat if you have corresponding street Network
#from_nodexy <- c(9.172404,47.688852)
from_nodexy <- c(9.157345,47.686012)
#from_nodexy <- c(9.164507,47.688087)
#from_nodexy <- c(9.154614,47.683042)
to_nodexy <- c(9.159216,47.679538)

# calculate shortest path between input nodes -----------------------------------------------
## outputs tidygraph of all edges as list of nodeID pairs
shortest_path <- get_shortest_path(from_nodexy,to_nodexy, graph, nodes, edges)


# ----------------------------------------------------------------------------------
# plots
# ----------------------------------------------------------------------------------
# plot road data from osm
ggplot(data = kn_lines) + geom_sf()

# plot road data and from and to node
ggplot() +
  geom_sf(data = graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), col = 'darkgrey') +
  #geom_sf(data = graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), col = 'blue') +
  geom_sf(data = fromto_df, color = 'red', size = 4)

# plot road data, from and to node, and shortest path
ggplot() +
  geom_sf(data = graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), col = 'darkgrey') +
  geom_sf(data = graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), col = 'darkgrey', size = 0.5) +
  geom_sf(data = path_graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), lwd = 1, col = 'firebrick') +
  geom_sf(data = path_graph %>% activate(nodes) %>% filter(nodeID %in% c(from_node, to_node)) %>% as_tibble() %>% st_as_sf(), size = 2)  +
  geom_sf(data = fromto_df, color = 'red', size = 4)



# ----------------------------------------------------------------------------------
# maybe interesting for later
# ----------------------------------------------------------------------------------
## possible preprocessing step if it gets too slow later
# matrix with all shortest paths from all vertices to all vertices
distances <- distances(
  graph = graph,
  weights = graph %>% activate(edges) %>% pull(length) # set weight of graph as edge length
)

