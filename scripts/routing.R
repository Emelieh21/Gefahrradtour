
#https://rviews.rstudio.com/2019/03/06/intro-to-graph-analysis/



# ----------------------------------------------------------------------------------
# convert start & stop coordinate into sf-df for plotting
# ----------------------------------------------------------------------------------

fromto_xy_tosf <- function(from_node, to_node) {
  
  df <- data.frame("nodeID" = c(99999999, 99999998), "lon" = c(from_node[1],to_node[1]), "lat" = c(from_node[2],to_node[2]))
  df_sf <- df
  coordinates(df_sf) <- c("lon", "lat")
  df_sf <- st_as_sf(df_sf, coords = c("lon", "lat"))
  
  fromto_df <- df_sf %>%  st_set_crs(st_crs(nodes))
  
  return(fromto_df)
}


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

prep_accident_data <- function(nodes) {
  accidents_org <- read.csv2("data/AfSBBB_BE_LOR_Strasse_Strassenverkehrsunfaelle_2019_Datensatz.csv", stringsAsFactors = FALSE)
  
  
  accidents <- accidents_org %>% 
    dplyr::rename(lon = XGCSWGS84, lat = YGCSWGS84) %>%
    dplyr::rename_all(.funs = tolower) %>%
    mutate(lon2 = lon, lat2 = lat,
           ukategorie = case_when(
             ukategorie == 1 ~ 30,
             ukategorie == 2 ~ 20,
             ukategorie == 3 ~ 10
           ),
           ukategorie = ukategorie/10) #%>%
    #filter(lon < 13.25 & lat < 52.57)
    #filter(lat < max(nodes$lat) & lat > min(nodes$lat) & lon < max(nodes$lon) & lon > min(nodes$lon))
    # TODO: make the filtering work: if accidents are outside the street shapes, outer edges have too many accidents
  
  coordinates(accidents) <- c("lon", "lat")
  accidents <- st_as_sf(accidents, coords = c("lon", "lat")) %>%  st_set_crs(st_crs(nodes))
  
  return(accidents)
}

# ----------------------------------------------------------------------------------
# create edges and nodes tables
# ----------------------------------------------------------------------------------
create_edges_nodes <- function(road_data, influence_factor_accidents, add_meters) {
  
  # create nodes & edges data frame, fix ids
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
  
  print("got source and target nodes")
  
  nodes <- nodes %>%
    distinct(nodeID, .keep_all = TRUE) %>%
    select(-c(edgeID, start_end)) %>%
    st_as_sf(coords = c('X', 'Y')) %>%
    st_set_crs(st_crs(edges))
  
  print(paste0("Nodes are done, we have ", nrow(nodes), " nodes."))
  
  # keep lat & lon also as numeric columns
  nodes_coords <- st_coordinates(nodes)
  nodes$lon <- nodes_coords[,1]
  nodes$lat <- nodes_coords[,2]
  
  # add accident data
  accidents <- prep_accident_data(nodes)
  
  # a nearest edgeID for each accident and add count variables
  accidents$nearest_edge <- st_nearest_feature(accidents, edges)
  
  st_geometry(accidents) <- NULL
  
  # calculate count & weighted count of accidents per edge
  # accidents_count: number of accidents per edge
  # accidents_count_weighted: number of accidents multiplied with its severity
    ## --> ukategorie: 1 = light injury, 2 = heavy injury, 3 = death victims
  accidents <- accidents %>%
    group_by(nearest_edge, ukategorie) %>%
    summarise(accidents_count_bycat = n()) %>% 
    mutate(accidents_count_weighted = accidents_count_bycat*ukategorie) %>% 
    ungroup() %>% 
    group_by(nearest_edge) %>%
    summarise(accidents_count = sum(accidents_count_bycat),
              accidents_count_weighted = sum(accidents_count_weighted)) %>% 
    data.frame() 
  
  # join accidents and edges to get accident count per edge
  zero_meters <- set_units(0, "meters")
  
  edges_accident <- edges %>%
    left_join(accidents, by = c("edgeID" = "nearest_edge")) %>%
    mutate(
      accidents_count = replace(accidents_count, is.na(accidents_count), 0),
      accidents_count_weighted = replace(accidents_count_weighted, is.na(accidents_count_weighted), 0),
           accident_happened = case_when(
             accidents_count == 0 ~ "yes",
             TRUE ~ "no"
           ),
      length_edge = st_length(geometry),
      length_weighted_exp = exp(influence_factor_accidents) * length_edge,
      length_weighted_m =  (accidents_count_weighted * add_meters) * length_edge,
      #length_weighted_m = replace(length_weighted_m, length_weighted_m == zero_meters, length_edge)
      length_weighted_m = case_when(
        length_weighted_m == zero_meters ~ length_edge,
        TRUE ~ length_weighted_m
      )
    )

  
  
  return(list(edges_accident, nodes))
  
}



# ----------------------------------------------------------------------------------
# create graph
# ----------------------------------------------------------------------------------

create_graph <- function(edges, nodes) {
  
  graph = tbl_graph(nodes = nodes, edges = as_tibble(edges), directed = FALSE)
  
  
  return(graph)
  
}
  # ----------------------------------------------------------------------------------
  # calculate shortest bath between two nodes
  # ----------------------------------------------------------------------------------
  
get_shortest_path <- function(from_node, to_node, graph, nodes, edges, weight_name) {
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
    
    # weight_name let's you choose the varialbe on which the selection is based
    graph <- graph %>%
      activate(edges) %>% 
      mutate(length = !!as.name(weight_name))
    
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
      summarise(length = sum(length_edge))
    
    # path$vpath look at nodes in path
    print(paste0("Path length is ", path_length, " meters."))
    
    # create graph from shortest path
    shortest_path_graph <- graph %>%
      subgraph.edges(eids = path$epath %>% unlist()) %>%
      as_tbl_graph()
    
    return(shortest_path_graph)
  }
  