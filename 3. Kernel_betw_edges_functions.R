library(igraph)
library(foreign)
library(dplyr)
library(intergraph)
library(tidyr)

# import edges, nodes, daily POI
ce = read.csv("D:/EconNet/Paris/R_cleaned/edgesC_clean.csv")
cn = read.csv("D:/EconNet/Paris/R_cleaned/nodesC_clean.csv")
am = read.dbf('D:/EconNet/Paris/POI_City/amenities_shops.dbf')
am = am[!am$amenity %in% c('restaurant','cafe'),]

# X is the row number of the write_file df of the last R script, imported with csv as column
cn = cn %>% select(-X)
ce = ce %>% select(-X)

# Order column for referencing in igraph (igraph ranks numbers smallest to largest, sets this as nodes)
cn = cn[order(cn$osmid_var), ]
cn['igraph_no'] = seq(1,nrow(cn),1)

# Determine unique amenity types and their counts, rank from amenity types with lower to higher count.
unq = unique(am$amenity)[order(unique(am$amenity))]
ord = am %>%
  group_by(amenity) %>%
  count()
ord = data.frame(ord)[order(data.frame(ord)$n),]
ord = ord$amenity
ord

# x and y coordinates for the road nodes
closest_node <- function(amenity, nodes) {
  a = amenity
  for (i in 1:nrow(a)) {
    dist_pyth = sqrt(abs(a[i,5] - cn[,6])**2 + abs(a[i,6] - cn[,7])**2) # Find distance
    a[i,'cl_dist_m'] = min(dist_pyth) # Get the min distance (closest)
    a[i,'cl_igno'] = cn[which.min(dist_pyth),'igraph_no'] # Get the node with min distance
    #if (i %% 2000 == 0) {print(i)}
  }
  # Merge node information, select columns and show its properties, chosen amenity
  nam = merge(a, cn, by.x = 'cl_igno', by.y = 'igraph_no')
  nam_adj = nam[,c('cl_igno','osmid_var','amenity','name','cl_dist_m','geom_x_m.y','geom_y_m.y','street_cou')]
  return(nam_adj)
}
kernel_pathfinding <- function(amenity_from, amenity_to, cn, ce, kernel = 4000) {
  nam_adj = amenity_from
  nama_adj = amenity_to
  
  coords_x = c(nam_adj[,6])
  coords_y = c(nam_adj[,7])
  
  # start (1) and end (2) x/y coordinates of road edges
  xe_coords1 = ce$x1_m
  ye_coords1 = ce$y1_m
  xe_coords2 = ce$x2_m
  ye_coords2 = ce$y2_m
  
  # Lists will contain
  paths = vector(mode='list', length=nrow(nam_adj))
  igraph_no = vector(mode='list', length=nrow(nam_adj))
  start = Sys.time()
  # MAIN determine number of occurring edges in shortest paths with amenity-specific made graphs
  for (i in 1:nrow(nam_adj)) {
    # Check edge distance to amenity closest road node
    ed1 = sqrt((coords_x[i] - xe_coords1)**2 + (coords_y[i] - ye_coords1)**2)
    ed2 = sqrt((coords_x[i] - xe_coords2)**2 + (coords_y[i] - ye_coords2)**2)
    
    # Include edges which are partly of completely in the specified amenity-roadnode radius
    ed = ce[ed1 <= kernel | ed2 <= kernel,]
    
    # Create graph with all nodes and filtered edges and add weights
    graph = graph_from_data_frame(d = ed[,c('u','v')], vertices = cn$osmid, directed = TRUE)
    E(graph)$weight = ed$perc_length
    
    # Delete isolated nodes in the new graph, add node information
    graph = delete.vertices(graph, which(degree(graph) == 0))
    
    cn_adj = cn[cn$osmid %in% V(graph)$name,]
    
    # Create an amenity-specific igraph reference number
    cn_adj['IG_subset'] = seq(1,nrow(cn_adj),1)
    cn_adj = cn_adj[,c('igraph_no','IG_subset')]
    
    # Determine non-similar destinations (no same amenity) which are in the determined radius
    nag = nama_adj[nama_adj$cl_igno %in% cn_adj[,1],]
    # Merge igraph-specific number for destinations
    nag_m = merge(nag, cn_adj, all.x = TRUE, by.x = 'cl_igno', by.y = 'igraph_no')
    
    # Determine the shortest path from the amenity to all its destinations (ego-centric)
    path = shortest_paths(graph, from=i, to = nag_m$IG_subset)$vpath
    paths[[i]] = path
    igraph_no[[i]] = cn_adj
    if(i %% 10 == 0) {print(i)}
  }
  return(list(paths = paths, kernel_ids = igraph_no))
  print(Sys.time() - start)
  
}
path_unpacking <- function(path_list, kernel_nodes) {
  start = Sys.time()
  c1 = c()
  c2 = c()
  for (i in 1:length(path_list)) {
    # By shortest path;
    c3 = kernel_nodes[[i]][,'igraph_no']
    path = path_list[[i]]
    for (j in 1:length(path)) {
      # unlist the shortest
      u = unname(unlist(path[j]))
      ig = c3[u]
      # Add 'moving windows' of two combinations to lists
      c1 = append(c1, ig[-length(ig)])
      c2 = append(c2, ig[-1])
    }
    print(i)
  }
  print(Sys.time() - start)
  
  # Put the individual path edges into a df
  df = data.frame(from = c1, to = c2)
  return(df)
}

# for every amenity type calculate paths within threshold to amenities of another type
start = Sys.time()
for (as in ord) {
  start_a = Sys.time()
  a = am [am['amenity'] == as,]
  ama = filter(am,!amenity %in% c(as))
  
  # Divide in chunks of 10, the function path_unpacking becomes increasingly slow with every iteration, 
  # this isn't the case when cut-off and its starts again
  len= seq(1,ceiling(nrow(a)/10),1)
  df = data.frame()
  for (j in len) {
    start_c = Sys.time()
    print(paste(as,j,'/',max(len)))
    
    # Find the closest road nodes for both the chosen amenity type (xnam) and other amenity types (xnama)
    xnam_adj = closest_node(a[(1+((j-1)*10)):(ifelse(j*10 > nrow(a),nrow(a),j*10)),])
    xnama_adj = closest_node(ama)
    
    # Determine eco-centric igraph kernel, calculate shortest path
    out = kernel_pathfinding(xnam_adj, xnama_adj, cn, ce)
    
    # Unpack the paths to their individual edges.
    counts = path_unpacking(out$paths, out$kernel_ids)
    df = rbind(df, counts)
    print(paste('time chunk',print(Sys.time() - start_c),'overall',Sys.time() - start))
  }
  cnt = df %>%
    group_by(from, to) %>%
    count()
  cnt = cnt[order(cnt$n, decreasing = TRUE), ]
  
  # Merge counts with osmid-info
  cnt_osm = merge(cnt, cn, all.x = TRUE, by.x = 'from', by.y ='igraph_no')[,c('from','to','n','osmid')]
  cnt_osm = rename(cnt_osm, 'osmid_from' = 'osmid')
  cnt_osm = merge(cnt_osm, cn, all.x = TRUE, by.x = 'to', by.y ='igraph_no')[,c('from','to','n','osmid_from','osmid')]
  cnt_osm = rename(cnt_osm, 'osmid_to' = 'osmid')
  
  write.csv(cnt_osm,paste0('D:/EconNet/Paris/Betweenness Count/',as,'_kernel_edges.csv'))
  print(paste('time chunk',print(Sys.time() - start_a),'overall',Sys.time() - start))
}
print(Sys.time() - start)
