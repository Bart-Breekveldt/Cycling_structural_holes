library(igraph)
library(foreign)
library(dplyr)
library(intergraph)
library(tidyr)

# import edges, nodes, daily POI
ce = read.csv("D:/EconNet/Paris/R_cleaned/edgesC_clean.csv")
cn = read.csv("D:/EconNet/Paris/R_cleaned/nodesC_clean.csv")
am = read.dbf('D:/EconNet/Paris/POI_City/amenities_shops.dbf')

# X is the row number of the write_file df of the last R script, imported with csv as column
cn = cn %>% select(-X)
ce = ce %>% select(-X)

# Order column for referencing in igraph (igraph ranks numbers smallest to largest, sets this as nodes)
cn = cn[order(cn$osmid_var), ]
cn['igraph_no'] = seq(1,nrow(cn),1)

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
kernel_pathfinding <- function(amenity_from, amenity_to, cn, ce) {
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
    ed = ce[ed1 <= 4000 | ed2 <= 4000,]
    
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

start = Sys.time()
for (as in ord[2:length(ord)]) {
  start_a = Sys.time()
  a = am [am['amenity'] == as,]
  ama = filter(am,!amenity %in% c(as))
  len= seq(1,ceiling(nrow(a)/10),1)
  df = data.frame()
  for (j in len) {
    start_c = Sys.time()
    print(paste(as,j,'/',max(len)))
    xnam_adj = closest_node(a[(1+((j-1)*10)):(ifelse(j*10 > nrow(a),nrow(a),j*10)),])
    xnama_adj = closest_node(ama)
    out = kernel_pathfinding(xnam_adj, xnama_adj, cn, ce)
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
  
  write.csv(cnt_osm,paste0('D:/EconNet/Paris/Betweenness Count/',as.character(as),'_kernel_edges.csv'))
  print(paste('time chunk',print(Sys.time() - start_a),'overall',Sys.time() - start))
}
print(Sys.time() - start)


# FORMATTING AFTER BETWEENNESS EDGES LOOP ABOVE
# Determine min and max osm per edge, for determining betweenness count from both directions
for (i in 1:nrow(cnt_osm)) {
  cnt_osm[i,'min_osm'] = min(cnt_osm[i,'osmid_from'], cnt_osm[i,'osmid_to'])
  cnt_osm[i,'max_osm'] = max(cnt_osm[i,'osmid_from'], cnt_osm[i,'osmid_to'])
  cnt_osm[i,'key'] = paste0(cnt_osm[i,'min_osm'],'-',cnt_osm[i,'max_osm'])
}

# Sum by the key of min-max osmid-edges
cnt2 = cnt_osm %>%
  group_by(key) %>%
  summarise(n = sum(n))
cnt2 = cnt2[order(cnt$n, decreasing = TRUE), ]

# Add the counts to the edge-table
for (i in 1:nrow(ce)) {
  ce[i,'min_osm'] = min(ce[i,'u'], ce[i,'v'])
  ce[i,'max_osm'] = max(ce[i,'u'], ce[i,'v'])
  ce[i,'key'] = paste0(ce[i,'u'],'-',ce[i,'v'])
}

# Merge counts per edge
ce_merge = merge(ce, cnt2, all.x = TRUE, by = 'key')
ce_merge = ce_merge[,c('u','v','min_osm','max_osm','n','osmid','name','hwys','Ftype','maxspeed','Fsp','oneway','Fow','length','perc_length','lanes','Flane')]
ce_merge = ce_merge[order(ce_merge$n, decreasing = TRUE), ]

write.csv(ce_merge, 'D:/EconNet/Paris/Betweenness Count/kernel_edges.csv')
write.csv(cn, 'D:/EconNet/Paris/Betweenness Count/kernel_nodes.csv')
