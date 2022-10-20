library(igraph)
library(foreign)
library(dplyr)
library(intergraph)

# import edges, nodes, daily POI
ce = read.csv("D:/EconNet/Paris/R_cleaned/edges_clean.csv")
cn = read.csv("D:/EconNet/Paris/R_cleaned/nodes_clean.csv")
am = read.dbf('D:/EconNet/Paris/POI/amenities_shops.dbf')

cn = cn[order(cn$osmid_var), ]
cn = rename(cn, 'igraph_no' = 'X')
cn['igraph_no'] = seq(1,nrow(cn),1)

a = am [am['amenity'] == 'doctors',]

# Find closest road node
for (i in 1:nrow(a)) {
  a[i,'cl_dist_m'] = min(abs(a[i,5] - cn[,7]) + abs(a[i,6] - cn[,8]))
  a[i,'cl_igno'] = cn[which.min(abs(a[i,5] - cn[,7]) + abs(a[i,6] - cn[,8])),'igraph_no']
  if (i %% 500 == 0) {print(i)}
}

# Merge node information, select columns and show its properties
nam = merge(a, cn, by.x = 'cl_igno', by.y = 'igraph_no')
nam_adj = nam[,c('cl_igno','osmid_var','amenity','name','cl_dist_m','geom_x_m.y','geom_y_m.y','street_cou')]
str(nam_adj)


coords_x = c(nam_adj[,5])
coords_y = c(nam_adj[,5])
osmid = c(nam_adj[,'cl_igno'])
l1 = list()
for (i in 1:nrow(nam_adj)) {
  osmids = nam_adj[(abs(coords_x[i] - coords_x) + abs(coords_y[i] - coords_y)) <=4000,'cl_igno']
  osmids = osmids[osmids != osmid[i]]
  l1 = append(l1, list(c(osmids)))
  if (i %% 100 == 0) {print(i)}
}

# Create the graph
graph = graph_from_data_frame(d = ce[,c('u','v')], vertices = cn$osmid_var, directed = TRUE) 
E(graph)$weight = ce$perc_length

E(graph)

c1 = c()
c2 = c()
for (i in 1:nrow(nam_adj)) {
  path = shortest_paths(graph, from=i, to = unlist(l1[i]))$vpath
  for (j in 1:length(path)) {
    u = unname(unlist(path[j]))
    c1 = append(c1, u[-length(u)])
    c2 = append(c2, u[-1])
  }
  print(i)
}
df = data.frame(from = c1, to = c2)

cnt = df %>%
  group_by(startn, endn) %>%
  count()
cnt = cnt[order(cnt$n, decreasing = TRUE), ]
cnt






