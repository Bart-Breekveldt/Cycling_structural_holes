ce = read.csv("D:/EconNet/Paris/R_cleaned/edgesC_clean.csv")
ce = ce %>% select(-X)

cn = read.csv("D:/EconNet/Paris/R_cleaned/nodesC_clean.csv")
cn = cn %>% select(-X)
library(igraph)

#computing and exporting centralities of nodes in the whole network of Paris
g = graph_from_data_frame(d=ce[,c('u','v')], directed = TRUE)
E(g)$weight = ce$perc_length
cc = closeness(g)
bc = betweenness(g)
ec = evcent(g)$vector
data = data.frame(
  bc,
  cc,
  ec
)
write.csv(data,'D:/EconNet/Paris/centralities.csv')

par(mfrow=c(2,2))
hist(cn$street_cou, main = 'degree centrality', xlab = 'degree')
hist(data$cc, ylim = range(0,1000), main = 'closeness centrality', xlab = 'closeness')
hist(bc_scientific, ylim = range(0,2000), main = 'betweenness centrality', xlab = 'betweenness')
hist(data$ec, ylim = range(0,100), main = 'eigenvector centrality', xlab = 'eigenvector')

#computing the minimum spanning tree of the dedicated bike network
gb = graph_from_data_frame(d=ceb[,c('u','v')], directed = TRUE)
E(gb)$weight = ceb$perc_length
minsp = minimum.spanning.tree(gb)
minspel = as_edgelist(minsp)
colnames(minspel) = c('u','v')
write.csv(minspel,"D:/EconNet/Paris/minspann.csv")
