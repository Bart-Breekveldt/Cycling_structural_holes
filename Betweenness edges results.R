# Loading the cleaned edges file
ce = read.csv("D:/EconNet/Paris/R_cleaned/edgesC_clean.csv")
ce = ce %>% select(-X)

# Load the files of betweenness count
path = "D:/EconNet/Paris/Betweenness Count"
files = list.files(path=path, pattern=NULL, all.files=FALSE,
           full.names=FALSE)

# bind all the dataframes together
df = data.frame()
for (i in files) {
  df = rbind(df, read.csv(paste0(path,'/',i)))
}

# summarise by osmid-key
cnt_osm = df %>%
  group_by(osmid_from, osmid_to) %>%
  summarise(n = sum(n))
cnt_osm = cnt_osm[order(cnt_osm$n, decreasing = TRUE), ]

write.csv(cnt_osm,"D:/EconNet/Paris/Betweenness Count/total_kernel_edges.csv")

# Determine min and max osm per edge, for determining betweenness count from both directions
from = unlist(as.list(cnt_osm[,'osmid_from']))
to = unlist(as.list(cnt_osm[,'osmid_to']))
min = c()
max = c()
onedir = c()
bidir = c()
for (i in 1:nrow(cnt_osm)) {
  min = append(min, min(from[i], to[i]))
  max = append(max, max(from[i], to[i]))
  onedir = append(onedir, paste0(from[i],'-',to[i]))
  bidir = append(bidir, paste0(min[i],'-',max[i]))
  if (i %% 100 ==0) {print(i)}
}
cnt_osm$min_osm = min
cnt_osm$max_osm = max
cnt_osm$key_onedir = onedir
cnt_osm$key_bidir = bidir

# Sum by the key of min-max osmid-edges
cnt2 = cnt_osm %>%
  group_by(key_bidir) %>%
  summarise(n = sum(n))
cnt2 = cnt2[order(cnt2$n, decreasing = TRUE), ]

# Add the counts to the edge-table
for (i in 1:nrow(ce)) {
  ce[i,'min_osm'] = min(ce[i,'u'], ce[i,'v'])
  ce[i,'max_osm'] = max(ce[i,'u'], ce[i,'v'])
  ce[i,'key_bidir'] = paste0(ce[i,'min_osm'],'-',ce[i,'max_osm'])
  ce[i,'key_onedir'] = paste0(ce[i,'u'],'-',ce[i,'v'])
  if (i %% 100 ==0) {print(i)}
}

# Merge counts per bi-directional edge
ce1_merge = merge(ce, cnt2, all.x = TRUE, by = 'key_bidir')
ce1_merge = ce1_merge[,c('u','v','min_osm','max_osm','n','osmid','name','hwys','Ftype','maxspeed','Fsp','oneway','Fow','length','perc_length','lanes','Flane')]
ce1_merge = ce1_merge[order(ce1_merge$n, decreasing = TRUE), ]

# Merge per one-directional edge
ce2_merge = merge(ce, cnt_osm, all.x = TRUE, by = 'key_onedir')
ce2_merge = ce2_merge[,c('u','v','min_osm.x','max_osm.x','n','osmid','name','hwys','Ftype','maxspeed','Fsp','oneway','Fow','length','perc_length','lanes','Flane')]
ce2_merge = ce2_merge[order(ce2_merge$n, decreasing = TRUE), ]

colnames(ce2_merge)

write.csv(ce2_merge, 'D:/EconNet/Paris/Betweenness Count/kernel_edges_onedirectional.csv')
write.csv(ce1_merge, 'D:/EconNet/Paris/Betweenness Count/kernel_edges_bidrectional.csv')


