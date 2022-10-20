install.packages('splus2R')
library(foreign)
library(dplyr)
library(splus2R)

e = read.dbf("D:\\EconNet\\Paris\\Cycling_routes\\BikeRoutes.dbf")
n = read.dbf("D:\\EconNet\\Paris\\Cycling_routes\\BikePoints.dbf")

# Take only the relavant columns for the edges (e) and the nodes (n)
ce = e[,c(1,2,4:10,21)]
cn = n[,c(1:5,7:9)]

# Remove the bribleways and busways which are usually not bikable 
ce = ce[1==1 & ce$highway != c('busway','busstop','bribleway'),]

# Fill missing values. Assumption that most minor streets contain missing information, thus 30 km/h, 1 lane, 0 (False) oneway
ce$lanes[is.na(ce$lanes)] <- 1
ce$oneway[is.na(ce$oneway)] <- 0
ce$maxspeed[is.na(ce$maxspeed)] <- 30
ce$cleanspeed[is.na(ce$cleanspeed)] <- 30

# Rename the column for formula usability
ce = 
  ce %>% 
    rename_at('cleanspeed',~'clsp')

# Group all sorts of roads into main categories
ce$hwys = ifelse(ce$highway %in% c('pedestrian','track'),'pedestrian',
                            ifelse(ce$highway %in% c('secondary','secondary_link'),'secondary',
                                   ifelse(ce$highway %in% c('path','cycleway'),'cycleway',
                                          ifelse(ce$highway %in% c('primary','trunk','trunk_link','turning_circle'),'primary','residential'))))

#'tertiary','tertiary_link','service','tertiary','living street','service','crossings','unclassified','services' are 'residential

# Take the assumptions about cycling suitability. One way gets a slight disadvantage for cyclists that may move in both directions.
ce$Fow = ifelse(ce$oneway == 1, 0.025, 0) 

# Amount of lanes is more car volume and wider crossings
ce$Flane = ifelse(ce$lanes==0,0,(ce$lanes-1)*0.05)

# Higher speeds means more speed difference with bikes (around 15 km/h)
ce$Fsp = case_when(ce$clsp == 90 ~ 0.60,
                            ce$clsp >=80 ~ 0.45,
                            ce$clsp >=70 ~ 0.32, 
                            ce$clsp >=60 ~ 0.22, 
                            ce$clsp >=50 ~ 0.15, 
                            ce$clsp >=40 ~ 0.09,
                            ce$clsp >=30 ~ 0.04,
                            TRUE ~ 0)

# Dedicated cycleways get preference, after which the road type suitability is determined for cycling.
ce$Ftype = case_when(ce$hwys == 'cycleway' ~ 0,
                           ce$hwys == 'pedestrian' ~ 0.10,
                           ce$hwys == 'secondary' ~ 0.10,
                           ce$hwys == 'tertiary' ~ 0.15,
                           TRUE ~ 0.05)

# The percieved distance of the road according to the factors mentoined
ce$perc_length = ce$length * (1+ce$Fow) * (1+ce$Flane)* (1+ce$Fsp)* (1+ce$Ftype)

# Split the cycleways and other bikable roads.                                                   
clean_bike = ce[ce$highway == 'cycleway',]
clean_road = ce[ce$highway != 'cycleway',]

write.csv(ce, 'D:/EconNet/Paris/R_cleaned/edges_clean.csv')
write.csv(cn, 'D:/EconNet/Paris/R_cleaned/nodes_clean.csv')
write.csv(clean_bike, 'D:/EconNet/Paris/R_cleaned/bike_clean.csv')
write.csv(clean_road, 'D:/EconNet/Paris/R_cleaned/road_clean.csv')


