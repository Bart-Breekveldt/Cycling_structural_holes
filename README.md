# Cycling_structural_holes

The main goal of these scripts is to find structural holes in the cycling network of Paris. This is done by finding shortest paths between popular amenities, shops and transit facilities. The percieved length increases with penalties from the amount of lanes, max speed, one-way streets and street type, this prefers paths over more suitable streets. The occurrences of edges on shortest paths (betweenness) are counted and multiplied with an importance factor, dependent on the penalties, for improving the street for cyclists. Performance is enhanced by using moving kernels, only using edges that fall partly or completely within a threshold Euclidean distance.

##Extracting amenities/shops and the bikeable network of Paris (Python, 1)

For the city of Paris the bikeable network is extracted from the OSMnx package in Python from OpenStreetMap. This overlays the requested geographical area and returns the bikable network, both cycleways and cycleable streets. Together with Paris, Saint-Mande is also included to fill a gap between Bois a Vincennes and Urban Paris. Retain all retains all edges/nodes that are partly within the requested area. Clean_periphery creates a 500m buffer and then truncates to a smooth network. If an edge has two street types, the cycleway is preferred, otherwise the first element is taken as is with all the other columns which have a list inside a record.

Streets with a speed equal to or over 90 km/h are removed due to the danger of speed difference between cyclist and motor traffic. Bus lanes and bribleways are removed as unusable for cyclists (2nd script R). For simplicity, both cycleways (2nd script R) and pedestrian streets are transformed to 10 km/h. Streets that are indicated by speed 'signals' get the speed of 50 km/h. To enable distance measuring in meters, x and y coordinates from nodes and edges (start and end) are determined in meters with crs/epsg 3043. 

Amenities are also extracted from OpenStreetMap, except for the bikestations and transit entry points for RATP (underground) and SNCF (train) stations. These last are from Apur/Ile-de-France open data platform (STATION VELIB,  ACCES METRO IDFM). For comparability and simplification, the type of amenity, its osmid/other id, its name, the geometry and resulting x and y coordinates in meters are taken. For all geometries the centroid is taken to eliminate Polygon-building shapes. Amenities included are restaurant, cafe, college, university, pharmacy, dentist, doctors, cinema and theatre. From shops supermarkets, butchers, greengrocers and bakeries are taken.

##Data cleaning and adding perceived street length (2, R)
Columns that are non-relevant are removed, and for mentioned reasons also busways and bribleways. Cycleways set to 10 km/h. An assumption is that less important streets are missing more often in the data, therefore NA values are filled with 30 km/h (max speed), 1 (lanes) and 0 (one-way). From visual inspection, streets are classified into five types; pedestrian, cycleways, residential, secondary and primary, with their own penalties. Highways are already removed as being non-bikeable. Penalties for max speed are slightly exponential, due to the mentioned danger in speed difference between cyclists and motor traffic. At last both the street length and these penalties form the perceived street length for cyclists, assuming cyclists want to slightly divert to better suitable streets.

##Determining kernel betweenness edges (3, R)




