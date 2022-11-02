# Cycling_structural_holes

This script is written for the course Economies of Networks and part of the group work by Bart Breekveldt, Veit Schneider, Laura Smet, Haby Daffe and Emma Hoogenboom. It builds on network theory and bikability theories. Details can be found in the report.

A given presenentation of "Where are the structural holes in the biking-network of Paris to improve the city's bikeability?" can be found on Canva https://www.canva.com/design/DAFQzS1fnu0/ZDMXdh4BY9UbnI9gzHxuAg/edit#

The main goal of these scripts is to find structural holes in the cycling network of Paris. This is done by finding shortest paths between popular amenities, shops and transit facilities. The percieved length increases with penalties from the amount of lanes, max speed, one-way streets and street type, this prefers paths over more suitable streets. The occurrences of edges on shortest paths (betweenness) are counted and multiplied with an importance factor, dependent on the penalties, for improving the street for cyclists. Performance is enhanced by using moving kernels, only using edges that fall partly or completely within a threshold Euclidean distance.

## Extracting amenities/shops and the bikeable network of Paris (1, Python)

For the city of Paris the bikeable network is extracted from the OSMnx package in Python from OpenStreetMap. This overlays the requested geographical area and returns the bikable network, both cycleways and cycleable streets. Together with Paris, Saint-Mande is also included to fill a gap between Bois a Vincennes and Urban Paris. Retain all retains all edges/nodes that are partly within the requested area. Clean_periphery creates a 500m buffer and then truncates to a smooth network. If an edge has two street types, the cycleway is preferred, otherwise the first element is taken as is with all the other columns which have a list inside a record.

Streets with a speed equal to or over 90 km/h are removed due to the danger of speed difference between cyclist and motor traffic. Bus lanes and bribleways are removed as unusable for cyclists (2nd script R). For simplicity, both cycleways (2nd script R) and pedestrian streets are transformed to 10 km/h. Streets that are indicated by speed 'signals' get the speed of 50 km/h. To enable distance measuring in meters, x and y coordinates from nodes and edges (start and end) are determined in meters with crs/epsg 3043. 

Amenities, shops and transit entry points (daily POI) are also extracted from OpenStreetMap, except for the bikestations and transit entry points for RATP (underground) and SNCF (train) stations. These last are from Apur/Ile-de-France open data platform (STATION VELIB,  ACCES METRO IDFM). For comparability and simplification, the type of amenity, its osmid/other id, its name, the geometry and resulting x and y coordinates in meters are taken. For all geometries the centroid is taken to eliminate Polygon-building shapes. Amenities included are restaurant, cafe, college, university, pharmacy, dentist, doctors, cinema and theatre. From shops supermarkets, butchers, greengrocers and bakeries are taken. But all amenities, shops and others that can be extracted by OSM tags are possibe.

## Data cleaning and adding perceived street length (2, R)
Columns that are non-relevant are removed, and for mentioned reasons also busways and bribleways. Cycleways set to 10 km/h. An assumption is that less important streets are missing more often in the data, therefore NA values are filled with 30 km/h (max speed), 1 (lanes) and 0 (one-way). From visual inspection, streets are classified into five types; pedestrian, cycleways, residential, secondary and primary, with their own penalties. Highways are already removed as being non-bikeable. Penalties for max speed are slightly exponential, due to the mentioned danger in speed difference between cyclists and motor traffic. At last both the street length and these penalties form the perceived street length for cyclists, assuming cyclists want to slightly divert to better suitable streets.

## Getting a brief grasp of the network (3, R)
For getting an overview of the network both centralities are calculated the minimum spanning tree. For the first a graph was created with the percieved length as weights. After it node-centralities are calculated. These are degree (node-connections), closeness (closness of all nodes to an ego in terms of geodesic distances), betweenness (count of nodes on geodesic paths) and eigenvector centrality (connectiveness of your connections (degree)). With mfrow, the centralities are then put in four quarters of a single graph. The minimum spanning tree (all nodes connected at lowest cost with edges = nodes-1) is calculated for the biking network standalone, to see which lanes are critical in longer distance travel (these distances are also calculated with the percieved length).

## Determining igraph-kernels and counting edge betweenness (3, R)
Amenities restaurant and cafe were too large in size to include in the research, these are removed. The osmid column is ordered increasingly and an igraph-no is created, because igraph ranks nodes small to large and uses this as node-ids internally. Closest road igraph-ids and the distance to the closest road node are determined by the function 'closest node', some node information is also added. The function kernel-pathfinding checks if edges (their start and end points) are partly or fully within the set kernel (in here 4000m) by their distance to the ego-node, the cloest road node of the specified amenity. The perceived lenghts are added as weights. After graph creation, isolated nodes are removed as their connected edges lie completely beyond the kernel threshold. These nodes are subsetted, and a new igraph-id for the specific graph is created and attached to amenities within the kernel distance, because the rank of the original graph is not complete anymore. Pathfinding then takes place and all stored paths and node information are returned.

For every amenity type, paths are found to other amenity types (people don't go to two supermarkets). For every specific amenity closest road nodes are found and igraph-kernels determined. Paths are calculated from every specific amenity to all amenities within range. With the function path_unpacking all paths are unpacked. All paths are unlisted and unnamed for appending them in unnested lists. Then all amenity-specific igraph-ids are referred to igraph-no of the holistic graph for comparability between amenities. Individual edges are then set first into lists then into a dataframe. The amount of occurrences are then determined and node information merge, including osmids. Information from every amenity is stored separately, to prevent information loss if the PC gets overloaded.

## Summarizing results on structural holes and create interactive Leaflet-maps (4, Rmd)
Paths are first loaded with the list.files function. Then all the edge betweenness counts are merged into one dataframe, the counts of the individual edges of all different amenity types then summed. Two keys are made to merge the edge information with these counts; a one-directional key and a bidirectional key. The first summarizes a result for both directions separately, the latter combines them, because when improving a street, both directions are usually done together, thus the combined importance of the street is also important. When the latter is chosen the merge-key exists of the min and max osmid (both 1-2 and 2-1 will result in a key 1-2). At the first the from and to osmids are held as key. With the bidirectional edge occurrences, a second round of summing occurs (amounts of 1-2 and 2-1 get summed here). The merge of efges and edge occurrences is done with a left join, because some edges don't are not in between any path and in the case of bidirectional count both directions (1-2 and 2-1) get the same summed amount. 

The importance factor is equal to the compounded penalties also used in calculated the perceived length. They are intended to have a relative limited effect, for classifying structural holes this is larger. Multiplying this result with the edge-occurences gets the improve importance factor, which we see as structural hole in the network. Other structural holes can be found by plotting the results, like in QGIS. For improvement a primary multi-lane street with higher speeds is multiple times more important to improve than a quiet residential street. Results for edge-occurrences (count col = n), importance factor and the improvement importance for one-directional and bidirectional edges can be found in the attached csv.

Next maps are made with the Leaflet package. Geometries are combined with the transformed edge information. For this values are ordered decreasing, to favor large values in showing of the graph. Color palettes are made upfront (better suited for sf / leaflet). The graphs are colored and weighted by the edge betweenness, important facor or improve important factor (structural holes). A popup is added to generate extra information if the user requests so. The widget is saved in a HTML-format to be reused. A snapshort of the Leaflet map for structural holes in Paris can be found below. The interactive part can be found on RPubs. Due to the interactive character of the maps these are not plotted in GitHub. Use this link https://rpubs.com/Bart_dsguy/Structural_holes_maps

![afbeelding](https://user-images.githubusercontent.com/83957293/199369001-73aa03cc-3b33-44ff-b834-df775f0f0e1a.png)

### Limitations
Limitations include that not all amenities and shops are taken and that residential and office areas are excluded. There is also no distance decay function implemented in the code, connections with amenities close or far within the kernel are rated the same. Structural holes that exist due to divertion at perceived lengths vs raw lengths are not taken into account.





