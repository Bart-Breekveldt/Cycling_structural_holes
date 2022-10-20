# Cycling_structural_holes

Extracting the cycling network (1)

The script for extracting the network is done in Python, instead of in R. This is due to the package ‘OSMnx’ which is able to extract OpenStreetMap (OSM) data directly and includes the cycling network and amenities that are used in this research. The goal of this script is to extract the bikeable network for the Paris urban area. The steps are as follows. First the Paris urban area is taken “citypopulation.de”, for which the source is INSEE, the national French statistical bureau for determining urban area or in French: ‘unités_urbaine’. This table is extracted by webscraping and then formatted to only include communes and municipal arrondissements, other types are aggregations or more detailed statistical areas.  

Cycling networks (network_type = ‘bike’) are then extracted for all the municipalities within the Paris urban agglomeration within a list or single city. The list contains all Paris urban communities. Retain_all retains all nodes and edges when they are partly within the specified limits. Clean_periphery creates a buffer of 500 meters around the specified area for wayfinding. The graph is split into nodes and edges. Edges can contain multiple values in a list in a single record, for matrix computation this is problematic. If ‘cycleway’ is in a list this is taken, otherwise the first element, which is assumed to be the most important. For the same matrix computation goal, object data types are replaced by floats for a few columns. Walking speed is assumed to be 5 km/h, speed at signals 50 km/h, roads over 90 km/h too dangerous to cycle on. X and Y coordinates in meters are determined (for later use in R) and the nodes and edges are stored.  


Extracting amenities and shops (3)

This script is also written in Python due to its connection to the package OSMnx. This package is also used for extracting amenities and shops according to the time geography described in “Extracting the cycling network”. The Paris urban area is extracted at the same way as in this script. 
