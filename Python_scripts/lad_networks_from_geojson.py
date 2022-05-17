#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri May 13 14:54:22 2022

@author: james
"""
#______________________________________________________________________________
def get_networks(place_polygons, tags):

    # This function takes a list of tuples of the form (place_name, place_polygon). 
    # Next the function requests networks for each place_name using the osmnx function
    # geometries_from_polygon.
    # Finally the function stores requested networks in a dicctionary (networks) with 
    # key:value pairs being 'lad':lad_networks before returning this dictionary. 
    
    import osmnx as ox
    networks = {}
    
    counter = 0 
    skipped = [] 
    for place, polygon in place_polygons:
        print('viewing place', place)
        
        # Checks that polygon is a valid geometry for geometries_from_place function
        if polygon.geom_type not in ['MultiPolygon', 'Polygon']:
            print(place, 'is not an accepted geometry type (MultiPolygon or Polygon). ERR_Type:', polygon.geom_type)
            skipped.append(place)
            continue # Skip to next placename that is a valid geometry
                   
        
        # Gets current LAD network from OSM servers
        ox_network = ox.geometries.geometries_from_polygon(polygon, tags=tags)
        # Adds column 'geom_type' that describes the shapely geometry type
        ox_network['geom_type'] = ox_network['geometry'].geom_type
        # Removes any OSM feature that is a node (point)
        ox_network = ox_network[ox_network.geom_type != 'Point']

        networks[place] = ox_network
        counter += 1 
        print('#:', counter, 'finsied lad:', place)

    print('\n\nFinished obtaining networks for', len(networks), 'LADs.'+'\n Having skipped',len(skipped),'LADs for invalid polygon geometry types\n', skipped)
    return networks

#______________________________________________________________________________
import warnings
warnings.filterwarnings("ignore")


import geopandas as gpd
import osmnx as ox

regions_fp = '/home/james/Desktop/joined_lads_fixed.geojson'
regions_lad_url = 'https://github.com/udsleeds/openinfra/raw/main/data-small/joined_lads_fixed.geojson'
transport_regions_url = 'https://github.com/udsleeds/openinfra/raw/main/data-small/regions_new.geojson'

regions_lad = gpd.read_file(regions_lad_url)
#regions_lad = gpd.read_file(regions_fp)
regions_transport = gpd.read_file(transport_regions_url)


geometry = regions_lad.geometry.loc[regions_lad.LAD21NM == 'Leicester'].iloc[0]
name = regions_lad.LAD21NM.iloc[15]

# Testing
leicester = ox.geometries.geometries_from_polygon(geometry, tags ={'highway':True, 'building':False})
leicester['geom_type'] = leicester['geometry'].geom_type
print('Network of Leicester')
leicester[leicester.geom_type == 'LineString'].plot()


# Gets LAD names and polygons
place_polygons = list(zip(regions_lad.LAD21NM, regions_lad.geometry))
# Tags for OSMnx geometires_from_polygon function
tags = {'highway':True, 'buildings':False}

# Returns the OSM network for each LAD 
lad_networks = get_networks(place_polygons, tags)

