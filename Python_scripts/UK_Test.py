#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri May 13 11:44:37 2022

@author: james
"""
def get_OSM_data(place_name, network_type):
    import pyrosm

    fp = pyrosm.get_data(place_name)
    print(place_name, 'data downloaded to:', fp)

    osm = pyrosm.OSM(fp)

    network = osm.get_network(network_type = network_type)

    print(place_name, 'network obtained')    
    return network



import pyrosm 
import pandas as pd 
import geopandas as gpd

# All counties within England
counties = pyrosm.data.available['subregions']['great_britain']

# We want to remove the following countries from the counties list, as they are not counties.
countries = ['england', 'wales', 'scotland']

print(len(counties))
for country in countries:
    try:
        counties.remove(country)
    except:
        print(country,'has already been removed')
print(len(counties))


networks = []

for county in counties: 
    networks.append(get_OSM_data(county, 'driving'))
    print('Obtained network:', county)

print('Combining Networks')
England = gpd.GeoDataFrame( pd.concat(networks, ignore_index=True), crs=networks[0].crs)

print('Plotting England')
England.plot()