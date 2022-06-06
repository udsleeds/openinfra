# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import pyrosm
import time
#import osmnx 
#import geemap
#import folium 

print('Hello World!')


# View available places  for analysis
available_places = pyrosm.data.available
available_places.keys()
'Leeds' in available_places['cities']

# Gets data from pyrosm providers (BBBike or Geofabrik) and stores in /temp directory - file can be saved to a user specified
# location with additional arguments i.e. get_data(place_name, directory='Desired location to save file')
place_name = 'Leeds'
file_path = pyrosm.get_data(place_name)
print('Data downloaded to:', file_path)

# Initialises the OSM object that parses .osm.pbf files
osm = pyrosm.OSM(file_path)
print('osm type:', type(osm))

# Obtaining the total network for 'Leeds'
leeds_total_network = osm.get_network(network_type = 'all')
print('Variable shape:',leeds_total_network.shape, 'and type:', type(leeds_total_network))


# View all columns of data returned
keys = leeds_total_network.columns
print(keys)

# Plotting the total network for Leeds
leeds_total_network.plot()
time.sleep(60)

# Plot the dirving network of Leeds
leeds_driving_network = osm.get_network(network_type='driving')
leeds_driving_network.plot()

# Here we request and plot ways that a friendly to pedestrians
leeds_walking_network = osm.get_network(network_type='walking')
leeds_walking_network.plot()

# Here we request and plot ways that a friendly to cyclists
leeds_cycling_network = osm.get_network(network_type='cycling')
leeds_cycling_network.plot()

# View the number of rows of each network returned
print('all:', leeds_total_network.shape[0],
     '\ndriving:', leeds_driving_network.shape[0],
     '\nwalking:', leeds_walking_network.shape[0],
     '\ncycling:', leeds_cycling_network.shape[0])

# Here we request and plot the dirivng and Public Service Vehicle network for Leeds
test_driving_and_psv = osm.get_network(network_type='driving_psv')
test_driving_and_psv.plot()

# Here we request and plot the driving + service network for Leeds
test_driving_and_Service = osm.get_network(network_type='driving+service')
test_driving_and_Service.plot()

# Using the custom filter - here we will demonstrait the process of defining your own custom network. 
# To do this we will go through the process of defining a walking network before comparing with network_type='walking'

'FROM THE PYROSM DOCUMENTATION WE CAN SEE HOW THE DEFAULT WALKING NETWORK IS FILTERED'

# =============================================================================
# def walking_filter():
#     """
#     Walking filters for different tags as in OSMnx for 'walk'.
#     Filter out cycle ways, motor ways, private ways, and anything
#     specifying foot=no. allow service roads, permitting things like parking
#     lot lanes, alleys, etc that you *can* walk on even if they're not exactly
#     pleasant walks. some cycleways may allow pedestrians, but this filter ignores
#     such cycleways.
#     Applied filters:
#     '["area"!~"yes"]["highway"!~"cycleway|motor|proposed|construction|abandoned|
#       platform|raceway|motorway|motorway_link"]'
#     '["foot"!~"no"]["service"!~"private"]'
#     """
#     return dict(
#         area=["yes"],
#         highway=[
#             "cycleway",
#             "motor",
#             "proposed",
#             "construction",
#             "abandoned",
#             "platform",
#             "raceway",
#             "motorway",
#             "motorway_link",
#         ],
#         foot=["no"],
#         service=["private"],
#     )
# 
# =============================================================================



