#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jun 13 13:16:10 2022

@author: james
"""

import pyrosm

# Specifying desired keys to be kept - this is our first level of filtering
osm = pyrosm.OSM(pyrosm.get_data("Leeds"))

keys_to_keep = ["highway", "route"]
tags_to_keep = ["route" ]

# Specifying key:value pairs to be filtered - this is the second level of filtering. 
drive_filter = dict(
        area=["yes"],
        highway=[
            "cycleway",
            "footway",
            "path",
            "pedestrian",
            "steps",
            "track",
            "corridor",
            "elevator",
            "escalator",
            "proposed",
            "construction",
            "bridleway",
            "abandoned",
            "platform",
            "raceway",
        ],
        motor_vehicle=["no"],
        motorcar=["no"],
        service=["parking", "parking_aisle", "private", "emergency_access"],
    )

# Specifying if the above tags should be kept or removed
filter_type = "exclude"

# Filter the network: 
# From the docuemtnation on get_network() function, nodes and relations are set False as default so we do so here too.
leeds_custom_driving = osm.get_data_by_custom_criteria(custom_filter = drive_filter, 
                                                       osm_keys_to_keep = keys_to_keep,
                                                       filter_type = filter_type,
                                                       tags_as_columns = tags_to_keep,
                                                       keep_nodes = False,
                                                       keep_relations = True)

# Visualisation and stats
print("Leeds_custom_driving plot")
leeds_custom_driving.plot()


leeds_custom_driving['geom_type'] = leeds_custom_driving['geometry'].geom_type
print(leeds_custom_driving.shape)
print(leeds_custom_driving.geom_type.value_counts())


leeds_custom_driving.route.value_counts()

leeds_bus_routes = leeds_custom_driving[leeds_custom_driving.route == 'bus']
print(leeds_bus_routes.shape)
leeds_bus_routes.head(20)
print("Leeds_bus_routes plot")
leeds_bus_routes.plot()


import folium
leeds_coords = [53.8008, -1.5491]
m = folium.Map(leeds_coords, zoom_start=13)

folium.Choropleth(leeds_bus_routes.geometry,
                  line_weight= 3,
                  line_color= 'blue').add_to(m)

file_path = "/home/james/Desktop/maps/leeds_bus_routes_folium.html"
m.save(file_path)