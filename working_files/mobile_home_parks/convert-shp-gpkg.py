import os
import geopandas as gpd

if __name__ == '__main__':
    # Import shapefile (as exported from ArcGIS)
    file = 'mobile-home-parks-geocode.shp'
    gdf = gpd.read_file(file)
    
    # Convert to geopackage
    gdf.to_file('mobile-home-parks-geocode.gpkg', driver='GPKG')