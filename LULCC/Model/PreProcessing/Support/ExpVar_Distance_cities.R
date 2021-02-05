source('./Main/Global_functions.R')
source('./Main/General_GIS.R')

# computes distance to the ocean and distance to urban areas ------------------------
# spatial res is 500m 

compute_standard_distance_urban_regions = function() {
  # src : Corine Land  Cover
  # default res: 500m
  # unit: metwers
  
  # prepare data
  pt = read_sf('./LULCC/Activity_data/Admin/Agrarian_region.shp')
  r_tmp = raster('./LULCC/Activity_data/CLC/500m/CLC_PT1990.tif')
  r_tmp[r_tmp<143] = 1 # reclassify urban regions to 1
  r_tmp[r_tmp>143] = 0 # reclassify non urban regions to 0
  
  
  dist = gridDistance(r_tmp, origin=1) # calculate distance from urbban regions
  dist = mask(crop(dist, extent(pt)), pt)
  
  export_file(module = 'LULCC', file = dist, folder = 'Exploratory_variables', filename = 'Distance_urban', subfolder = 'MRB', subfolderX2 = '500m')
  rm(list=c('pt','r_tmp','dist'))
}


compute_standard_distance_ocean = function() {
  # src : Corine Land  Cover
  # unit: meters 
  
  pt = read_sf('./LULCC/Activity_data/Admin/Agrarian_region.shp')
  ocean = raster('./LULCC/Activity_data/Physical_constraints/LU_ocean.tif')
  ocean  = ocean==44
  ocean = aggregate(ocean, 5, fun=sum)
  ocean = ocean>0
  
  dist = gridDistance(ocean, origin=1) # calculate distance from urbban regions
  dist = mask(crop(dist, extent(pt)), pt)
  
  export_file(module = 'LULCC', file = dist, folder = 'Exploratory_variables', filename = 'Distance_ocean', subfolder = 'MRB', subfolderX2 = '500m')
  rm(list=c('pt','ocean','dist'))
}



