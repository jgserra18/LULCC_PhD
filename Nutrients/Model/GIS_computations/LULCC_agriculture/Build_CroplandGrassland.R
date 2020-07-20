source('./Main/General_GIS.R')



create_mainland_annual_NUTS2_raster_mosaic <- function(spatial_res, year) {
  # CURRENTLY ONLY AVAILABLE FOR 500m
  # creates a raster mosaic based on the annual LULC map for each NUTS2 region
  
  nuts2_id <- c(18,15,16,11,17)
  r_list <- list()
  
  if (as.integer(year)<1990) {
    
    year = '1990'
  }
  
  for (i in 1:length(nuts2_id)) {
    
    
    file_yr <- get_dir_files(module = 'LULCC', folder = 'Output', param_pattern = 'LULC', subfolder = 'NUTS2', subfolderX2 = nuts2_id[i], mainfolder = spatial_res, file_pattern = year)
    r_file <- raster(file_yr)
    r_list <- append(r_file, r_list)
  }
  
  r_list$fun = sum
  r_list <- do.call(mosaic, r_list)
  clc_mask <- get_activity_data(module = 'LULCC', folder = 'CLC', pattern = '1990', subfolder = '500')
  r_list <- mask(crop(r_list, extent(clc_mask)), clc_mask)
  
  return(r_list)
  rm(list=c('file_yr','r_file','clc_mask'))
}


compute_annual_LULC_cropland = function(year, spatial_res = '500', LULC) {
  # computes the annual cropland at a default spatial resolution of 500m
  
  year = gsub('X','', year)
  
  yr_clc = create_mainland_annual_NUTS2_raster_mosaic(spatial_res, year)
  clc_cropland = get_activity_data(module = 'LULCC', folder = 'CLC', pattern = 'CLC_CroplandGrassland')
  
  if (missing(LULC)==TRUE | LULC == 'Cropland') {
    
    clc_cropland[which(clc_cropland$category_id != 1), 'category_id'] = 0
  }
  else if (LULC == 'Grassland') {
    
    clc_cropland[which(clc_cropland$category_id != 2), 'category_id'] = 0
  }
  
  clc_cropland = clc_cropland[, -4]
  yr_cropland = reclassify(yr_clc, rcl = as.matrix(clc_cropland))
  
  return(yr_cropland)
  rm(list=c('year','yr_clc','clc_cropland'))
}


