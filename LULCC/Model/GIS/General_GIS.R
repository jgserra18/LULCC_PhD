source('./Model/Main/Set_directory.R')



fasterize_admin_statistical <- function(admin_res, file, name_id, name_field, spatial_res) {
  # fasterizes a given column of a specified file based on administrative boundaries
  # NUTS3, Parish, Municipality
  
  admin <- get_activity_data(folder = 'Admin', pattern = admin_res)
  ext1 <- get_activity_data(folder = 'Admin', pattern = 'Municipality')
  
  admin <- merge(admin, file, name_id)
  
  r_template <- raster(ext=extent(ext1), crs=crs(admin), res = spatial_res)
  r_statistical <- fasterize::fasterize(sf = admin, raster = r_template, field = name_field)
  
  return(r_statistical)
  rm(list=c('admin', 'r_template'))
}


fasterize__100m <- function(shp_file, col_field) {
  
  ext1 <- get_activity_data(folder = 'Admin', pattern = 'Municipality')
  r_template <- raster(ext=extent(ext1), crs=crs(ext1), res = 100)
  r_fasterize <- fasterize::fasterize(sf = shp_file, raster = r_template, field = col_field)
  
  return(r_fasterize)
  rm(list=c('ext1', 'r_template'))
}


resample_to_CLC <- function(raster_file, mask_CLC, spatial_res) {
  # resamples the rasterfiles to the CLC
  # this is because the extents are somewhat and somehow different
  # nothing major
  # additionally, there is an option to mask the rasterfile according to CLC
  
  clc <- get_activity_data('CLC', '2000', spatial_res)
  r_v2 <- resample(raster_file, clc)
  
  if (missing(mask_CLC)==TRUE) {
    return(r_v2)
  }
  else {
    r_v2[is.na(r_v2[])] <- 0
    r_v2 <- mask(r_v2, clc)

    return(r_v2)
  }
  rm(list='clc')
}

general_RasterCrop_admin <- function(r_file, admin, admin_id) {
  
  admin_shp <- get_activity_data(folder = 'Admin', pattern = admin)
  
  ifelse(class(admin_id)=='character',
    admin_shp <- subset(admin_shp, Admin_name==admin_id),
    admin_shp <- subset(admin_shp, Admin_id==admin_id))
  
  r_file <- crop(r_file, extent(admin_shp))
  r_file <- mask(r_file, admin_shp)
  r_file <- stack(r_file)
  
  return(r_file)
  rm(list='admin_shp')
}


disagregate_stacked_data <- function(r_stack, spatial_res, filename, subfolder) {
  
  r_stack <- disaggregate(r_stack, spatial_res)
  export_file(file = r_stack, folder = 'Stacked_params', filename = filename, subfolder = subfolder, subfolderX2 = spatial_res)
  rm(list='r_stack')
}


loop_disagregate_stacked_data <- function(r_stack, spatial_res, filename, subfolder) {
  
  if (length(spatial_res)==1) {
    disagregate_stacked_data(r_stack, spatial_res, filename, subfolder)
  } 
  else if (length(spatial_res)>1) {
    
    for (i in spatial_res) {
      print(paste0('Working in ', i))
      disagregate_stacked_data(r_stack, i, filename, subfolder)
    }
  }
}
