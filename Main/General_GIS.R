source('./Main/Global_functions.R')




fasterize_admin_statistical <- function(module, admin_res, file, name_id, name_field, spatial_res) {
  # fasterizes a given column of a specified file based on administrative boundaries
  # NUTS3, Parish, Municipality
  
  admin <- get_activity_data(module, folder = 'Admin', pattern = admin_res)
  ext1 <- get_activity_data(module, folder = 'Admin', pattern = 'Municipality')
  
  admin <- merge(admin, file, name_id)
  
  r_template <- raster(ext=extent(ext1), crs=crs(admin), res = spatial_res)
  r_statistical <- fasterize::fasterize(sf = admin, raster = r_template, field = name_field)
  
  return(r_statistical)
  rm(list=c('admin', 'r_template'))
}


fasterize__100m <- function(module, shp_file, col_field) {
  
  ext1 <- get_activity_data(module, folder = 'Admin', pattern = 'Municipality')
  r_template <- raster(ext=extent(ext1), crs=crs(ext1), res = 100)
  r_fasterize <- fasterize::fasterize(sf = shp_file, raster = r_template, field = col_field)
  
  return(r_fasterize)
  rm(list=c('ext1', 'r_template'))
}


resample_to_CLC <- function(module='LULCC', raster_file, mask_CLC, spatial_res, ngb) {
  # resamples the rasterfiles to the CLC
  # this is because the extents are somewhat and somehow different
  # nothing major
  # additionally, there is an option to mask the rasterfile according to CLC
  
  clc <- get_activity_data(module, 'CLC', '2000', spatial_res)
  
  if (missing(ngb)==TRUE) {
    r_v2 <- resample(raster_file, clc)
  }
  else {
    r_v2 <- resample(raster_file, clc, method='ngb')
  }

  
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



general_RasterCrop_admin <- function(module, r_file, admin, admin_id) {
  
  admin_shp <- get_activity_data(module, folder = 'Admin', pattern = admin)
  
  ifelse(class(admin_id)=='character',
    admin_shp <- subset(admin_shp, Admin_name==admin_id),
    admin_shp <- subset(admin_shp, Admin_id==admin_id))
  
  r_file <- crop(r_file, extent(admin_shp))
  r_file <- mask(r_file, admin_shp)
  r_file <- stack(r_file)
  
  return(r_file)
  rm(list='admin_shp')
}



statistical_downscaling = function(r_file, r_exp_vars, spatial_res) {
  
  r_file = resample_to_CLC('LULCC', r_file, T, spatial_res)
  
  clc <- get_activity_data('LULCC', 'CLC', '2000', spatial_res)
  lon = init(clc, 'x')
  lat = init(clc, 'y')
  
  exp_vars = stack(lon, lat, r_exp_vars)
  
  
  downscaling = spatialEco::raster.downscale(x = exp_vars, y = r_file, scatter = T)
  
  return(downscaling)
  rm(list=c('r_file','clc', 'exp_vars'))
}

compute_raster_corr = function(r_stack, names_r_stack, formula) {
  # for raster predict
  # r_pred = predict(r_stack, model, progress =)
  
  
  fm = as.formula(formula)
  names(r_stack) = names_r_stack
  df = as.data.frame(r_stack, xy=T, na.rm=T)
  
  model = lm(formula, data = df)
  r2 = summary(model)$r.squared
  
  return(r2)
}





