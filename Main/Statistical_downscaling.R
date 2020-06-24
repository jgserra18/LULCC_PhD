source('./Main/Global_functions.R')
source('./Main/General_GIS.R')
source('./Main/Data_operations.R')



statistical_downscaling = function(r_file, r_exp_vars, spatial_res) {
  # statistical downscaling using spatialEco library
  # the exploratory vars should be inputed as a raster_stack
  # ideally these should correlate with the independent variable
  
  #* EXAMPLE:
  #* prec = raster('./Nutrients/Output/Environmental_params/Climatic/Prec//25x25km/X1979.tif')
  #* elev = raster('./LULCC/Activity_data/Environmental_params/1000m//Elevation.tif')
  #*  prec = crop(prec, extent(elev))
  #*  prec = resample(prec, elev, 'ngb')
  #* lon = init(prec, 'x')
  #* lat = init(prec, 'y')
  #* stt = stack(elev, lon, lat)
  #* prec_ds = spatialEco::raster.downscale(x = stt, y = prec, scatter = T)

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
