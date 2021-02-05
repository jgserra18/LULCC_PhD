source('./Nutrients/Model/Irrigation/Preprocessing/3_Preprocessing_SW_1987_2017.R')


compile_SW_nitrate_Watershed = function() {
  # compile the existing data averaged over each water shed
  # then interpolates the missing years
  # unit: mg NO3 L-1
  
  # create template raster
  r_tmp = get_activity_data(module = 'LULCC', folder = 'CLC', subfolder = '500', pattern = '1990')
  r_tmp <- raster(ext=extent(r_tmp), crs=crs(r_tmp), res=500)
  
  dataset = compile_SW_annual_mean_NO3_all_stations()
  hydro_shed = get_activity_data(module = 'LULCC', folder = 'Admin', pattern = 'Hydroshed')
  
  yrs = paste0(seq(1987,2017))
  
  # compile existing data
  for (yr in yrs) {
    
    yr_stations  = spatialize_SW_annual_mean_NO3_stations(yr, dataset)
    r_yr_stations <- rasterize(yr_stations, r_tmp, paste0('X', yr))
    hydro_shed[, paste0('X', yr)] = exactextractr::exact_extract(r_yr_stations, hydro_shed, 'mean')
  }
  
  # prepare interpolation dataframe
  df = hydro_shed[, paste0('X', yrs)]
  df = as.data.frame(df)
  df = df[, paste0('X', yrs)]
  names(df) = gsub('X','', names(df))
  
  # linearly interpolate missing years
  for (i in 1:nrow(df)) {
    print(i)
    condition = apply(df, 1, function(x) rowSums(is.na(df[i, ])))[1]
    
    if (condition == 31 | condition == 0) {
      next 
    }
    else {
      
      # select cols with NA (x_out)
      NAs_col_id = which(is.na(df[i,]) == TRUE)
      NA_yrs = names(df)[NAs_col_id]
      x_out = as.numeric(NA_yrs)
      
      # select yrs with data (x_in)
      NA_yrs_cols = unlist(lapply(NA_yrs, function(x) which(yrs==x)))
      x_in = as.numeric(yrs[-NA_yrs_cols])
      
      # predict
      pred = approx(x = x_in, y = df[i, as.character(x_in)], xout = x_out, rule=2)[2][[1]]
      df[i, as.character(x_out)] = pred
    }
  }
  
  # prepare for merging 
  hydro_shed = get_activity_data(module = 'LULCC', folder = 'Admin', pattern = 'Hydroshed')
  hydro_shed$id = seq(1, nrow(hydro_shed))
  names(df) = paste0('X', names(df))
  yrs = paste0('X', yrs)
  df[, 'id'] = seq(1,nrow(hydro_shed))
  
  hydro_shed = merge(hydro_shed, df, 'id')
  
  return(list(hydro_shed = hydro_shed, stations_dataset = dataset))
  rm(list=c('yrs','df','NAs_col_id','NA_yrs','x_out','NA_yrs_cols','x_in','pred', 'r_yr_stations'))
}



spatial_interpolation_over_watershed = function(year, hydroshed_interpolated_shp) {
  # useful to plot both the stations data and interpolated spatial data for a given year
  # unit:mg NO3 L-1
  
  dataset = hydroshed_interpolated_shp[[2]]
  hydro_shed = hydroshed_interpolated_shp[[1]]
  
  # create template raster
  r_tmp = get_activity_data(module = 'LULCC', folder = 'CLC', subfolder = '500', pattern = '1990')
  r_tmp <- raster(ext=extent(r_tmp), crs=crs(r_tmp), res=500)
  
  # call annual nitrate stations and rasterize
  yr_stations  = spatialize_SW_annual_mean_NO3_stations(year, annual_mean_NO3_all_stations_df = dataset)
  r_yr_stations <- rasterize(yr_stations, r_tmp, paste0('X', year))
  
  # compute average over each watershed and rasterize it
  #hydro_shed[, paste0('X', year)] = exactextractr::exact_extract(r_yr_stations, hydro_shed, 'mean')
  r_hydro = fasterize::fasterize(hydro_shed, r_tmp, paste0('X', year))
  r_hydro = general_RasterCrop_mainland(r_file = r_hydro, spatial_res = '500')
  
  return(list(avg_SW_hydro = r_hydro, point_file  = yr_stations))
  rm(list=c('r_tmp','yr_stations', 'r_yr_stations', 'hydro_shed'))
}


loop_spatial_interpolation_over_watershed = function(watershed_SW) {
  # exports NO3 data averaged over each watershed for 1987-2017, including linear interpolates
  
  yrs = paste0(seq(1987,2017))
  
  for (yr in yrs) {
    print(yr)
    SW_NO3_yr = spatial_interpolation_over_watershed(yr, watershed_SW)[[1]]
    export_file(module = 'Nutrients', folder = 'Irrigation', subfolder = 'Nitrate_modelling', subfolderX2 = 'SW', subfolderX3 = 'Spatial_prediction', file = SW_NO3_yr, filename = paste0('NO3_SW_', yr))
  }
}



# INTEPORLATE HYDROUNITS WITH MISSING VALUES -------------------------------------------

interpolate_surface_NO3_hydrosheds = function() {
  # linealry interpolates missing surface nitrate data for all portuguese watersheds
  # note: this replaces the output averaged N in surface waters
  # unit: mg N L-1
  yrs = paste0('X',seq(1987,2017))
  
  # prepare hydro shed df/shp 
  hydro_shed = get_activity_data(module = 'LULCC', folder = 'Admin', pattern = 'Hydroshed')
  hydro_shed$id = seq(1, nrow(hydro_shed))

  # surface no3 concentrations 
  for (yr in yrs) {
    
    yr = gsub('X','',yr)
    surf_no3 = raster(list.files(path = './Nutrients/Output/Irrigation/Nitrate_modelling/SW/Spatial_prediction/', pattern = yr, full.names = T))
    hydro_shed[, yr] = exactextractr::exact_extract(surf_no3, hydro_shed, 'mean')
  }
  df = as.data.frame(hydro_shed)
  df = df[, c('id', as.character(seq(1987,2017)))]
  
  # remove cols outside PT or = 31 NA values per watershed
  df = subset(df, rowSums(is.na(df))<31)
  
  # interpolate missing values
  all_yrs = names(df)[2:ncol(df)]
  
  for (i in 1:nrow(df)) {
    
    print(i)
    NA_cols = which(is.na(df[i,]))
    
    if (length(NA_cols)==0) {
      # if no NAs no need to make predictions
      next 
    }
    else {
      # start prediction for the ith watershed
      xout = as.numeric(names(df)[NA_cols]) # or NA_yrs
      available_yrs = all_yrs[-(NA_cols-1)]
      
      calc_df = data.frame( x = as.numeric(available_yrs),
                            y = as.vector(sapply(available_yrs, function(x) df[i, x])))
      lm_model <- lm(y~x, calc_df)
      lm_prediction <- round(
        predict(lm_model, newdata =  data.frame(x =  xout)), 2)
      # populate missing values with the lm prediciton
      df[i, NA_cols] = lm_prediction
    }
  }
  
  hydro_shed = get_activity_data(module = 'LULCC', folder = 'Admin', pattern = 'Hydroshed')
  hydro_shed$id = seq(1, nrow(hydro_shed))
  hydro_shed = merge(hydro_shed, df, 'id')

  return(hydro_shed)
  rm(list=c('calc_df','lm_model','lm_prediction','df','surf_no3',''))
}


export_interpolated_surface_NO3_watershed = function() {
  # final function to export spatially explicit average surface nitrate@watershed 1987-2017
  # unit: mg N L-1
  
  surface_no3 = interpolate_surface_NO3_hydrosheds()
  
  # create template raster
  r_tmp = get_activity_data(module = 'LULCC', folder = 'CLC', subfolder = '500', pattern = '1990')
  r_tmp <- raster(ext=extent(r_tmp), crs=crs(r_tmp), res=500)
  
  yrs = as.character(seq(1987,2017))
  for (yr in yrs) {
    
    # rasterize and export averag surface nitrate for each watershed
    r_hydro = fasterize::fasterize(surface_no3, r_tmp, yr)
    r_hydro = general_RasterCrop_mainland(r_file = r_hydro, spatial_res = '500')
    export_file(module = 'Nutrients', folder = 'Irrigation', subfolder = 'Nitrate_modelling', subfolderX2 = 'SW', subfolderX3 = 'Spatial_prediction', file = r_hydro, filename = paste0('NO3_SW_', yr))
  }
  rm(list=c('surface_no3','r_tmp','yrs','r_hydro'))
}

