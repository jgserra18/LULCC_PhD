source('./Nutrients/Model/Irrigation/Preprocessing/1_Preprocessing_2017.R')



list_SW_1987_2017_files = function(stations_source='SW') {
  #* from "1_Preprocessing_2017.R"

  files = list_X2017_files(stations_source)
  return(files)
}


compute_annual_mean_NO3_SW_stationfiles = function(station_df) {
  # computes the mean average NO3 in all the  SW stations within a fil
  # unit: mg NO3 L-1
  
  file = station_df
  last_rows = seq(nrow(file)-3,nrow(file), 1)
  # delete last 3 rows with metadata
  file = file[-last_rows, ]
  # create new df to store correct values
  df = data.frame(date=file[, 1])
  
  for (c in 2:ncol(file)) {
    
    condition = which(grepl(pattern  = 'FLAG', x = file[, c]) == TRUE)
    if (length(condition) == 0 ) {
      n = names(file)[c]
      df[, as.character(n)] = file[, c]
    }
  }
  
  # small preprocessing related to colnames and structure
  names(df) = gsub('X','', names(df))
  df = sapply(df, as.numeric)
  df = as.data.frame(df)
  df[, 'date'] = file[,1]
  df = df[-1,]
  
  df$year = substr(df$date,  7, 10)
  df = df[, c('date','year', names(df[, -c(1, ncol(df))]))]

  # creat a dataframe to collate all annual data
  yrs = unique(df$year)
  annual_df = data.frame(yr=yrs)
  annual_df[, names(df[, -c(1,2)])] = NA
  
  # calculate theavverage annual nitrate data for each station and adds to the annual df
  ctr = 0
  for (yr in yrs) {
    
    ctr = ctr + 1
    sb_df  = subset(df, year == yr)
    annual_df[ctr, 2:ncol(annual_df)] = round(colMeans(sb_df[, -c(1,2)], na.rm=T), 3)
  }
  
  return(annual_df)
  rm(list=c('df','yrs','sb_df'))
}


compile_SW_annual_mean_NO3_all_stations = function() {
  # compile all stations individual files into one big dataset with the average annual NO3 cocnentrations
  # unit: mg NO3 L-1
  
  df = data.frame(yr=seq(1987,2017))
  
  all_files = list_SW_1987_2017_files()
  
  for (file in all_files) {
    
    r_file = read.csv(file)
    r_file = compute_annual_mean_NO3_SW_stationfiles(r_file)
    df = plyr::join(df, r_file)
  }
  
  new_df = data.frame(stations = names(df[,-1]))
  new_df[, as.character(seq(1987,2017))] = data.table::transpose(df)[-1, ]
  
  # prepare for spatialization
  station_info = get_activity_data(module = 'Nutrients', folder = 'Nutrient_params', subfolder = 'N', subfolderX2 = 'Irrigation', subfolderX3 = 'SW_monitoring_stations', pattern = 'stations')
  new_df = plyr::join(new_df, station_info[, c('stations','COORD_X','COORD_Y')])
  new_df = new_df[, c('stations','COORD_X','COORD_Y', as.character(seq(1987,2017)))]
  new_df[, c(2,3)] = sapply(c(2,3), function(x) as.numeric(new_df[, x]))

  return(new_df)
  rm(list=c('r_file','file','all_files', 'new_df'))
}


spatialize_SW_annual_mean_NO3_stations = function(year, annual_mean_NO3_all_stations_df) {
  
  dataset = annual_mean_NO3_all_stations_df
  dataset = dataset[, c('stations','COORD_X','COORD_Y', year)]
  dataset = na.omit(dataset)
  names(dataset)[4] = paste0('X', year)
  
  SP <- SpatialPoints(dataset[, 2:3],  
                      proj4string = CRS('+proj=tmerc +lat_0=39.66666666666666 +lon_0=-8.131906111111112 +k=1 +x_0=200000 +y_0=300000 +ellps=intl +towgs84=-288.885,-91.744,126.244,-1.691,0.410,-0.211,-4.598 '))
  SPdf <- SpatialPointsDataFrame(SP, dataset)
  # reproject it to LAEA
  SPdf <- spTransform(SPdf, 
                      CRS('+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs'))
  # fine tune the extent 
  new_bb <- c(2635900,  1729700, 2977200, 2298200)
  names(new_bb) = c("xmin", "ymin", "xmax", "ymax")
  attr(new_bb, "class") = "bbox"
  SPdf <- st_as_sf(SPdf)
  attr(st_geometry(SPdf), "bbox") <- new_bb
  SPdf <- sf::as_Spatial(SPdf)
  SPdf <- st_as_sf(SPdf)
  
  return(SPdf)
}





compile_SW_nitrate_Watershed = function() {
  # compile the existing data averaged over each water shed
  # then interpolates the missing years
  # unit: mg NO3 L-1
  
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
  hydro_shed[, paste0('X', year)] = exactextractr::exact_extract(r_yr_stations, hydro_shed, 'mean')
  r_hydro = fasterize::fasterize(hydro_shed, r_tmp, paste0('X', year))
  r_hydro = general_RasterCrop_mainland(r_file = r_hydro, spatial_res = '500')
  
  return(list(avg_SW_hydro = r_hydro, point_file  = yr_stations))
  rm(list=c('r_tmp','yr_stations', 'r_yr_stations', 'hydro_shed'))
}




loop_spatial_interpolation_over_watershed()
loop_spatial_interpolation_over_watershed = function() {
  # exports NO3 data averaged over each watershed for 1987-2017, including linear interpolates
  watershed_SW = compile_SW_nitrate_Watershed()
  yrs = paste0('X', seq(1987,2017))
  
  for (yr in yrs) {
    
    SW_NO3_yr = spatial_interpolation_over_watershed(yr, watershed_SW)[[1]]
    export_file(module = 'Nutrients', folder = 'Output', subfolder = 'Irrigation', subfolderX2 = 'Nitrate_modelling', subfolderX3 = 'SW', subfolderX4 = 'Spatial_prediction', file = SW_NO3_yr, filename = paste0('NO3_SW_', yr))
  }
}


watershed_SW = compile_SW_nitrate_Watershed()


d = spatial_interpolation_over_watershed('1999', watershed_SW)


