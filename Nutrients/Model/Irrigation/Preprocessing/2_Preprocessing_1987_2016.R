source('./Main/Global_functions.R')

library(sf)

# GW -------------------------------------------------------------------------------------



select_station_years = function(year, all_stations_df) {
  # subets based on coordinates, station id and the year
  
  if (as.numeric(year)<1991) {
    stop('Limited data: 1981,1984,1985, 1991-2016.')
  }
  else {
    
    year = paste0('X', year)
    year_cols  = which(grepl(year, names(all_stations))==TRUE)
    all_stations_df = all_stations_df[, c(1,2,3, year_cols)]
    
    return(all_stations_df)
  }
}



compute_annual_mean_NO3 = function(year, all_stations_df) {
  
  annual_stations = select_station_years(year, all_stations_df)
  
  # calculate mean average nitrates for each station ;; if only NAs will be ignored
  if (ncol(annual_stations)==4) {
    names(annual_stations)[4] = 'Avg_NO3'
  }
  else {
    annual_stations[, 'Avg_NO3'] = round(rowMeans(annual_stations[, -seq(1,3)], na.rm=T), 2)
  }
  # remove NaNs
  annual_stations = annual_stations[-which(is.nan(annual_stations[, 'Avg_NO3'])==TRUE), ]
  # select only wanted cols
  annual_stations = annual_stations[, c(1,2,3, ncol(annual_stations))]
  
  return(annual_stations)
}


spatialize_annual_mean_NO3 = function(year, all_stations_df) {
  # returns two things:
  # 1 - stations_df with the average mean NO3 data and station coordinates
  # 2 - Point #1 but as SPdf
  
  annual_avg_NO3 = compute_annual_mean_NO3(year, all_stations_df)
  SP <- SpatialPoints(annual_avg_NO3[, 1:2],  
                      proj4string = CRS('+proj=tmerc +lat_0=39.66666666666666 +lon_0=-8.131906111111112 +k=1 +x_0=200000 +y_0=300000 +ellps=intl +towgs84=-288.885,-91.744,126.244,-1.691,0.410,-0.211,-4.598 '))
  SPdf <- SpatialPointsDataFrame(SP, annual_avg_NO3)
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
  
  return(list(stations_df = annual_avg_NO3, stations_shp = SPdf))
}

#all_stations  = get_activity_data(module = 'Nutrients', folder = 'Nutrient_params', subfolder = 'N', subfolderX2 = 'Irrigation', subfolderX3 = 'GW_monitoring_station',  subfolderX4 = '1987-2016', pattern = 'GW')
#no3_2000 = spatialize_annual_mean_NO3(2016, all_stations)


