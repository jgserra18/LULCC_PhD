source('./Main/Global_functions.R')

# pre processing of monitoring stations 2017 ----------------------------------------------------------------------

#* delete first 2 rows; deleted 4 last rows
#* delete cols starting from 3, as t + 2


preprocess_monitoring_stations_X2017 = function(station_df) {
  # preprocess SNIRH monitoring stations data for 2017
  # first 2 rows must be cleaned already
  
  
  file = station_df
  last_rows = seq(ncol(file)-3,)
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
  
  # mean NO3 value for all stations
  annual_mean  = round(colMeans(df[, -1], na.rm=T), 2)
  annual_mean = as.data.frame.list(annual_mean)
  stations = names(annual_mean)
  
  annual_mean_df = data.frame(stations=stations,
                              AVG_NO3 = data.table::transpose(annual_mean))
  names(annual_mean_df)[2] = 'Avg_NO3'
  
  
  return(annual_mean_df)
  rm(list=c('file','df','annual_mean','stations'))
}


compile_stations_X2017 = function(list_file_path) {
  # compile all stations for a given list_file_path into one common dataset with the annual average NO3 data
  
  store_X2017 = data.frame()
  
  for (file in list_file_path) {
    
    stations_file = read.csv(file)
    stations_file = preprocess_monitoring_stations_X2017(stations_file)
    store_X2017 = rbind(store_X2017, stations_file)
  }
  # remove NaN
  store_X2017 = store_X2017[-which(is.nan(store_X2017[, 2])==TRUE), ]
  
  return(store_X2017)
  rm(list=c('stations_file'))
}



list_X2017_files = function(stations_source) {
  
  if (stations_source == 'GW') {
    
    files = list.files(path = './Nutrients/Activity_data/Nutrient_params/N/Irrigation/GW_monitoring_stations/2017/', pattern = '.csv', full.names = TRUE)
  }
  else {
    files = list.files(path = './Nutrients/Activity_data/Nutrient_params/N/Irrigation/SW_monitoring_stations//2017/', pattern = '.csv', full.names = TRUE)
  }
  return(files)
}


spatialize_stations_X2017 = function(stations_source) {
  # e.g., spatialize_stations_X2017('GW')
  # returns two things:
  # 1 - stations_df with the average mean NO3 data and station coordinates
  # 2 - Point #1 but as SPdf
  
  list_file_path = list_X2017_files(stations_source)
  gw_X2017 = compile_stations_X2017(list_file_path)
  
  # pre process
  gw_X2017$stations = gsub('X','', gw_X2017$stations)
  
  stations_info  = get_activity_data(module = 'Nutrients', folder = 'Nutrient_params', subfolder = 'N', subfolderX2 = 'Irrigation', subfolderX3 = paste0(stations_source, '_monitoring_station'), pattern = paste0(stations_source, '_stations_2017'))
  stations_info$stations = gsub('/','.', stations_info$stations)
  stations_info = stations_info[, c('stations','COORD_X','COORD_Y')]
  names(stations_info)[c(2,3)] = c('x','y')
  
  # join
  stations = plyr::join(gw_X2017, stations_info)
  
  # spatialize
  SP <- SpatialPoints(stations[, 3:4],  
                      proj4string = CRS('+proj=tmerc +lat_0=39.66666666666666 +lon_0=-8.131906111111112 +k=1 +x_0=200000 +y_0=300000 +ellps=intl +towgs84=-288.885,-91.744,126.244,-1.691,0.410,-0.211,-4.598 '))
  SPdf <- SpatialPointsDataFrame(SP, stations)
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
  
  return(list(stations_df = stations, stations_shp = SPdf))
  rm(list=c('gw_X2017','list_file_path','stations_info','SP'))
}









