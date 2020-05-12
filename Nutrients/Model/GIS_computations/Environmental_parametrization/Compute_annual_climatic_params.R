source('./Main/Global_functions.R')


library('gstat')



## MAIN CLIMATIC FUNCTIONS ---------------------------------------------------------------------------------------

get_Climatic_params_zipFiles <- function(climatic_param) {
  # climatic_param = 'ET0'
  
  param_filepath <- get_dir_files(module = 'Nutrients', folder = 'Activity_data', param_pattern = 'Environmental_params', subfolder = 'Climatic', subfolderX2 = 'Raw_data', file_pattern = climatic_param)
  return(param_filepath)
}

unzip_read_Climatic_params <- function(climatic_param) {
  
  param_zip <- get_Climatic_params_zipFiles(climatic_param)
  param_zip <- unzip(param_zip)
  
  r_file <- data.table::fread(param_zip)
  
  return(r_file)
  rm(list='param_zip')
}


delete_files <- function(climatic_param) {
  
  file <- list.files(path = './', pattern = climatic_param, full.names = T)
  file.remove(file)
  rm(list='file')
}




## COMPUTE ANNUAL CLIMATIC PARAMS  ---------------------------------------------------------------------------------------

## 1 - subset based on GRID_NO

## 2 - Sum annual climatic_param (1987-2017)

## 3 - repeat for each GRID_NO

## 4 - aggregate the different GRID_NO for a given year


compute_historical_annual_climatic_params_gridNO <- function(climatic_param, climatic_param_df, grid_no) {
  # FOR A GIVEN GRID_NO, COMPUTES THE ANNUAL CLIMATIC PARAM (E.G., ET0, AVG TEMPERATURE) for the period 1979-2017
  # source: AGRI4CAST
  
  grid_no_df <- subset(climatic_param_df, GRID_NO == as.character(grid_no))
  grid_no_df <- as.data.frame(grid_no_df)
  
  # create template df
  yrs <- as.character(seq(1979,2017))
  calc_df <- data.frame(GRID_NO = grid_no)
  calc_df[, yrs] <- sapply(yrs, function(x) calc_df[,x] <- 0)
  
  for (i in yrs) {
    
    # find the rows where substr(DAYS,1,4) == i 
    year_id <- which(substr(grid_no_df[,'DAY'], 1,4) == i)
    
    
    # compute annual statistics for climatic_param
    ifelse(climatic_param == 'Avg_temp',
      year_sum <- mean(grid_no_df[year_id, ncol(grid_no_df)]),
      year_sum <- sum(grid_no_df[year_id, ncol(grid_no_df)]))
    
    # insert into calc_df
    calc_df[1, as.character(i)] <- year_sum
  }
  return(calc_df)
}


compute_historical_annual_climatic_params_ALL <- function(climatic_param, climatic_param_df) {
  # COMPUTES THE ANNUAL CLIMATIC PARAMS FOR ALL CELLS  FOR THE SPECIFIED PARAM
  
  yrs <- as.character(seq(1979,2017))
  calc_df <- data.frame(GRID_NO = unique(climatic_param_df$GRID_NO))
  calc_df[,yrs] <- sapply(yrs, function(x) calc_df[,x] <- 0)
  
  for (i in 1:nrow(calc_df)) {
    
    print(paste0('==== Computing for GRID_NO ', calc_df[i, 'GRID_NO']))
    grid_no_sum <- compute_historical_annual_climatic_params_gridNO(climatic_param, climatic_param_df, grid_no = calc_df[i, 'GRID_NO'])
    calc_df[i,yrs] <- sapply(yrs, function(x) calc_df[i,x] <- grid_no_sum[1,x])
  }
  return(calc_df)
}


aggregate_historical_annual_climatic_params_dataset <- function(climatic_param_df, annual_param_df) {
  # AGGREGATES THE PREVIOUSLY COMPUTED ANNUAL CLIMATIC PARAM FOR ALL CELS
  # AND AGGREGATES THIS INTO A DATASET ACCORDING TO COORDINATES
  
  # select unique values from annual_param_df and merge with annual_param_df
  unique_df <- unique(subset(climatic_param_df, select = c('GRID_NO','LATITUDE','LONGITUDE','ALTITUDE')))
  annual_param_df <- plyr::join(annual_param_df, unique_df)
  
  # re-order columns
  annual_param_df <- annual_param_df[, c('GRID_NO','LATITUDE','LONGITUDE','ALTITUDE', seq(1979,2017))]
  
  return(annual_param_df)
}

loop_climatic_params <- function() {
  
  climatic_params <- c('ET0', 'Prec', 'Avg_temp')
  #climatic_params <- c('Avg_temp')
  
  for (i in climatic_params) {
    
    param <- unzip_read_Climatic_params(climatic_param = i)
    
    annual_calc <- compute_historical_annual_climatic_params_ALL(climatic_param = i, climatic_param_df = param)
    annual_calc <- aggregate_historical_annual_climatic_params_dataset(climatic_param_df = param, annual_param_df = annual_calc)
    export_file(module = 'Nutrients', 
                file = annual_calc, 
                filename = i, 
                folder = 'Activity_data',
                subfolder = 'Environmental_params',
                subfolderX2 = 'Climatic', 
                subfolderX3 = 'Processed_data')
    # delete unzipped file
    delete_files(climatic_param = i)
  }
}


## INTERPOLATING ANNUAL CLIMATIC PARAMS --------------------------------------------------------------------------


create_raster_template <- function(spatial_res) {
  
  clc <- get_activity_data(module = 'LULCC', folder = 'CLC', pattern = '1990', subfolder = 'Native')
  r <- raster(ext=extent(clc), crs = crs(clc), res = spatial_res)
  
  return(r)
  rm(list='clc')
}


SPdf_climatic_param <- function(climatic_param) {
  
  # get coordinates for the climatic_param --------------------------------------------------------
  sp <- get_activity_data(module = 'Nutrients',  folder = 'Environmental_params', subfolder = 'Climatic', subfolderX2 = 'Processed_data', pattern = climatic_param)
  sp <- na.omit(sp)
  SP <- SpatialPoints(sp[, 3:2], proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs'))
  SPdf <- SpatialPointsDataFrame(SP, sp)
  SPdf <- spTransform(SPdf, CRSobj = CRS('+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'))
  
  return(SPdf)
  rm(list=c('sp','SP'))
}


grid_25km_climatic_param <- function(climatic_param) {
  # create a 25x25 km grid for the climatic param
  # this is the default spatial resolution from the AGRI4CAST dataset
  
  # get coordinates for the climatic_param --------------------------------------------------------
  SPdf <- SPdf_climatic_param(climatic_param)
  
  r <- create_raster_template(25000)
  r_stack <- rasterize(SPdf[, paste0('X', seq(1979,2017))], r)
  
  return(r_stack)
  rm(list=c('r','SPdf'))
}

export_grid_25km_climatic_param <- function(climatic_param) {
  
  rstack <- grid_25km_climatic_param(climatic_param)
  
  yrs <- paste0('X', seq(1979,2017))
  for (i in yrs) {
    
    r_year <- rstack[[i]]
    export_file(module = 'Nutrients', 
                file = r_year, 
                filename = i, 
                folder = 'Environmental_params',
                subfolder = 'Climatic',
                subfolderX2 = climatic_param, 
                subfolderX3 = '25x25km')
  }
}


interpolate_IDW_1x1km_climatic_param <- function(climatic_param, spatial_res = 1000) {
  
  
  clc <- get_activity_data(module = 'LULCC', folder = 'CLC', pattern = '1990', subfolder = as.character(spatial_res))
  
  # get coordinates for the climatic_param --------------------------------------------------------
  r_temp <- create_raster_template(spatial_res)

  SPdf <- SPdf_climatic_param(climatic_param)

  yrs <- paste0('X', seq(1979,2017))
  
  for (i in yrs) {
    
    fm <- as.formula(paste(i, '~ 1'))
    gs <- gstat(formula = fm, locations=SPdf, nmax=5, set=list(idp=2))
    r_idw <- interpolate(r_temp, gs)
    r_idw <- resample(r_idw, clc)
    r_idw <- mask(r_idw, clc)
    export_file(module = 'Nutrients', 
                file = r_idw, 
                filename = i, 
                folder = 'Environmental_params',
                subfolder = 'Climatic',
                subfolderX2 = climatic_param, 
                subfolderX3 = '1x1km')
  }
  rm(list=c('clc','r_temp','SPdf','yrs','fm','gs','r_idw'))
}



loop_climatic_param_gridding <- function() {
  
  climatic_param <- c('ET0','Avg_temp')
  
  for (i in climatic_param) {
    
    export_grid_25km_climatic_param(climatic_param = i)
    interpolate_IDW_1x1km_climatic_param(climatic_param = i)
  }
}

