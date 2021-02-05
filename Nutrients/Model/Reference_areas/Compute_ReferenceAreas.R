source('./Main/Global_functions.R')


# UAA ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

compute_municipality_annual_UAA <- function(year) {
  # computes annual UAA for the specified year
  
  
  yr <- paste0('X', year)
  
  standard_params <- get_activity_data(module = 'Nutrients', folder = 'General_params', pattern = 'Params_list')
  param_col <- 'Crop'
  main_param_col <- 'Main_crop'
  
  muni_store <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  
  for (i in 1:nrow(standard_params)) {
    
    main_param <- standard_params[i, main_param_col]
    param <- standard_params[i, param_col]
    
    # call crop df, select only the given year and populate muni_store data.frame
    crop_df <- get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = main_param, pattern = param)

    # populate muni_store dataframe
    muni_store[, param] <- crop_df[, yr]
  }
  muni_store[, 'uaa'] <- rowSums(muni_store[, -c(1,2,3)])
  
  return(muni_store)
  rm(list=c('standard_params','param_col','main_param_col','main_param','param','crop_df'))
}


loop_municipality_UAA <- function() {
  
  
  muni_store <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  yrs <- seq(1987, 2017)
  
  for (i in seq_along(yrs)) {
    print(paste0('Calculating municipality_UAA for ', yrs[i]))
    uaa <- compute_municipality_annual_UAA(yrs[i])
    muni_store[, paste0('X',yrs[i])] <- uaa[, 'uaa']
    
  }
  export_file(module = 'Nutrients', file = muni_store, filename = 'UAA', folder = 'Reference_areas', subfolder = 'UAA')
}


# ARABLE LAND ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

compute_municipality_annual_arableLand <- function() {
  # computes arable land per municipality (UAA - perma grass)
  # unit: ha 
  
  yrs = paste0('X',seq(1987,2017))
  
  standard_params <- get_standard_params_list(main_param = 'Crops')
  param_col <- 'Crop'
  main_param_col <- 'Main_crop'
  
  yrs = paste0('X',seq(1987,2017))
  muni_store <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  muni_store[, yrs] = sapply(yrs, function(x) muni_store[,x] = 0)
  
  for (i in 1:nrow(standard_params)) {
    
    main_param <- standard_params[i, main_param_col]
    param <- standard_params[i, param_col]
    
    if (param == 'Extensive_pasture') {
      next 
    }
    else {
      
      # call crop df, select only the given year and populate muni_store data.frame
      crop_area <- get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = main_param, pattern = param)
      
      # populate muni_store dataframe
      muni_store[, yrs] = sapply(yrs, function(x) round(muni_store[,x] + crop_area[,x], 1))
    }
  }

  return(muni_store)
  rm(list=c('standard_params','param_col','main_param_col','main_param','param','crop_df'))
}


export_municipality_ArableLand <- function() {
  
  arable_land = compute_municipality_annual_arableLand()
  export_file(module = 'Nutrients', file = arable_land, filename = 'Arable_land', folder = 'Reference_areas', subfolder = 'Arable_land')
}
