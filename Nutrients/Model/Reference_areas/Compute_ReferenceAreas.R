source('./Main/Global_functions.R')



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


loop_municipality_UAA <- function(mainland_uaa) {
  
  
  if (mainland_uaa == TRUE) {
    
    yrs <- seq(1987, 2017)
    new_df <- data.frame(yrs = yrs)
    
    for (i in 1:nrow(new_df)) {
      
      uaa <- compute_municipality_annual_UAA(new_df[i,1])
      uaa <- sum(uaa[, 'uaa'])
      new_df[i, 'uaa'] <- uaa
    }
    return(new_df)
    rm(list=c('yrs','uaa'))
  }
  else {
    
    muni_store <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
    yrs <- seq(1987, 2017)
    
    for (i in seq_along(yrs)) {
      print(paste0('Calculating municipality_UAA for ', yrs[i]))
      uaa <- compute_municipality_annual_UAA(yrs[i])
      muni_store[, paste0('X',yrs[i])] <- uaa[, 'uaa']
      
    }
    export_file(module = 'Nutrients', file = muni_store, filename = 'UAA', folder = 'Output', subfolder = 'Reference_areas', subfolderX2 = 'UAA')
  }
}
loop_municipality_UAA(mainland_uaa = FALSE)


