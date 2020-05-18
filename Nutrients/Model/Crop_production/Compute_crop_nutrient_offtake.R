source('./Main/Global_functions.R')


# 1 - disaggregate crop yields from the AR to the municipality scale

# 2 - loop each crop param and calculate production (in tonnes DM)

# 3 - Calculate crop N and P offtake 
  # DON'T FORGET TO CONVERT P205 OFFTAKE TO P

# call crop C/N ration and calculate C offtake


##  SUPPORT FUNCTIONS ---------------------------------------------------------------------------


get_crop_yields <- function(main_param, param) {
  
  if (param == 'Other_dried_pulses') {
    
    AR_yields <-  get_activity_data(module = 'Nutrients', folder = 'Raw_data_Agrarian', subfolder = 'Yields', subfolderX2 = main_param, pattern = 'Beans')
  }
  else if (param == 'other_fresh') {
    
    AR_yields <-  get_activity_data(module = 'Nutrients', folder = 'Raw_data_Agrarian', subfolder = 'Yields', subfolderX2 = main_param, pattern = 'Cherry')
  }
  else {
    AR_yields <-  get_activity_data(module = 'Nutrients', folder = 'Raw_data_Agrarian', subfolder = 'Yields', subfolderX2 = main_param, pattern = param)
  }
  return(AR_yields)
}

get_spatially_disaggagregated_yields <- function(main_param, param) {
  # spatially disaggregates crop yields into the municipality level
  # format: Muni_ID, Muni, ID, 1987, ....
  
  # get AT yields
  # specification: Other_dried_pulses --> Beans
  AR_yields <- get_crop_yields(main_param, param)
  names(AR_yields)[1] <- 'agrarian_region_id'
  
  # spatially disaggregate 
  disagg_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Spatial_disaggregation')
  AR_yields <- plyr::join(x = disagg_df, y = AR_yields, by = 'agrarian_region_id')
  AR_yields <- AR_yields[, -c(seq(4,8))]
  
  return(AR_yields)
  rm(list='disagg_df')
}


find_crop_variable <- function(df, param_col, param, var) {
  # var is the variable to subset the dataset
  # e.g., find_crop_variable(df = FRAC_DM, param_col = 'crop', param = 'Oat', var = 'DM_frac')
  
  find_crop <- df[which(df[, param_col]==param), ] 
  select_var <- find_crop[, var]
  return(select_var)
  rm(list='find_crop')
}




## COMPUTE DRY-MATTER PRODUCTION ---------------------------------------------------------------------------


compute_crop_DM_production <- function(main_param, param) {
  # computes the dry-matter production for a given crop
  # unit: tonnes DM yr-1
  
  
  # get data ------------------------------------------------
  FRAC_DM <- get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Crops', subfolderX2 = 'Offtake', pattern = 'DM_content')
  FRAC_DM <- find_crop_variable(df = FRAC_DM, param_col = 'crop', param = param, var = 'DM_frac')
  
  areas <- get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = main_param, pattern = param)
  yields <- get_spatially_disaggagregated_yields(main_param, param)
  
  # compute DM production (in kg DM yr-1)
  yrs <- paste0('X', seq(1987,2017))
  Prod_DM <- yields
  Prod_DM[, yrs] <- sapply(yrs, function(x) round(yields[, x] * areas[, x] * FRAC_DM / 1000, 1))
  
  return(Prod_DM)
  rm(list=c('FRAC_DM','areas','yields','yrs'))
}


## COMPUTE NUTRIENT OFFTAKE OF DRY MATTER PRODUCTION ---------------------------------------------------------------------------

convert_offtake_P2O5_to_P <- function() {
  # conversion factor : P2O5 to P = 0.4364
  
  P2O5_offtake <- get_activity_data(module = 'Nutrients', folder = 'Nutrient_params', subfolder = 'P', subfolderX2 = 'Crops',subfolderX3 = 'Offtake', pattern = 'offtake')
  P_offtake <- P2O5_offtake
  P_offtake[,3] <- P_offtake[,3] * 0.4364
  
  return(P_offtake)
  rm(list='P2O5_offtake')
}


compute_nutrient_offtake <- function(main_param, param, nutrient) {
  # calculates the nutrient offtake of a given crop
  # unit: kg Nutrient yr-1 
  
  
  print(paste0('===========Computing ', nutrient, ' offtake for ', param))
  Prod_DM <- compute_crop_DM_production(main_param, param)
  
  # select appropriate offtake coeficcient
  if (nutrient == 'N') {
    
    nutrient_offtake <- get_activity_data(module = 'Nutrients', folder = 'Nutrient_params', subfolder = nutrient, subfolderX2 = 'Crops', subfolderX3 = 'Offtake', pattern = 'offtake')
  }
  else if (nutrient == 'P') {
    
    nutrient_offtake <- convert_offtake_P2O5_to_P()
  }
  else {
    
    nutrient_offtake <- get_activity_data(module = 'Nutrients', folder = 'Nutrient_params', subfolder = nutrient, subfolderX2 = 'Crops', subfolderX3 = 'Offtake', pattern = 'offtake')
  }
  
  # select nutrient offtake for the crop
  # Unit: kg Nutrient ton DM-1 yr-1
  nutrient_offtake_var <- find_crop_variable(df = nutrient_offtake, param_col = 'crop', param = param, var = 'Avg_offtake')
  
  yrs <- paste0('X', seq(1987,2017))
  nutrient_offtake <- Prod_DM
  nutrient_offtake[, yrs] <- sapply(yrs, function(x) round(nutrient_offtake_var * Prod_DM[, x], 1))
  
  return(nutrient_offtake)
  rm(list=c('Prod_DM','yrs'))
}



compute_all_crop_nutrient_offtake <- function(nutrient) {
  # computes nutrient offtake for each crop for the specified nutrient (P,C,N)
  # unit: kg nutrient yr-1
  
  standard_params <- get_activity_data(module = 'Nutrients', folder = 'General_params', pattern = 'Params_list')
  
  param_col <- 'Crop'
  main_param_col <- 'Main_crop'
  
  
  for (i in 1:nrow(standard_params)) {
    
    main_param <- standard_params[i, main_param_col]
    param <- standard_params[i, param_col]
    
    if (main_param == 'Pastures' | main_param == 'Forage') {
      next
    }
    else {
      param_interpol <- compute_nutrient_offtake(main_param, param, nutrient)
      export_file(module = 'Nutrients', 
                  file = param_interpol, 
                  filename = param, 
                  folder = 'Crop_offtake', 
                  subfolder = nutrient, 
                  subfolderX2 = main_param)
    }
  }
  rm(list=c('standard_params','param_col','main_param_col','param_interpol'))
}




loop_nutrient_offtake <- function() {
  
  nutrient <- c('C','N','P')
  sapply(nutrient, compute_all_crop_nutrient_offtake)
}



compute_total_nutrient_offtake('N')
compute_total_nutrient_offtake <- function(nutrient) {
  
  yrs <- paste0('X', seq(1987,2017))
  store_main_param <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  store_main_param[, yrs] <- sapply(yrs, function(x) store_main_param[,x] <- 0)
  
  standard_params <- get_standard_params_list(main_param = 'Crops')
  
  for (i in 1:nrow(standard_params)) {
    
    main_param <- standard_params[i, 1]
    param <- standard_params[i,2]
    
    if (main_param == 'Pastures' | main_param == 'Forage') {
      next
    }
    else {
      
      crop_offtake <- get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Crop_offtake', subfolder = nutrient, subfolderX2 = main_param, pattern = param)
      store_main_param[, yrs] <- sapply(yrs, function(x) round(store_main_param[,x] + crop_offtake[,x], 1))
    }
  }
  export_file(module = 'Nutrients', 
              file = store_main_param, 
              filename = 'Total_sum', 
              folder = 'Crop_offtake', 
              subfolder = nutrient, 
              subfolderX2 = 'Total')
}
