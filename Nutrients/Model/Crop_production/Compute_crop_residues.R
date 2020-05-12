source('./Main/Global_functions.R')
source('./Nutrients/Model/Crop_production/Compute_crop_nutrient_offtake.R')
source('./Main/Data_operations.R')

## BURNT AND REMAINING AREAS------------------------------------------------------------------------ 


compute_crop_burnt_areas <- function(main_param, param) {
  # unit: ha burnt yr-1
  
  areas <- get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = main_param, pattern = param)
  
  # find FRAC_burnt for param
  FRAC_burnt <- get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Crops', subfolderX2 = 'Residues', pattern = 'Residues_FRACburnt')
  FRAC_burnt <- general_linear_extrapolation_3years(file_df = FRAC_burnt, existing_years = c('X1990','X1999','X2009'))
  FRAC_burnt <-  FRAC_burnt[which(FRAC_burnt[, 'crop']==param), ] 
  
  # find Cf for param
  cf <- get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Crops', subfolderX2 = 'Residues', pattern = 'Residues_Cf')
  cf <- find_crop_variable(df = cf, param_col = 'Crop', param = param, var = 'Cf')
  
  # calculation --- 
  yrs <- paste0('X', seq(1987,2017))
  areas[, yrs] <- sapply(yrs, function(x) round ( areas[,x] * FRAC_burnt[, x] * cf, 1))
  
  return(areas)
  rm(list=c('FRAC_burnt','cf'))
}


compute_remain_crop_areas <- function(main_param, param) {
  # unit: ha unburnt yr-1
  
  areas <- get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = main_param, pattern = param)
  burnt_areas <- compute_crop_burnt_areas(main_param, param)
  
  # calculation --- 
  yrs <- paste0('X', seq(1987,2017))
  areas[, yrs] <- sapply(yrs, function(x) round ( areas[,x] - burnt_areas[, x], 1))
  
  return(areas)
  rm(list=c('burnt_areas','yrs'))
}



## DM YIELDS ---------------------------------------

compute_crop_DM_yield <- function(main_param, param) {
  # unit: kg DM ha-1 yr-1
  
  if (main_param != 'Pastures') {
    
    yields <- get_spatially_disaggagregated_yields(main_param, param)
    FRAC_DM <- get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Crops', subfolderX2 = 'Residues', pattern = 'Residues_params')
    FRAC_DM <- find_crop_variable(df = FRAC_DM, param_col = 'Crop', param = param, var = 'DM_frac')
    
    # calculation --- 
    yrs <- paste0('X', seq(1987,2017))
    yields[, yrs] <- sapply(yrs, function(x) round ( yields[,x] * FRAC_DM, 1))
    
    return(yields)
    rm(list='FRAC_DM')
  }
  else {
    
    ifelse(param == 'Intensive_pasture', yields <- 4000, yields <- 2000)
  }
}


## ABOVEGROUND RESIDUES ----------------- 

compute_AG_biomass <- function(main_param, param) {
  
  
  residues_params <- get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Crops', subfolderX2 = 'Residues', pattern = 'Residues_params')
  slope <- find_crop_variable(df = FRAC_DM, param_col = 'Crop', param = param, var = 'slope')
  intercept <- find_crop_variable(df = FRAC_DM, param_col = 'Crop', param = param, var = 'intercept')
  
  yield <- compute_crop_DM_yield(main_param, param)
  
}

