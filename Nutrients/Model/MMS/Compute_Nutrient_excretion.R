source('./Main/Global_functions.R')


## PHOSPHORUS AND NITROGEN IMPLEMENTATION -------------------------------------------------------------------------------------


select_animal_Nutrient_excretion_coefficient <- function(nutrient, param) {
  # finds the nutrient excretion rate for a given nutrient and animal
  # param is Animals (e.g., Non_dairy, etc)
  
  # get nutrient excretion coefficient datasets
  if (param == 'Dairy_cows') {
    
    animal_coef <- get_activity_data(module = 'Nutrients', subfolder = 'Nutrient_params', subfolderX2 = 'P', subfolderX3 = 'MMS', subfolderX4 = 'Excretion_coefficients', pattern = 'Dairy')
  }
  else {
    
    nutrient_coef <- get_activity_data(module = 'Nutrients', subfolder = 'Nutrient_params', subfolderX2 = 'P', subfolderX3 = 'MMS', subfolderX4 = 'Excretion_coefficients', pattern = 'Other')
    coef_id <- which(nutrient_coef[, 'Animals'] == param)
    animal_coef <- nutrient_coef[coef_id, ncol(nutrient_coef)]
    rm(list=c('nutrient_coef', 'coef_id'))
  }
  
  return(animal_coef)
}


compute_animal_nutrient_excretion <- function(nutrient, main_param, param) {
  # computes the total nutrient excretion for a given animal
  # unit: kg N yr-1 or kg P yr-1
  
  nutrient_coef <- select_animal_Nutrient_excretion_coefficient(nutrient, param)
  animal_pop <-  get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Animals', subfolderX2 = main_param, pattern = param)
  animal_excretion <- animal_pop
  
  calc_cols <- paste0('X', seq(1987,2017))
  
  ifelse(param == 'Dairy_cows',
    animal_excretion[, calc_cols] <- sapply(calc_cols, function(x) round(animal_pop[, x] * nutrient_coef[, x], 0)),
    animal_excretion[, calc_cols] <- sapply(calc_cols, function(x) round(animal_pop[, x] * nutrient_coef, 0)))
  
  return(animal_excretion)
  rm(list=c('nutrient_coef','animal_pop','calc_cols'))
}

## gross manure of a given nutrient --------------------------------------

compute_gross_manure_nutrient <- function(nutrient) {
  # computes the gross manure of a given nutrient for all animals
  
  standard_params <- get_standard_params_list()
  
  for (i in 1:nrow(standard_params)) {
    
    main_param <- standard_params[i, 'Main_animals']
    param <- standard_params[i, 'Animals']
    
    print(paste0('Exporting ====='))
    print(param)
    # compute annual nutrient excretion
    animal_excretion <- compute_animal_nutrient_excretion(nutrient, main_param, param)
    export_file(module = 'Nutrients', 
                file = animal_excretion, 
                filename = param, 
                folder = 'Output', 
                subfolder = 'Gross_manure', 
                subfolderX2 = nutrient, 
                subfolderX3 = 'Total_Nexcretion', 
                subfolderX4 = main_param)
  }
  rm(list=c('standard_params','param_col','main_param_col','main_param','param','animal_excretion'))
}

loop_gross_manure_nutrient <- function() {
  
  n <- c('P','N')
  sapply(n, function(x) compute_gross_manure_nutrient(x))
}



#### gross manure allocation (housing, grazing, yards) --------------------------------------

source('./Nutrients/Model/MMS/')



grazing_dairy_cows_condition <- function(dairy_graz_frac) {
  # assumption: no grazing in Agrarian Region Entre Douro e Minho
  
  disagg_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Spatial_disaggregation')
  
  rows_AR_1 <- which(disagg_df$agrarian_region_id == '1')
  
  calc_cols <- paste0('X', seq(1987,2017))
  disagg_df[, calc_cols] <- sapply(calc_cols, function(x) disagg_df[,x] <- NA)
  
  disagg_df[-rows_AR_1, calc_cols] <- sapply(calc_cols, function(x) disagg_df[-rows_AR_1, x] <- dairy_graz_frac[, x])
  disagg_df[rows_AR_1, calc_cols] <- sapply(calc_cols, function(x) disagg_df[rows_AR_1, x] <- 0)
  
  return(disagg_df)
}


compute_dairy_grazing <- function(dairy_man_nutrient, dairy_graz_frac) {
  # calculates grazing manure N of dairy cattle
  # unit: kg N yr-1
  
  calc_cols <- paste0('X', seq(1987,2017))
  dairy_man_nutrient[, calc_cols] <- sapply(calc_cols, function(x) dairy_man_nutrient[, x] <- dairy_man_nutrient[, x] * dairy_graz_frac[,x])
  
  return(dairy_man_nutrient)
}



compute_manure_grazing_nutrient <- function(nutrient) {
  # computes manure N grazing for the different animal classes
  # FRAC_graz comes from APA 2017 and it was linearly extrapolated based on data for 1990 and 2014

  FRAC_graz <- linearly_intrapolate_share_MMS(general_param = 'Distribution', param = 'Grazing')
  
  standard_params <- get_standard_params_list(main_param = 'Animals')
  
  for (i in 1:nrow(standard_params)) {
    
    main_param <- standard_params[i, 'Main_animals']
    param <- standard_params[i, 'Animals']
    
    # get gross man N
    man_nutrient <- compute_animal_nutrient_excretion(nutrient, main_param, param)
    grazing_N <- man_nutrient
    
    # subset historical grazing FRAC
    animal_row <- which(FRAC_graz[, 'Animals'] == param)
    animal_graz_frac <- FRAC_graz[animal_row, paste0('X', seq(1987,2017))]
    
    if (param == 'Dairy_cows') {
      
      grazing_N <- compute_dairy_grazing(man_nutrient, animal_graz_frac)
    }
    else {
      
      calc_cols <- paste0('X', seq(1987,2017))
      grazing_N[, calc_cols] <- sapply(calc_cols, function(x) grazing_N[, x] * animal_graz_frac[, x])
    }
    export_file(module = 'Nutrients', 
                file = grazing_N, 
                filename = param, 
                folder = 'Output', 
                subfolder = 'Gross_manure', 
                subfolderX2 = nutrient, 
                subfolderX3 = 'Grazing', 
                subfolderX4 = main_param)
  }
}




## CARBON IMPLEMENTATION -------------------------------------------------------------------------------------


