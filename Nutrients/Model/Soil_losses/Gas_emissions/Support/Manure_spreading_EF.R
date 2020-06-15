source('./Main/Global_functions.R')
source('./Nutrients/Model/MMS/Spreading/Spreading.R')
source('./Nutrients/Model/Fertilization/3_1_CropManureAllocation.R')


#* CALCULATES A WEIGHTED AVERAGE FOR MANURE SPREADING EMISSION FACTORS FOR RUMINANT AND NON-RUMINANT MANURE
#* THIS IS BECAUSE THE MANURE ALLOCATION MECHANISM DOES NOT DISTINGUISH BETWEEN DIFFERENT ANIMAL CLASSES
#* DOING THIS, WOULD GREATLY INCREASE THE COMPLEXITY AND UNCERTAINTY 
#* THEREFORE, THE WEIGHTED AMMONIA EMISSION FACTORS ARE CALCULATED BASED ON THE FRACTION OF GROSS MANURE FOR DIFFERENT ANIMAL CLASSES
#* WITHIN THE MAIN DIVISION: RUMINANTS AND NON-RUMINANTS


# CONVERT EMEP 2019 TAN FLOW-BASED EMISSION FACTOR TO N ------------------------------------------------------------------------------------- 

compute_spreading_N_EF = function(nutrient = 'N', main_param, param, manure_type)  {
  # calculates the emission factor for NH3 for gross manure spreading on a different unit
  # unit: kg N-NH3 kg N applied-1
  
  gross_man = get_activity_data(module = 'Nutrients', mainfolder =  'Output', folder = 'Gross_manure', subfolder = nutrient, subfolderX2 = 'Gross_spreading', subfolderX3 = manure_type, subfolderX4 = main_param, pattern = param)
  net_man = get_activity_data(module = 'Nutrients', mainfolder =  'Output', folder = 'Gross_manure', subfolder = nutrient, subfolderX2 = 'Net_spreading', subfolderX3 = manure_type, subfolderX4 = main_param, pattern = param)
  
  EF_spread_N = net_man
  
  yrs = paste0('X', seq(1987,2017))
  EF_spread_N[, yrs] = sapply(yrs, function(x) round((gross_man[, x] - net_man[, x])/gross_man[, x], 2))
  EF_spread_N = data_cleaning(EF_spread_N)
  
  return(EF_spread_N)
  rm(list=c('gross_man','net_man'))
}




# CALCULATE WEIGHTED VALUE FOR THE EF FOR EACH ANIMAL SUBCLASS  ------------------------------------------------------------------------------------- 


compute_gross_man_spreading_animal_division = function(nutrient = 'N', animal_division, manure_type) {
  # calculates the sum of gross manure spreading for either ruminants or non-ruminants
  # unit: kg N yr-1
  
  main_param = get_animal_class_distribution(animal_division)
  main_param = append(main_param, 'Pigs')
  
  # prepare dataframe to store the sum of gross man spreading for a given animal division
  yrs <- paste0('X', seq(1987,2017))
  gross_man_div <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  gross_man_div[, yrs] <- sapply(yrs, function(x) gross_man_div[,x] <- 0)
  
  for (i in main_param) {
    
    gross_man_main_param = get_activity_data(module = 'Nutrients', mainfolder =  'Output', folder = 'Gross_manure', subfolder = nutrient, subfolderX2 = 'Gross_spreading', subfolderX3 = manure_type, subfolderX4 = 'Total', pattern = i)
    gross_man_div[, yrs] = sapply(yrs, function(x) round(gross_man_div[, x] + gross_man_main_param[, x], 3))
  }
  
  return(gross_man_div)
  rm(list=c('gross_man_main_param'))
}



compute_gross_man_spreading_FRAC_animal = function(nutrient = 'N', main_param, param, manure_type) {
  # computes the fraction of the gross manure spreading for a given animal subclass relative to the total gross manure of its animal division (i.e., ruminant or not)
  # unit: %
  
  if (main_param %in% c('Bovine', 'Sheep', 'Goats')) { animal_div = 'ruminants' } else { animal_div = 'non_ruminants'}
  
  
  gross_man_param = get_activity_data(module = 'Nutrients', mainfolder =  'Output', folder = 'Gross_manure', subfolder = nutrient, subfolderX2 = 'Gross_spreading', subfolderX3 = manure_type, subfolderX4 = main_param, pattern = param)
  tot_gross_man_animalDiv = compute_gross_man_spreading_animal_division(nutrient, animal_division = animal_div, manure_type = manure_type)
  
  FRAC_man_param_animal_div = tot_gross_man_animalDiv
  
  yrs <- paste0('X', seq(1987,2017))
  FRAC_man_param_animal_div[, yrs] <- sapply(yrs, function(x) round ( gross_man_param[,x] / tot_gross_man_animalDiv[,x], 3))
  FRAC_man_param_animal_div = data_cleaning(FRAC_man_param_animal_div)
  
  return(FRAC_man_param_animal_div)
  rm(list=c('gross_man_param', 'tot_gross_man_animalDiv'))
}



compute_weighted_EF_animal_param = function(nutrient = 'N', main_param, param, manure_type) {
  # computes the weighted emission factor for NH3 emissions following spreading for a given animal within the animal division
  # unit: %
  
  FRAC_gross_man_animal = compute_gross_man_spreading_FRAC_animal(nutrient = 'N', main_param, param, manure_type)
  man_spread_animal_EF = compute_spreading_N_EF(nutrient = 'N', main_param, param, manure_type)
  
  wght_EF = man_spread_animal_EF
  yrs <- paste0('X', seq(1987,2017))
  wght_EF[, yrs] <- sapply(yrs, function(x) round ( man_spread_animal_EF[,x] * FRAC_gross_man_animal[,x], 3))
  
  return(wght_EF)
  rm(list=c('FRAC_gross_man_animal', 'man_spread_animal_EF'))
}



# CALCULATE WEIGHTED AVERAGE EF_NH3 FOR RUMINANTS AND NON-RUMINANTS  ------------------------------------------------------------------------------------- 


compute_weighted_avg_EF_animal_div = function(nutrient = 'N', animal_division, manure_type) {
  # computes the weighted average EF_NH3 for manure spreading for ruminants and non_ruminants
  # unit: kg N-NH3 kg N applied-1 yr-1
  
  standard_params <- get_standard_params_list(main_param = 'Animals')
  
  if (animal_division == 'ruminants') { 
    standard_params = subset(standard_params, Main_animals == 'Bovine' | Main_animals == 'Goats' | Main_animals == 'Sheep') 
  } 
  else { 
    standard_params = subset(standard_params, Main_animals != 'Bovine' & Main_animals != 'Goats' & Main_animals != 'Sheep') 
  }
  
  # prepare dataframe to store the sum of the weighted avg approach of gross man spread EF_NH3 for ruminants and non_ruminants
  yrs <- paste0('X', seq(1987,2017))
  store_ef <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  store_ef[, yrs] <- sapply(yrs, function(x) store_ef[,x] <- 0)
  
  
  for (i in 1:nrow(standard_params)) {
    
    main_param = standard_params[i, 'Main_animals']
    param = standard_params[i, 'Animals']
    
    animal_wghtd_EF = compute_weighted_EF_animal_param(nutrient, main_param, param, manure_type)
    store_ef[, yrs] = sapply(yrs, function(x) round( store_ef[, yrs] + animal_wghtd_EF[, x] , 3))
  }
  
  return(store_ef)
}


