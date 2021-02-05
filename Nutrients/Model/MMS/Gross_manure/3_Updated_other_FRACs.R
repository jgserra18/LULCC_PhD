source('./Nutrients/Model/MMS/Gross_manure/Compute_Nutrient_excretion.R')
source('./Nutrients/Model/MMS/Gross_manure/2.1_Update_FRAC_grazing.R')

#' @description Adjustes bovine, poultry and pigs housing fraction based on the updatesd grazing fractions

yrs = paste0('X',seq(1987,2017))

correct_FRAC_housing = function(threshold=800, main_param, param) {
  #' @param threshold threshold of maximum allowed limite on N excreted onto pastures (kg N ha-1 yr-1) ; from identify_default_surpass_threshold()
  #' @param main_param  main animal category (Bovine, Pigs, etc)
  #' @param param animal subcategories/species (e.g., dairy cows, pregnant sows)
  #' @description corrects/adjusts the FRAC_housing for the main animal categories where FRAC grazing was updated
  #' @return returns the correct FRAC_housing for a given animal (ie, param)
  #' @usage correct_FRAC_housing(1500, 'Bovine','Dairy_cows')
  
  yrs = paste0('X',seq(1987,2017))
  
  if (main_param == 'Dairy_cows') { main_param = 'Bovine'}
  old_FRAC_housing = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Animals', subfolderX2 = 'Downscaled_distribution', subfolderX3 = 'Housing', subfolderX4 = main_param, pattern = param)
  old_FRAC_grazing = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Animals', subfolderX2 = 'Downscaled_distribution', subfolderX3 = 'Grazing', subfolderX4 = main_param, pattern = param)
  new_FRAC_grazing = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Animals', subfolderX2 = 'Grazing_algorith', subfolderX3 = paste0('Threshold_', threshold), subfolderX4 = 'Updated_FRAC_grazing', subfolderX5 = main_param, pattern = param)
  old_FRAC_housing[, yrs] = sapply(yrs, function(x) old_FRAC_housing[,x] + (old_FRAC_grazing[,x] - new_FRAC_grazing[,x]))

  return(old_FRAC_housing)
  rm(list=c('old_FRAC_grazing','new_FRAC_grazing'))
}


loop_adjusted_FRAC_housing = function(threshold=800) {
  
  standard_params <- get_standard_params_list(main_param = 'Animals')
  standard_params = subset(standard_params, Main_animals != 'Rabbits')
  
  for (i in 1:nrow(standard_params)) {
    
    param = standard_params[i,1]
    main_param = standard_params[i,2]
    new_FRAC_housing = correct_FRAC_housing(threshold, main_param, param)
    new_FRAC_housing = data_cleaning(new_FRAC_housing)
    
    export_file(module = 'Nutrients', folder = 'Activity_data', subfolder = 'General_params', subfolderX2 = 'Animals', subfolderX3 = 'Grazing_algorithm', subfolderX4 = paste0('Threshold_',threshold), subfolderX5 = 'Updated_FRAC_housing', subfolderX6 = ifelse(param=='Dairy_cows','Bovine',main_param), file = new_FRAC_housing, filename = param)
  }
}
