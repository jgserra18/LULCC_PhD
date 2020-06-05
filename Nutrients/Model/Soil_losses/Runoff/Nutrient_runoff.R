source('Main/Global_functions.R')


compute_net_grazing_soil = function(nutrient) {
  # returns the total net nutrient amounts to soil from animal grazing
  # it is assumed no P losses while for N NH3 emissions must be excluded
  # unit: kg nutrient yr1-
  
  tot_nut_grazing = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gross_manure', subfolder = nutrient, subfolderX2 = 'Total', subfolderX3 = 'Total', pattern = 'Total')
  
  if (nutrient == 'N') {
    
    tot_nh3_grazing = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gas_N_emissions', subfolder = 'NH3', subfolderX2 = 'Grazing',subfolderX3 = 'Total', subfolderX4 = 'Total', pattern = 'Total_sum')
    
    yrs = paste0('X', seq(1987,2017))
    tot_nut_grazing[, yrs] = sapply(yrs, function(x) round(tot_nut_grazing[, x] - tot_nh3_grazing[, x], 1))
  }
  
  return(tot_nut_grazing)
}



source('./Nutrients/Model/Fertilization/3_CropManureAllocation.R')
source('./Nutrients/Model/Fertilization/4_BiosolidsAllocation.R')



compute_net_org_fertilisers_soil = function(nutrient, fert_type, manure_method = 'Method 1') {
  
  # store everything
  yrs <- paste0('X', seq(1987,2017))
  store_nut <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  store_nut[, yrs] <- sapply(yrs, function(x) store_nut[,x] <- 0)
  
  # get data according to the organic fertiliser 
  
  if (fert_type == 'Manure') {
    
    standard_params = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Crops', subfolderX2 = 'Fertilisers', subfolderX3 = 'Manure_crop_distribution', pattern = 'Crop_hierarchy')
    files_path = 'Manure_application_rates'
  }
  else {
    
    standard_params = get_sludge_distribution_crops()
    files_path = 'Biosolids_application_rates'
  }
  
  # loop around all the allocated crops and store in "store_nut
  for (i in 1:nrow(standard_params)) {
    
    main_param = standard_params[i, 1]
    param = standard_params[i, 2]
    print(param)
    
    crop_app_rate = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Fertilisation',  subfolder = nutrient, subfolderX2 = files_path, subfolderX3 = manure_method, subfolderX4 = main_param, pattern = param)
    crop_area =  get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = main_param, pattern = param)
    crop_nutrient = crop_app_rate
    
    crop_nutrient[, yrs] = sapply(yrs, function(x) round( crop_app_rate[, x] * crop_area[, x], 1))
    store_nut[, yrs] = sapply(yrs, function(x) round (store_nut[,x] + crop_nutrient[, x], 1))
  }
  
  return(store_nut)
  rm(list=c('yrs','standard_params','files_path','crop_app_rate','crop_area','crop_nutrient'))
}
d = compute_net_org_fertilisers_soil('N','Manure')
sapply(yrs, function(x) sum(d[, x]) / 1000000)


