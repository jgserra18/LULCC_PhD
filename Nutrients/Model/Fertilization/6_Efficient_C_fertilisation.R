source('./Main/Global_functions.R')


# get crop manure/sludge N rate (kg N ha_crop-1 yr-1)
# get the respective CN ratio and the HU coefficient
# multiply by area
# export in kg C yr-1

get_CN_ratio_org_fert = function(org_fert, ...) {
  # get CN ratio for "Biosolids" (Le Noe et al 2016)
  # or for "Manure"; args must follow the order: main_param, param, manure_type
  # where manure_type is either "Solid", "Slurry" or "Fresh_grass" for direct droppings
  
  if (org_fert == 'Biosolids') {
    CN = 10
  }
  else if (org_fert  == 'Manure') {
    
    args = list(...)
    main_param = args[[1]]
    param = args[[2]]
    manure_type = args[[3]]
    
    CN = get_activity_data(module = 'Nutrients', folder = 'Nutrient_params', subfolder = 'C', subfolderX2 = 'MMS', subfolderX3 = 'Excretion_coefficients', pattern = 'CN')
    CN = subset(CN, Manure_type == manure_type & Main_animals == main_param & Animals == param)[, 'C_N']
  }
  
  return(CN)
}

get_HU_coefficient = function(org_fert, ...) {
  # gets the humificiation C coefficients from Le Noe et al 2016
  # biosolid assumed from slurry
  
  if (org_fert == 'Biosolids') {
    HU_coef = 0.26
  }
  else if (org_fert  == 'Manure') {
    
    args = list(...)
    manure_type = args[[1]]
    
    HU_coeff = get_activity_data(module = 'Nutrients', folder = 'Nutrient_params', subfolder = 'C', subfolderX2 = 'MMS', subfolderX3 = 'Excretion_coefficients', pattern = 'HU')
    HU_coeff = subset(HU_coeff, Manure_type == manure_type)[, 'Eff_frac']
  }
  
  return(HU_coeff)
}



compute_Cefficient_org_fertilisation = function(org_fert, ...) {
  
  
}



crop_man_solid = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Fertilisation', subfolder = nutrient, subfolderX2 = 'Manure_application_rates', subfolderX3 = 'Method 1', subfolderX4 = folder_div, subfolderX5 = 'Solid', subfolderX6 = main_param, pattern = param)

