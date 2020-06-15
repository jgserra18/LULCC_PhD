


## GRAZING NH3 EMISSIONS  ------------------------------------------------------------------------------------

source('./Nutrients/Model/MMS/Grazing/Grazing_Nemissions.R')

loop_grazing_NH3_emissions()



## MANURE CROP APPLICATION NH3 EMISSIONS ---------------------------------------------------------------------

source('./Nutrients/Model/Soil_losses/Gas_emissions/Support/Manure_spreading_EF.R')


get_crop_animal_div = function(main_param) {
  
  if (main_param == 'Forage' | main_param == 'Pastures') {
    
    animal_div = 'ruminants'
  }
  else {
    animal_div = 'non_ruminants'
  }
  return(animal_div)
}



compute_updated_crop_manure_spreading_NH3_emissions = function(main_param, param, manure_type, manure_surplus_fills_nutDemand = F, manure_method = 'Method 1', nutrient = 'N') {
  # computes the N-NH3 emissions following crop manure spreading
  # uses source('./Nutrients/Model/Soil_losses/Gas_emissions/Support/Manure_spreading_EF.R')
  # unit: kg N-NH3 yr-1
  
  if (manure_surplus_fills_nutDemand == TRUE) { folder_div = 'With_ManSurplus'} else { folder_div = 'Without_ManSurplus'}
  
  crop_man_rate = get_activity_data(module = 'Nutrients', 
                                mainfolder =  'Output', 
                                folder = 'Fertilisation', 
                                subfolder = nutrient, 
                                subfolderX2 = 'Manure_application_rates',
                                subfolderX3 = manure_method, 
                                subfolderX4 = folder_div,
                                subfolderX5 = manure_type, 
                                subfolderX6 = main_param, 
                                pattern = param)
  crop_area = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = main_param, pattern = param)
  
  animal_div = get_crop_animal_div(main_param)
  EF_man_spread = compute_weighted_avg_EF_animal_div(nutrient = 'N', animal_div, manure_type)
  
  crop_spread_NH3 = EF_man_spread
  
  # calculation
  yrs <- paste0('X', seq(1987,2017))
  crop_spread_NH3[, yrs] <- sapply(yrs, function(x) round(crop_area[,x] * crop_man_rate[, x] * EF_man_spread[, x], 1))
  
  return(crop_spread_NH3)
  rm(list=c('crop_man_rate','crop_area','EF_man_spread'))
}



loop_update_crop_manure_spreading_NH3_emission = function(manure_type, manure_surplus_fills_nutDemand = F, manure_method = 'Method 1', nutrient = 'N') {
  #
  if (manure_surplus_fills_nutDemand == TRUE) { folder_div = 'With_ManSurplus'} else { folder_div = 'Without_ManSurplus'}
  standard_params <- get_standard_params_list(main_param = 'Crops')
  
  for (i in 1:nrow(standard_params)) {
    
    main_param = standard_params[i, 'Main_crop']
    param = standard_params[i, 'Crop']
    
    
    if (main_param == 'Pastures' | main_param == 'Forage' | param == 'Tomato' | main_param == 'Horticulture') {
      next
    }
    else {
      
      crop_man_app_NH3 = compute_updated_crop_manure_spreading_NH3_emissions(main_param, param, manure_type)
      export_file(module = 'Nutrients', 
                  file = crop_man_app_NH3, 
                  filename = param, 
                  folder = 'Gas_N_emissions', 
                  subfolder = 'NH3', 
                  subfolderX2 = 'Manure_application',
                  subfolderX3 = manure_method, 
                  subfolderX4 = folder_div,
                  subfolderX5 = manure_type, 
                  subfolderX6 = main_param)
    }
  }
}

loop_update_crop_manure_spreading_NH3_emission_ManType = function(manure_surplus_fills_nutDemand = F, manure_method = 'Method 1', nutrient = 'N') {
  
  man_type = c('Solid','Slurry')
  sapply(man_type, function(x) loop_update_crop_manure_spreading_NH3_emission(x, manure_surplus_fills_nutDemand, manure_method))
}



compute_total_manure_spreading_NH3 = function(manure_surplus_fills_nutDemand = F, manure_method = 'Method 1', nutrient = 'N') {
  
  if (manure_surplus_fills_nutDemand == TRUE) { folder_div = 'With_ManSurplus'} else { folder_div = 'Without_ManSurplus'}
  standard_params <- get_standard_params_list(main_param = 'Crops')
  
  for (i in 1:nrow(standard_params)) {
    
    main_param = standard_params[i, 'Main_crop']
    param = standard_params[i, 'Crop']
    
    
    if (main_param == 'Pastures' | main_param == 'Forage' | param == 'Tomato' | main_param == 'Horticulture') {
      next
    }
    else {
      
      crop_man_app_slurry_NH3 = get_activity_data(module = 'Nutrients', 
                                                  mainfolder =  'Output', 
                                                  folder = 'Gas_N_emissions', 
                                                  subfolder = 'NH3', 
                                                  subfolderX2 = 'Manure_application',
                                                  subfolderX3 = manure_method, 
                                                  subfolderX4 = folder_div,
                                                  subfolderX5 = 'Slurry', 
                                                  subfolderX6 = main_param, 
                                                  pattern = param)
      crop_man_app_solid_NH3 = get_activity_data(module = 'Nutrients', 
                                                 mainfolder =  'Output', 
                                                 folder = 'Gas_N_emissions', 
                                                 subfolder = 'NH3', 
                                                 subfolderX2 = 'Manure_application',
                                                 subfolderX3 = manure_method, 
                                                 subfolderX4 = folder_div,
                                                 subfolderX5 = 'Solid', 
                                                 subfolderX6 = main_param, 
                                                 pattern = param)
      
      # total manure
      # calculation
      yrs <- paste0('X', seq(1987,2017))
      crop_man_app_solid_NH3[, yrs] <- sapply(yrs, function(x) round(crop_man_app_solid_NH3[,x] + crop_man_app_slurry_NH3[, x], 1))
      
      export_file(module = 'Nutrients', 
                  file = crop_man_app_solid_NH3, 
                  filename = param, 
                  folder = 'Gas_N_emissions', 
                  subfolder = 'NH3', 
                  subfolderX2 = 'Manure_application',
                  subfolderX3 = manure_method, 
                  subfolderX4 = folder_div,
                  subfolderX5 = 'Total', 
                  subfolderX6 = main_param)
    }
  }
  rm(list=c('crop_man_app_solid_NH3','crop_man_app_slurry_NH3'))
}



## CROP RESIDUES BURNT ON-SITE NH3 EMISSIONS -----------------------------------------------------------------

source('./Nutrients/Model/Crop_production/Compute_crop_residues.R')

set_NH3_EF_CropResiduesBurnt = function() {
  # rom EMEP 2019
  # unit: kg N-NH3 kg DM-1
  
  EF_NH3 = 0.0024 * 17/17
  
  return(EF_NH3)
}



compute_crop_residue_burnt_NH3 = function(main_param, param, nutrient = 'N') {
  # computes crop residue burnt NH3 emissions according to EMEP 2019
  # unit: kg N-NH3 yr-1
  
  ef = set_NH3_EF_CropResiduesBurnt()
  res_burnt_N = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Crop_residues', subfolder = 'Burnt', subfolderX2 = 'N', subfolderX3 = main_param, pattern = param)
  
  if (main_param == 'Cereals') {
    FRAC_res_N = get_residues_nutrient_content('N', param, 'AG')
  }
  else {
    PC_Nres = get_activity_data(module = 'Nutrients', folder = 'Nutrient_params', subfolder = 'N', subfolderX2 = 'Crops', subfolderX3 = 'Residues',pattern = 'PermaCrops_Ncontent')
    FRAC_res_N = find_crop_variable(df = PC_Nres, param_col = 'crop', param = param, var = 'N_res')
  }
  
  # compute biomass burnt
  # unit: kg DM 
  yrs = paste0('X',seq(1987,2017))
  res_burnt_DM = res_burnt_N
  res_burnt_DM[, yrs] = sapply(yrs, function(x) round(res_burnt_N[, x] / FRAC_res_N, 1))
  
  # compute NH3 emissions
  res_burnt_NH3 = res_burnt_DM
  res_burnt_NH3[, yrs] = sapply(yrs, function(x) round(res_burnt_DM[, x] * ef, 1))
  
  return(res_burnt_NH3)
  rm(list=c('ef','res_burnt_N','FRAC_res_N','yrs','res_burnt_DM'))
}



compute_all_crop_residues_burnt_NH3 <- function() {
  
  main_crops <- c('Cereals','Vineyard', 'Olive_grove','Citrus','Dried_fruits','Fresh_fruits')
  standard_params <- get_standard_params_list(main_param = 'Crops')
  
  
  for (i in main_crops) {
    
    crops <- standard_params[which(standard_params$Main_crop==i), 'Crop']
    
    for (j in crops) {
      
      crop_res_burnt_NH3 = compute_crop_residue_burnt_NH3(main_param = i, param = j)
      export_file(module = 'Nutrients', 
                  file = crop_res_burnt_NH3, 
                  filename = j, 
                  folder = 'Gas_N_emissions', 
                  subfolder = 'NH3', 
                  subfolderX2 = 'Crop_residues_burnt',
                  subfolderX3 = i)
    }
  }
}


## BIOSOLID FIELD APPLICATION NH3 EMISSIONS -----------------------------------------------------------------------

source('./Nutrients/Model/Fertilization/2_Biosolids.R')
source('./Nutrients/Model/Fertilization/4_BiosolidsAllocation.R')

set_NH3_biosolid = function() {
  # EMEP 2019
  # unit kg N-Nh3 kg N applied -1
  
  EF_NH3_biosolid = 0.13 * 17/14 
  return(EF_NH3_biosolid)
}


compute_crop_biosolid_app_NH3 = function(main_param, param, manure_surplus_fills_nutDemand = F, manure_method = 'Method 1', nutrient = 'N') {
  # unit: kg N-NH3 yr-1
  
  if (manure_surplus_fills_nutDemand == TRUE) { folder_div = 'With_ManSurplus'} else { folder_div = 'Without_ManSurplus'}
  
  ef = set_NH3_biosolid()
  crop_biosolid_app = get_activity_data(module = 'Nutrients', 
                                        mainfolder = 'Output', 
                                        folder = 'Fertilisation', 
                                        subfolder = 'N', 
                                        subfolderX2 = 'Biosolids_application', 
                                        subfolderX3 = manure_method, 
                                        subfolderX4 = folder_div,
                                        subfolderX5 = main_param, 
                                        pattern = param)
  
  yrs = paste0('X',seq(1987,2017))
  crop_biosolid_app_NH3 = crop_biosolid_app
  crop_biosolid_app_NH3[, yrs] = sapply(yrs, function(x) round(crop_biosolid_app[, x] * ef, 1))
  
  return(crop_biosolid_app_NH3)
  rm(list=c('ef','crop_biosolid_app'))
}


loop_crop_biosolid_app_NH3 = function(manure_surplus_fills_nutDemand = F, manure_method = 'Method 1', nutrient = 'N') {
  
  if (manure_surplus_fills_nutDemand == TRUE) { folder_div = 'With_ManSurplus'} else { folder_div = 'Without_ManSurplus'}
  
  biosolid_crops = get_sludge_distribution_crops()
  
  for (i in 1:nrow(biosolid_crops)) {
    
    main_param = biosolid_crops[i, 'Main_crop']
    param = biosolid_crops[i, 'Crop']
    
    crop_biosolid_app_NH3 = compute_crop_biosolid_app_NH3(main_param, param, manure_method)
    export_file(module = 'Nutrients', 
                file = crop_biosolid_app_NH3, 
                filename = param, 
                folder = 'Gas_N_emissions', 
                subfolder = 'NH3', 
                subfolderX2 = 'Biosolid_application',
                subfolderX3 = manure_method, 
                subfolderX4 = folder_div,
                subfolderX5 = main_param)
  }
}

compute_total_biosolid_app_NH3 = function(manure_surplus_fills_nutDemand = F, manure_method = 'Method 1', nutrient = 'N') {
  
  if (manure_surplus_fills_nutDemand == TRUE) { folder_div = 'With_ManSurplus'} else { folder_div = 'Without_ManSurplus'}
  
  biosolid_crops = get_sludge_distribution_crops()
  
  yrs <- paste0('X', seq(1987,2017))
  store_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  store_df[, yrs] <- sapply(yrs, function(x) store_df[,x] <- 0)
  
  for (i in 1:nrow(biosolid_crops)) {
    
    main_param = biosolid_crops[i, 'Main_crop']
    param = biosolid_crops[i, 'Crop']
    
    crop_biosolid_app_NH3 = compute_crop_biosolid_app_NH3(main_param, param, manure_method)
    store_df[, yrs] = sapply(yrs, function(x) round(store_df[,x] + crop_biosolid_app_NH3[,x], 1))
  }
  export_file(module = 'Nutrients', 
              file = store_df, 
              filename = 'Total', 
              folder = 'Gas_N_emissions', 
              subfolder = 'NH3', 
              subfolderX2 = 'Biosolid_application',
              subfolderX3 = manure_method, 
              subfolderX4 = folder_div,
              subfolderX5 = 'Total')
}
