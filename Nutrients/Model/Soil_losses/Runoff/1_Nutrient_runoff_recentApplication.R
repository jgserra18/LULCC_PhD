source('Main/Global_functions.R')

# disaggregate pastures --------------------------------------------------------

compute_pastures_FRAC = function(main_param = 'Pastures', param) {
  
  temp_grass = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = main_param, pattern = 'Intensive_pasture')
  perm_grass = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = main_param, pattern = 'Extensive_pasture')
  
  yrs <- paste0('X', seq(1987,2017))
  grass = perm_grass
  FRAC_grass = perm_grass
  grass[, yrs] <- sapply(yrs, function(x) round(perm_grass[,x] + temp_grass[,x], 1))
  
  if (param == 'Intensive_pasture') {
    FRAC_grass[, yrs] <- sapply(yrs, function(x) round(temp_grass[,x]/grass[,x], 3))
  }
  else {
    FRAC_grass[, yrs] <- sapply(yrs, function(x) round(perm_grass[,x]/grass[,x], 3))
  }
  
  return(FRAC_grass) 
}

compute_FRAC_pastures_nutrient_flows = function(nutrient_flow_df, pasture_type) {
  # disaggregates nutrient flows per pasture type
  
  
  yrs <- paste0('X', seq(1987,2017))
  FRAC_grass = compute_pastures_FRAC(param = pasture_type)
  nutrient_flow_df[, yrs] = sapply(yrs, function(x) round(FRAC_grass[,x] * nutrient_flow_df[,x], 2))
  
  return(nutrient_flow_df)
}


# GRAZING NET N RETURNED TO SOIL ----------------------------------------------------------------------------------------------- 

compute_net_grazing_soil = function(pasture_type, nutrient) {
  # returns the total net nutrient amounts to soil from animal grazing per pasture type (Intensive_pasture or Extensive_pasture)
  # it is assumed no P losses while for N NH3 emissions must be excluded
  # unit: kg nutrient yr1-
  
  tot_nut_grazing = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gross_manure', subfolder = nutrient, subfolderX2 = 'Total', subfolderX3 = 'Total', pattern = 'Total')
  tot_nut_grazing = compute_FRAC_pastures_nutrient_flows(tot_nut_grazing, pasture_type)
    
  if (nutrient == 'N') {
    
    tot_nh3_grazing = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gas_N_emissions', subfolder = 'NH3', subfolderX2 = 'Grazing',subfolderX3 = 'Total', subfolderX4 = 'Total', pattern = 'Total_sum')
    tot_nh3_grazing = compute_FRAC_pastures_nutrient_flows(tot_nh3_grazing, pasture_type)
    
    yrs = paste0('X', seq(1987,2017))
    tot_nut_grazing[, yrs] = sapply(yrs, function(x) round(tot_nut_grazing[, x] - tot_nh3_grazing[, x], 1))
  }
  
  return(tot_nut_grazing)
}




# ORGANIC and INORGNAIC FERTILISERES NET NUTRIENT RETURNED TO SOIL ---------------------------------------------------------------------------

source('./Nutrients/Model/Fertilization/3_1_CropManureAllocation.R')
source('./Nutrients/Model/Fertilization/4_BiosolidsAllocation.R')
source('./Nutrients/Model/Soil_losses/Gas_emissions/NH3_other_emissions.R')


compute_crop_net_N_fertilisers = function(main_param, param, fert_type, nutrient = 'N', manure_surplus_fills_nutDemand = F, manure_method = 'Method 1') {
  # cmputes the net N returned to the soil following the application of  fertilisers and NH3 emissions
  # unit: kg N yr-1
  
  if (manure_surplus_fills_nutDemand == TRUE) { folder_div = 'With_ManSurplus'} else { folder_div = 'Without_ManSurplus'}
  
  
  if (fert_type == 'Manure') {
    
    app_rate = 'Manure_application_rates'
    app_nh3 = 'Manure_application'
    crop_app_rate = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Fertilisation',  subfolder = nutrient, subfolderX2 = app_rate, subfolderX3 = manure_method, subfolderX4 = folder_div, subfolderX5 = 'Total', subfolderX6 = main_param, pattern = param)
    crop_app_NH3 =   get_activity_data(module = 'Nutrients', 
                                       mainfolder =  'Output', 
                                       folder = 'Gas_N_emissions', 
                                       subfolder = 'NH3', 
                                       subfolderX2 = app_nh3,
                                       subfolderX3 = manure_method, 
                                       subfolderX4 = folder_div,
                                       subfolderX5 = 'Total', 
                                       subfolderX6 = main_param, 
                                       pattern = param)
  }
  else if (fert_type == 'Inorganic') {
    
    app_rate = 'Fertiliser_application_rates'
    app_nh3 = 'Inorganic_fertiliser'
    crop_app_rate = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Fertilisation',  subfolder = nutrient, subfolderX2 = app_rate, subfolderX3 = manure_method, subfolderX4 = folder_div, subfolderX5 = main_param, pattern = param)
    crop_app_NH3 =   get_activity_data(module = 'Nutrients', 
                                       mainfolder =  'Output', 
                                       folder = 'Gas_N_emissions', 
                                       subfolder = 'NH3', 
                                       subfolderX2 = app_nh3,
                                       subfolderX3 = manure_method, 
                                       subfolderX4 = folder_div,
                                       subfolderX5 = 'Total',
                                       subfolderX6 = main_param, 
                                       pattern = param)
  }
  else {
    
    app_rate = 'Biosolids_application_rates'
    app_nh3 = 'Biosolid_application'
    crop_app_rate = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Fertilisation',  subfolder = nutrient, subfolderX2 = app_rate, subfolderX3 = manure_method, subfolderX4 = folder_div, subfolderX5 = main_param, pattern = param)
    crop_app_NH3 =   get_activity_data(module = 'Nutrients', 
                                       mainfolder =  'Output', 
                                       folder = 'Gas_N_emissions', 
                                       subfolder = 'NH3', 
                                       subfolderX2 = app_nh3,
                                       subfolderX3 = manure_method, 
                                       subfolderX4 = folder_div,
                                       subfolderX5 = main_param, 
                                       pattern = param)
  }

  crop_area =  get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = main_param, pattern = param)

  
  yrs <- paste0('X', seq(1987,2017))
  crop_nutrient = crop_app_rate
  crop_nutrient[, yrs] = sapply(yrs, function(x) round( crop_app_rate[, x] * crop_area[, x] - crop_app_NH3[,x], 1))
  
  return(crop_nutrient)
  rm(list=c('crop_app_rate','crop_area','crop_app_NH3'))
}



compute_total_net_fert_nutrient = function(nutrient, fert_type, manure_surplus_fills_nutDemand = F, manure_method = 'Method 1') {
  

  # store everything
  yrs <- paste0('X', seq(1987,2017))
  store_nut <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  store_nut[, yrs] <- sapply(yrs, function(x) store_nut[,x] <- 0)
  
  # get data according to the organic fertiliser 
  
  if (fert_type == 'Manure' | fert_type == 'Inorganic') {
    
    standard_params = get_standard_params_list(main_param = 'Crops')
  }
  else {
    
    standard_params = get_sludge_distribution_crops()
  }
  
  # loop around all the allocated crops and store in "store_nut
  for (i in 1:nrow(standard_params)) {
    
    main_param = standard_params[i, 1]
    param = standard_params[i, 2]
    
    if (fert_type == 'Manure' & (param == 'Extensive_pasture' | param == 'Tomato' | main_param == 'Horticulture')) {
      next
    }
    else if (fert_type == 'Inorganic' & param == 'Extensive_pasture') {
      next 
    }
    else {
  
      crop_net_N = compute_crop_net_N_fertilisers(main_param, param, fert_type, nutrient, manure_surplus_fills_nutDemand, manure_method)
      store_nut[, yrs] = sapply(yrs, function(x) round (store_nut[,x] + crop_net_N[, x], 1))
    }
  }
  
  return(store_nut)
  rm(list=c('yrs','standard_params','files_path','crop_net_N'))
}


# MASK RUNOFF ACCORDING TO DIFFERENT AGRICULTURAL OCUPATION  --------------------------------------------------------------------------------------------

source('./Nutrients/Model/GIS_computations/LULCC_agriculture/Build_CroplandGrassland.R')


# grazing runoff fraction -------------------------------------------------------------------------------------------------------------------------------


mask_runoff_grazing = function(year, spatial_res = '500') {
  #* masks an annual MITERRA runoff fracction using possible land uses where grazing can occur
  #* unit: %input
  
  FRAC_rf = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'MITERRA_fractions', subfolder = 'FRAC_runoff', subfolderX2 = 'Runoff_fraction', pattern = year)
  
  LULC_yr = create_mainland_annual_NUTS2_raster_mosaic(spatial_res, year)
  LULC_yr = resample_to_CLC(module = 'LULCC', raster_file = LULC_yr, mask_CLC = F, spatial_res = 'Native', ngb = TRUE)

  grazing_lu = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'LULCC', subfolderX2 = 'Runoff', subfolderX3 = 'LU_grazing', pattern = 'CLC_grazing')
  grazing_LU_yr = reclassify(LULC_yr, as.matrix(grazing_lu))
  
  FRAC_rf = FRAC_rf * grazing_LU_yr
  FRAC_rf[FRAC_rf==0] = NA
  
  return(FRAC_rf)
  rm(list=c('LULC_yr','grazing_lu', 'grazing_LU_yr'))
}


# nutrient field application runoff fraction -------------------------------------------------------------------------------------------------------------------------------


identify_main_crops_LU = function(main_param, param) {
  # identifies the main params/param to allocate/mask the runoff fraction based on the CLC_rules defined (see below)
  
  if (main_param == 'Cereals' | main_param == 'Pulses' | main_param == 'Forage' | main_param == 'Horticulture' |  main_param == 'Industry_crops' | main_param == 'Potato') {
    
    crop_LU = 'AnnualCrops'
  }
  else if (param == 'Intensive_pasture') {
    
    crop_LU = 'IntensivePasture'
  }
  else if (main_param == 'Fresh_fruits' | main_param == 'Citrus' | main_param == 'Dried_nuts') {
    
    crop_LU = 'FruitTrees'
  }
  else if (main_param == 'Vineyard') {
    
    crop_LU = 'Vineyards'
  }
  else if (main_param == 'Olive_grove') {
    
    crop_LU = 'OliveGrove'
  }
  return(crop_LU)
}



mask_runoff_field_application_LU = function(crop_LU, year, spatial_res = '500') {
  # masks the MITERRA runoff fraction according to the land uses allocated to different crops
  # unit: %input
  
  FRAC_rf = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'MITERRA_fractions', subfolder = 'FRAC_runoff', subfolderX2 = 'Runoff_fraction', pattern = year)

  alloc_name = paste0('LU_', crop_LU)
  LU_allocation = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'LULCC', subfolderX2 = 'Runoff', subfolderX3 = 'LU_allocation', pattern = alloc_name)

  LULC_yr = create_mainland_annual_NUTS2_raster_mosaic(spatial_res, year)
  LULC_yr = resample_to_CLC(module = 'LULCC', raster_file = LULC_yr, mask_CLC = F, spatial_res = 'Native', ngb = TRUE)
  LULC_yr = reclassify(LULC_yr, as.matrix(LU_allocation))
  
  FRAC_rf = FRAC_rf * LULC_yr
  FRAC_rf[FRAC_rf==0] = NA
  
  return(FRAC_rf)
  rm(list=c('crop_LU','LU_allocation','LULC_yr'))
}


compute_avg_muni_runoff_field_application_LU = function(crop_LU, spatial_res = '500') {
  # computes the avg runoff fraction for a given land use crop allocation
  # unit: %input
  
  store_muni = get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  muni = get_activity_data(module = 'LULCC', folder = 'Admin', pattern = 'Municipality')
  yrs = paste0(seq(1987,2017))

  store <- foreach(i=1:length(yrs),
                   .export = c('get_dir_files', 'resample_to_CLC',
                               'get_mainfolder_sub', 'identify_read_fileclass', 'create_mainland_annual_NUTS2_raster_mosaic',
                                'spatial_res', 'mask_runoff_field_application_LU', 'get_activity_data', 'mask_runoff_grazing'),
                   .combine = 'cbind',
                   .packages = c('exactextractr', 'raster', 'sf')) %dopar% {
                     
                     if (crop_LU == 'Grazing') {
                       FRAC_rf = mask_runoff_grazing(yrs[i])
                     }
                     else {
                       
                       FRAC_rf = mask_runoff_field_application_LU(crop_LU, yrs[i])
                     }
                     
                     d = exactextractr::exact_extract(FRAC_rf, muni, 'mean', include_cell = T)
                     data.frame(A = d)
                   }
  names(store) = paste0('X', yrs)
  store_muni[, paste0('X', yrs)] = sapply(paste0('X', yrs), function(x) round(store[, x], 3))
  store_muni = data_cleaning(store_muni)
  
  return(store_muni)
  rm(list=c('muni','store'))
  doParallel::stopImplicitCluster(cl)
}


loop_avg_muni_runoff_field_application_LU = function(spatial_res = '500') {
  # calculate
  
  require(doParallel)
  cl <- makeCluster(3)
  registerDoParallel(cl)  
  
  crop_LU = c('AnnualCrops','FruitTrees','OliveGrove','Vineyards','IntensivePasture', 'Grazing')

  for (i in crop_LU) {
    print(i)
    crop_LU_FRAC_rf = compute_avg_muni_runoff_field_application_LU(i)
    export_file(module = 'Nutrients', 
                file = crop_LU_FRAC_rf, 
                filename = i, 
                folder = 'Runoff', 
                subfolder = 'LU_FRAC_runoff', 
                subfolderX2 = 'Dataframe',
                subfolderX3 = 'Recent_Application')
  }
  .rs.restartR()
  rm(list='crop_LU_FRAC_rf')
}





# CALCULATE RECENT MEMORY NUTRIENT LOSSES FROM RUNOFF  --------------------------------------------------------------------------------------------


compute_recentMemory_runoff_nutrient_grazing = function(pasture_type, nutrient, manure_surplus_fills_nutDemand = F, manure_method = 'Method 1') {
  # recent memory runoff losses from grazing 
  # unit: kg nutrient yr-1
  

  net_nutrient_soil = compute_net_grazing_soil(pasture_type, nutrient)
  
  FRAC_rf_pasture_type = ifelse(pasture_type=='Extensive_pasture','Grazing','IntensivePasture')
  FRAC_rf_grazing = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Runoff', subfolder = 'LU_FRAC_runoff', subfolderX2 = 'Dataframe', subfolderX3 = 'Recent_Application', pattern = FRAC_rf_pasture_type)

  runoff_nutrient_grazing = FRAC_rf_grazing
  
  yrs = paste0('X', seq(1987,2017))
  FRAC_rf_grazing[, yrs] = sapply(yrs, function(x) round(net_nutrient_soil[,x] * FRAC_rf_grazing[,x], 1))
  
  return(FRAC_rf_grazing)
  rm(list=c('net_nutrient_soil','FRAC_rf_grazing'))
}


loop_recentMemory_runoff_nutrient_grazing = function(nutrient, manure_surplus_fills_nutDemand = F, manure_method = 'Method 1') {
  
  if (manure_surplus_fills_nutDemand == TRUE) { folder_div = 'With_ManSurplus'} else { folder_div = 'Without_ManSurplus'}
  
  past_type = c('Intensive_pasture','Extensive_pasture')
  
  yrs <- paste0('X', seq(1987,2017))
  total <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  total[, yrs] <- sapply(yrs, function(x) total[,x] = 0)
  
  for (pasture in past_type) {
    
    rf_grazing = compute_recentMemory_runoff_nutrient_grazing(pasture, nutrient, manure_surplus_fills_nutDemand, manure_method)
    total[, yrs] <- sapply(yrs, function(x) total[,x] +rf_grazing[,x])
    
    export_file(module = 'Nutrients', 
                file = rf_grazing, 
                filename = pasture, 
                folder = 'Runoff', 
                subfolder = 'Recent_memory', 
                subfolderX2 = nutrient,
                subfolderX3 = manure_method,
                subfolderX4 = folder_div, 
                subfolderX5 = 'Grazing')
  }
  export_file(module = 'Nutrients', 
              file = total, 
              filename = 'Total', 
              folder = 'Runoff', 
              subfolder = 'Recent_memory', 
              subfolderX2 = nutrient,
              subfolderX3 = manure_method,
              subfolderX4 = folder_div, 
              subfolderX5 = 'Grazing')
}






compute_recentMemory_runoff_nutrient_fieldApplication = function(fert_type, nutrient='N', manure_surplus_fills_nutDemand = F, manure_method = 'Method 1') {
  # computes the recent memory runof losses from field application of fertilisers to the respective cops
  #unit: kg nutrient yr-1
  
  if (manure_surplus_fills_nutDemand == TRUE) { folder_div = 'With_ManSurplus'} else { folder_div = 'Without_ManSurplus'}
  
  if (fert_type == 'Manure' | fert_type == 'Inorganic') {
    
    standard_params = get_standard_params_list(main_param = 'Crops')
  }
  else {
    
    standard_params = get_sludge_distribution_crops()
  }
  
  
  for (i in 1:nrow(standard_params)) {
    
    main_param = standard_params[i, 'Main_crop']
    param = standard_params[i, 'Crop']
  
    if (fert_type == 'Manure' & (param == 'Extensive_pasture' | param == 'Tomato' | main_param == 'Horticulture')) {
      next
    }
    else if (fert_type == 'Inorganic' & param == 'Extensive_pasture') {
      next 
    }
    else {
      
      LU_crops = as.character(identify_main_crops_LU(main_param, param))
      FRAC_rf_LU = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Runoff', subfolder = 'LU_FRAC_runoff', subfolderX2 = 'Dataframe', subfolderX3 = 'Recent_Application', pattern = LU_crops)
      crop_net_nut_fert = compute_crop_net_N_fertilisers(main_param, param, fert_type, nutrient, manure_surplus_fills_nutDemand, manure_method)
      rf_nut_fert = crop_net_nut_fert
      
      yrs = paste0('X', seq(1987,2017))
      rf_nut_fert[, yrs] = sapply(yrs, function(x) round ( crop_net_nut_fert[, x] * FRAC_rf_LU[, x] , 1 ))
      
      export_file(module = 'Nutrients', 
                  file = rf_nut_fert, 
                  filename = param, 
                  folder = 'Runoff', 
                  subfolder = 'Recent_memory', 
                  subfolderX2 = nutrient,
                  subfolderX3 = manure_method,
                  subfolderX4 = folder_div, 
                  subfolderX5 = fert_type, 
                  subfolderX6 = main_param)
    
    }
  }
}



compute_recentMemory_runoff_nutrient_fieldApplication_per_mainParam = function(fert_type, nutrient='N', manure_surplus_fills_nutDemand = F, manure_method = 'Method 1') {
  # computes the sum of recent memory runoff losses for a given fertiliser for each main crop classes
  # unit: kg nutrient yr-1
  
  
  if (manure_surplus_fills_nutDemand == TRUE) { folder_div = 'With_ManSurplus'} else { folder_div = 'Without_ManSurplus'}
  
  if (fert_type == 'Manure' | fert_type == 'Inorganic') {
    
    standard_params = get_standard_params_list('Crops')
  }
  else {
    
    standard_params = get_sludge_distribution_crops()
  }
  
  yrs <- paste0('X', seq(1987,2017))
  store_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  store_df[, yrs] <- sapply(yrs, function(x) store_df[,x] <- 0)
  
  main_params = unique(standard_params[, 'Main_crop'])
  
  for (i in main_params) {
    
    param_df = subset(standard_params, Main_crop == i)
    main_param_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
    main_param_df[, yrs] <- sapply(yrs, function(x) main_param_df[,x] <- 0)
    print(i)
    for (j in 1:nrow(param_df)) {
      
      param = param_df[j, 'Crop']
      
      if (fert_type == 'Manure' & (param == 'Extensive_pasture'  | param == 'Tomato' | i == 'Horticulture')) {
        next
      }
      else if ((fert_type == 'Inorganic' | fert_type == 'Manure') & param == 'Extensive_pasture') {
        next 
      }
      else {
        
        crop_tot_man_NH3 = get_activity_data(module = 'Nutrients', 
                                             mainfolder =  'Output', 
                                             folder = 'Runoff', 
                                             subfolder = 'Recent_memory', 
                                             subfolderX2 = nutrient,
                                             subfolderX3 = manure_method, 
                                             subfolderX4 = folder_div,
                                             subfolderX5 = fert_type, 
                                             subfolderX6 = i, 
                                             pattern = param)
        # store in main_param_df
        main_param_df[, yrs] = sapply(yrs, function(x) round(main_param_df[,x] + crop_tot_man_NH3[,x], 1))
        
        # store in TOTAL df
        store_df[, yrs] = sapply(yrs, function(x) round(store_df[,x] + crop_tot_man_NH3[,x], 1))
      }
    }
    if (fert_type == 'Manure' & i == 'Horticulture') {
      next 
    }
    else {
      export_file(module = 'Nutrients', 
                  file = main_param_df, 
                  filename = i, 
                  folder = 'Runoff', 
                  subfolder = 'Recent_memory', 
                  subfolderX2 = nutrient,
                  subfolderX3 = manure_method, 
                  subfolderX4 = folder_div,
                  subfolderX5 = fert_type,
                  subfolderX6 = 'Total')  
    }
  }
  export_file(module = 'Nutrients', 
              file = store_df, 
              filename = 'Total', 
              folder = 'Runoff', 
              subfolder = 'Recent_memory', 
              subfolderX2 = nutrient,
              subfolderX3 = manure_method, 
              subfolderX4 = folder_div,
              subfolderX5 = fert_type,
              subfolderX6 = 'Total')
}




loop_recentMemory_runoff_nutrient_fieldApplication = function(nutrient='N', manure_surplus_fills_nutDemand = F, manure_method = 'Method 1') {
  
  fert = c('Manure','Inorganic','Biosolids')
  sapply(fert, function(x) {
     compute_recentMemory_runoff_nutrient_fieldApplication(x, nutrient, manure_surplus_fills_nutDemand,  manure_method)
     compute_recentMemory_runoff_nutrient_fieldApplication_per_mainParam(x, nutrient, manure_surplus_fills_nutDemand,  manure_method)
  })
}



compute_total_runoff_nutrient_losses = function(nutrient='N', manure_surplus_fills_nutDemand = F, manure_method = 'Method 1') {
  # computes the total runoff losses as the usm of grazing and field application of organic and inorganic fertilisers
  # unit: kg nutrient yr-1
  
  if (manure_surplus_fills_nutDemand == TRUE) { folder_div = 'With_ManSurplus'} else { folder_div = 'Without_ManSurplus'}
  

  yrs <- paste0('X', seq(1987,2017))
  store_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  store_df[, yrs] <- sapply(yrs, function(x) store_df[,x] <- 0)
  
  # add grazing runoff losses
  rf_grazing = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Runoff', subfolder = 'Recent_memory', 
                                 subfolderX2 = nutrient, subfolderX3 =manure_method, subfolderX4 = folder_div, 
                                 subfolderX5 = 'Grazing', pattern = 'Total')
  store_df [, yrs] = sapply(yrs, function(x) round(rf_grazing[,x] + store_df[, x], 1))
  
  fert = c('Manure','Inorganic','Biosolids')
  
  for (i in fert) {
    

    rf_fert = get_activity_data(module = 'Nutrients', 
                                           mainfolder =  'Output', 
                                           folder = 'Runoff', 
                                           subfolder = 'Recent_memory', 
                                           subfolderX2 = nutrient,
                                           subfolderX3 = manure_method, 
                                           subfolderX4 = folder_div,
                                           subfolderX5 = i, 
                                           subfolderX6 = 'Total', 
                                           pattern = 'Total')
    
    store_df [, yrs] = sapply(yrs, function(x) round(store_df[,x] + rf_fert[, x], 1))
  }
  export_file(module = 'Nutrients', 
              file = store_df, 
              filename = 'Total', 
              folder = 'Runoff', 
              subfolder = 'Recent_memory', 
              subfolderX2 = nutrient,
              subfolderX3 = manure_method, 
              subfolderX4 = folder_div,
              subfolderX5 = 'Total',
              subfolderX6 = 'Total')
}

