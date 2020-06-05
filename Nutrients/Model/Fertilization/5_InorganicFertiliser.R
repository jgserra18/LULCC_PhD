source('./Nutrients/Model/Fertilization/4_BiosolidsAllocation.R')



## COMPILE IFASTAT FERTILISER DATA FOR PORTUGAL  ----------------------------------------------------------------- 

compute_IFASTAT_total_fert = function(nutrient) {
  
  
  IFASTAT_fert_nutrient = get_activity_data(module = 'Nutrients', folder = 'Nutrient_params', subfolder = nutrient, subfolderX2 = 'Fertilisers', subfolderX3 = 'Inorganic_fertiliser', pattern = 'IFASTAT')
  IFASTAT_fert_nutrient = IFASTAT_fert_nutrient[IFASTAT_fert_nutrient$Year>1986, ]
  
  if (nutrient == 'N') {
    
    
    store_df = data.frame(Product = 'Total', Year = seq(1987,2017), Fert_ktN = 0)
    
    yrs = seq(1987,2017)
    ctr = 0
    
    for (i in yrs) {
      
      ctr = ctr + 1
      IFASTAT_fert_nutrient_yr = subset(IFASTAT_fert_nutrient, Year == i)
      store_df[ctr, 'Fert_ktN'] = sum(IFASTAT_fert_nutrient_yr[, 'Fert_ktN'])
    }
    IFASTAT_fert_nutrient = store_df
  }
  
 return(IFASTAT_fert_nutrient) 
}



general_func_fertiliser_FAN = function(nutrient, fert_mainland_df) {
  # REQUIREMENT: fert_mainland_df must be in kg nutrient yr-1
  
  yrs = paste0('X', seq(1987,2017))
  IFASTAT = compute_IFASTAT_total_fert(nutrient)
  fert_mainland_df = sapply(yrs, function(x) round(sum(fert_mainland_df[, x]/1000000), 0))
  
  IFASTAT$Estimated_Fert_ktN = fert_mainland_df      
  IFASTAT$FAN = round(IFASTAT$Estimated_Fert_ktN/IFASTAT$Fert_ktN, 2)
  IFASTAT$Fert_excess = IFASTAT$Estimated_Fert_ktN - IFASTAT$Fert_ktN 
  
  return(IFASTAT)
}


## COMPUTE MAINLAND UNADJUSTED FERTILISERT NUTRIENT DEMAND ----------------------------------------------------------------- 

compute_crop_fertiliser_nutrient_demand = function(nutrient, main_param, param, manure_method = 'Method 1') {
  # **** BY DEFAULT IT IS APPLIED THE MANURE DISTRIBUTION METHOD I
  # Horticulture and Industry crops fert demand = nutrient demand
  # for the remaining, fert demand correspond to the nutrient demand following the applcation of manure and biosolids
  # unit: kg nutrient yr-1
  
  if (param == 'Extensive_pasture') {
    
    stop('These are not fertilised.')  
  }
  else if (main_param == 'Horticulture' | param == 'Tomato') {
    
    crop_fert = get_activity_data(module = 'Nutrients', mainfolder =  'Output', folder = 'Fertilisation', subfolder = nutrient, subfolderX2 = 'Crop_requirements', subfolderX3 = main_param, pattern = param)
  }
  else {
    
    crop_fert = crop_nutrientReq_minus_Biosolid(nutrient, main_param, param, manure_method)[[1]]
  }
  
  return(crop_fert)
}

compute_total_unadjusted_fertiliser_nutrient_demand = function(nutrient, manure_method = 'Method 1') {
    # computes the total nutrient demand from inorganic fertilisers to satisfy demand
    # default: method I manure approach
    # unit: kg nutrient yr-1
  
  # store main dataframe to update biosolid_surplus after crop application
  yrs = paste0('X', seq(1987,2017))
  main_fert = get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  main_fert[, yrs] = sapply(yrs, function(x) main_fert[,x] = 0)
  
  standard_params <- get_standard_params_list(main_param = 'Crops')
  
  for (i in 1:nrow(standard_params)) {
    
    main_param = standard_params[i, 'Main_crop']
    param = standard_params[i, 'Crop']
    print(param)
    
    if (param == 'Extensive_pasture') {
      next
    }
    else {
      
      crop_fert = compute_crop_fertiliser_nutrient_demand(nutrient, main_param, param, manure_method)
    }

    # update total fert nutrient demand
    main_fert[, yrs] = sapply(yrs, function(x) round( main_fert[,x] + crop_fert[, x], 1))
  }
  
  export_file(module = 'Nutrients', 
              file = main_fert, 
              filename = 'Unadjusted_fert_mainland', 
              folder = 'Fertilisation', 
              subfolder = 'N', 
              subfolderX2 = 'Inorganic_fertiliser',
              subfolderX3 = manure_method,
              subfolderX4 = 'Total',
              subfolderX5= 'Unadjusted')
  rm(list='crop_fert', 'main_fert')
}


## COMPUTE ADJUSTED FERTILISER CROP APPLICATION RATE ----------------------------------------------------------------- 

compute_fertiliser_FAN = function(nutrient) {
  # computes the fertiliser FAN indicator
  
  unadj_fert_mainland = get_activity_data(module = 'Nutrients', 
                                          mainfolder =  'Output', 
                                          folder = 'Fertilisation', 
                                          subfolder = nutrient, 
                                          subfolderX2 = 'Inorganic_fertiliser',
                                          subfolderX3 = 'Method 1', 
                                          subfolderX4 = 'Total', 
                                          subfolderX5 = 'Unadjusted', 
                                          pattern = 'Unadjusted')
  FAN = general_func_fertiliser_FAN(nutrient = nutrient, fert_mainland_df = unadj_fert_mainland)
  FAN$Year = paste0('X', FAN$Year)
  
  return(FAN)
  rm(list=c('unadj_fert_mainland'))
}


get_annual_fertiliser_FAN = function(FAN_df, year) {
  
  yr_FAN = FAN_df[which(FAN_df[, 'Year'] == year), 'FAN']
  return(yr_FAN)
}


compute_adjusted_fertiliser_crop_app_rates = function(nutrient, main_param, param, manure_method='Method 1') {
  
  FAN = compute_fertiliser_FAN(nutrient)
  crop_fert = compute_crop_fertiliser_nutrient_demand(nutrient, main_param, param, manure_method)
  crop_area = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = main_param, pattern = param)
  
  yrs = paste0('X', seq(1987,2017))
  
  crop_fert[, yrs] = sapply(yrs, function(x) {
    
    yr_FAN = get_annual_fertiliser_FAN(FAN, x)
    crop_fert[, x] = round(crop_fert[, x] * yr_FAN / crop_area[, x], 1)
  })
  
  crop_fert = data_cleaning(crop_fert)
  
  return(crop_fert)
  rm(list=c('FAN','yr_FAN'))
}


loop_adjusted_fertiliser_crop_app_rates = function(nutrient, manure_method = 'Method 1') {
  
  standard_params <- get_standard_params_list(main_param = 'Crops')
  
  for (i in 1:nrow(standard_params)) {
    
    main_param = standard_params[i, 'Main_crop']
    param = standard_params[i, 'Crop']

    if (param == 'Extensive_pasture') {
      next
    }
    else {
      
      crop_fert = compute_adjusted_fertiliser_crop_app_rates(nutrient, main_param, param, manure_method)
      export_file(module = 'Nutrients', 
                  file = crop_fert, 
                  filename = param, 
                  folder = 'Fertilisation', 
                  subfolder = nutrient, 
                  subfolderX2 = 'Fertiliser_application_rates',
                  subfolderX3 = manure_method,
                  subfolderX4 = main_param)
    }
  }
}


# COMPILE SUPPORT DATA --------------------------------

write_fert_mainland_dataset = function(nutrient, manure_method = 'Method 1') {
  
  unadj_fert_mainland = get_activity_data(module = 'Nutrients', 
                                          mainfolder =  'Output', 
                                          folder = 'Fertilisation', 
                                          subfolder = nutrient, 
                                          subfolderX2 = 'Inorganic_fertiliser',
                                          subfolderX3 = manure_method, 
                                          subfolderX4 = 'Total', 
                                          subfolderX5 = 'Unadjusted', 
                                          pattern = 'Unadjusted')
  dataset = general_func_fertiliser_FAN(nutrient, unadj_fert_mainland)
  export_file(module = 'Nutrients', 
              file = dataset, 
              filename = 'FAN_dataset', 
              folder = 'Fertilisation', 
              subfolder = 'N', 
              subfolderX2 = 'Inorganic_fertiliser',
              subfolderX3 = manure_method,
              subfolderX4 = 'Total',
              subfolderX5= 'Fert_dataset')
}
