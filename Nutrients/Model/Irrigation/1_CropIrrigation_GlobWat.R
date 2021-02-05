source('./Water/Model/Water_balance/4_CropIrrigation.R')

# GLOBWAT IRRIGATION WITHDRAWAL ----------------------------------------------------------------------------------------------------------------------------------
#' computes the total irrigation withdrawal for 1995-2017 at the municipality scale
#' crop irrigation demand/withdrawals were derived using monthly GlobWat vertical water balance


yrs = paste0('X',seq(1995,2017))

find_calculationYears = function(df) {
  #' @param df any df
  #' @description finds the computation years
  #' must be applied to these functions below
  #' 
  
  calc_yrs = names(df)
  calc_yrs = calc_yrs[4:length(calc_yrs)]
  calc_yrs
}

volumetric_cropIWW_globwat = function(main_param, param, method) {
  #' @param main_param main crop
  #' @param param crop
  #' @param method irrigation method
  #' @description computes the volumetric irrigation withdrawal for 1995-2017 for a given crop
  #' @unit m3 yr-1
  
  crop_irrig_area = get_activity_data(module = 'Nutrients',folder = 'Correct_data_Municipality',subfolder = 'Irrigation',subfolderX2 = 'Correct_irrigated_areas_method',subfolderX3 = method, subfolderX4 = main_param, pattern = param)   
  cropIWW = get_activity_data(module = 'Water', mainfolder = 'Output', folder = 'Crop_irrigation_withdrawals', subfolder = 'Avg_municipality', subfolderX2 = main_param, subfolderX3 = param, pattern = method)
  
  # because in some years specific crops were not irrigated (e.g., 2014 for forage roots)
  # we have to manually find the calculation years
  calc_yrs = find_calculationYears(cropIWW)
  
  cropIWW[, calc_yrs] = sapply(calc_yrs, function(x) round(cropIWW[,x] * crop_irrig_area[,x], 0)) # m3 yr-1
  
  return(cropIWW)
  rm(list=c('calc_yrs','crop_irrig_area'))
}



loop_volumetric_cropIWW_globwat = function() {
  
  # computes the total irrigation requirement for a given crop for all municipalities
  # unit: m3 yr-1
  
  irrig_id = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Irrigation', subfolderX2 = 'Portugal_statistics', pattern = 'Crop_ids')
  
  # total irrigation requirement@mainland
  IWW_mainland = get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  IWW_mainland[, yrs] = sapply(yrs, function(x) IWW_mainland[,x] = 0)
  
  for (i in 1:nrow(irrig_id)) {
    
    main_param = irrig_id[i, 'Main_crop']
    param = irrig_id[i, 'Crop']
    param = ifelse(param == 'Other_industry','Tomato',param)
  
    # total corp IWW (for all methods)
    tot_cropIWW = get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
    tot_cropIWW[, yrs] = sapply(yrs, function(x) tot_cropIWW[,x] = 0)
    
    irrig_methods = get_crop_irrigMethods(main_param, param)
    
    for (method in irrig_methods) {
      
      cropIWW = volumetric_cropIWW_globwat(main_param, param, method)
      # export crop IWW for a given method
      export_file(module = 'Nutrients', 
                  folder = 'Irrigation', 
                  subfolder = 'Irrigation_requirements', 
                  subfolderX2 = 'GlobWat',
                  subfolderX3 = method, 
                  subfolderX4 = main_param,
                  file = cropIWW, 
                  filename = param)
      
      calc_yrs = find_calculationYears(cropIWW) # find computation years
      
      # compute totals !
      tot_cropIWW[, calc_yrs] = sapply(calc_yrs, function(x) tot_cropIWW[,x] + cropIWW[,x])
      IWW_mainland[, calc_yrs] = sapply(calc_yrs, function(x) IWW_mainland[,x] + cropIWW[,x])
    }
    # export crop total irrigation!
    export_file(module = 'Nutrients', 
                folder = 'Irrigation', 
                subfolder = 'Irrigation_requirements', 
                subfolderX2 = 'GlobWat',
                subfolderX3 = 'Total', 
                subfolderX4 = main_param,
                file = tot_cropIWW, 
                filename = param)
  }
  # export mainland totals
  export_file(module = 'Nutrients', 
              folder = 'Irrigation', 
              subfolder = 'Irrigation_requirements', 
              subfolderX2 = 'GlobWat', 
              subfolderX3 = 'Total',
              subfolderX4 = 'Total',
              file = IWW_mainland, 
              filename = 'Total')
}


compute_total_irrigation_requirements_per_maincrop = function() {
  #' @description calculates the total irrigation requirements (IWR) for each main crop
  #' unit is m3 yr-1
  
  irrig_id = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Irrigation', subfolderX2 = 'Portugal_statistics', pattern = 'Crop_ids')
  main_params = unique(irrig_id$Main_crop)
  
  
  for (main_param in main_params) {
    print(main_param)
    # store main param IWW
    tot_irrig <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
    tot_irrig[, yrs] = sapply(yrs, function(x) tot_irrig[,x] = 0)
    
    crops = subset(irrig_id, Main_crop == main_param)[, 'Crop']
    
    for (crop in crops) {
      
      crop = ifelse(crop == 'Other_industry','Tomato',crop)
      IWW_crop = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Irrigation', subfolder = 'Irrigation_requirements', subfolderX2 = 'GlobWat', subfolderX3 = 'Total', subfolderX4 = main_param, pattern = crop)
      tot_irrig[, yrs] = sapply(yrs, function(x) round(tot_irrig[,x] + IWW_crop[,x], 1))
    }
    export_file(module = 'Nutrients', 
                folder = 'Irrigation', 
                subfolder = 'Irrigation_requirements', 
                subfolderX2 = 'GlobWat', 
                subfolderX3 = 'Total',
                subfolderX4 = 'Total',
                file = tot_irrig, 
                filename = main_param)
  }
}
