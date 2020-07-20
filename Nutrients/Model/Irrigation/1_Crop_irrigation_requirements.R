source('./Main/Global_functions.R')
source('./Nutrients/Model/Irrigation/Support/1_Populate_2009_irrigated_areas.R')

# set calculation period ---------------------------------------------------------------------------------------------------------------

yrs = paste0('X','2009')



# prepare crop irrigation requirements template -----------------------------------------------------------------------------------------

find_crop_irrigation_method = function(irrigation_volume_df, param) {
  # finds the row and column of a given crop 
  
  crop_row = which(irrigation_volume_df[, 'crop']==param)

  irrigation_volume_df = irrigation_volume_df[crop_row, ]
  return(irrigation_volume_df)
}

create_crop_irrigation_requirements = function(main_param, param) {
  # creates a df template for a given crop with the regional irrigation requirements for all irrig systems
  
  muni_template = get_activity_data(module = 'Nutrients', folder = 'General_params',subfolder = 'Irrigation',subfolderX2 = 'Irrigation_requirements', pattern = 'Template_regions')
  region = unique(muni_template$Volume_regions)
  
  for (irrig_region in region) {
    
    irrig_region_template = get_activity_data(module = 'Nutrients', folder = 'General_params',subfolder = 'Irrigation',subfolderX2 = 'Irrigation_requirements', pattern = irrig_region)
    irrig_region_template = find_crop_irrigation_method(irrig_region_template, param)
    
    
    new_cols = names(irrig_region_template)[-c(1,2)]
    
    irrig_region_rows = which(muni_template[, 'Volume_regions'] == irrig_region)
    muni_template[irrig_region_rows, new_cols] = irrig_region_template[, -c(1,2)]
  }
  
  return(muni_template)
  rm(list=c('irrig_region_template','new_cols','irrig_region_rows'))
}




# compute irrigation requirements --------------------------------------------------------

compute_crop_irrigation_requirement = function(main_param, param, irrig_method) {
  # computes irrigation water requirements for all municipalities for a given crop and irrigation method
  # unit: m3 yr-1
  
  
  irrig_requirement = create_crop_irrigation_requirements(main_param, param)[, c('Muni_ID',irrig_method)]
  crop_irrig_area = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Irrigation', subfolderX2 = 'Irrigated_areas', subfolderX3 = irrig_method, subfolderX4 = main_param, pattern = param)
  crop_irrig_vol = crop_irrig_area
  
  if (param == 'Rice' & irrig_method == 'other_gravity') {
    # if rice, add 20 cm water
    crop_irrig_vol[, yrs] = sapply(yrs, function(x) round( (crop_irrig_area[, x] * irrig_requirement[, irrig_method]) + 2000, 0))
    
  }
  else {
    
    # calculate necessary water volume for that irrigation method and crop
    crop_irrig_vol[, yrs] = sapply(yrs, function(x) round(crop_irrig_area[, x] * irrig_requirement[, irrig_method], 0))
  }
  
  return(crop_irrig_vol)
  rm(list=c('irrig_requirement','crop_irrig_area'))
}


compute_all_crop_irrigation_method_requirement = function(irrig_method) {
  # computes all crop requirements for ONE irrigation method
  # unit: m3 yr-1
  print(paste0('Working in ', irrig_method))
  irrig_id = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Irrigation', subfolderX2 = 'Portugal_statistics', pattern = 'Crop_ids')
  
  for (i in 1:nrow(irrig_id)) {
    
    main_param = irrig_id[i, 'Main_crop']
    param = irrig_id[i, 'Crop']
    param = ifelse(param == 'Other_industry','Tomato',param)
    
    irrig_req_method = compute_crop_irrigation_requirement(main_param, param, irrig_method)
    export_file(module = 'Nutrients', 
                folder = 'Irrigation', 
                subfolder = 'Irrigation_requirements', 
                subfolderX2 = irrig_method, 
                subfolderX3 = main_param,
                file = irrig_req_method, 
                filename = param)
  }
}


loop_crop_irrigation_requirements = function() {
  # compute irrigation requirements for all corps and irrigation methods
  # unit: m3 yr-1
  
  irrig_methods = set_irrigation_methods_INE_codes()[, -2]
  sapply(irrig_methods, function(x) compute_all_crop_irrigation_method_requirement(x))
}




# total irrigation requirements per crop -----------------------------------------

compute_total_crop_irrigation_requirements = function() {
  # computes the total irrigation requirement for a given crop for all municipalities
  # unit: m3 yr-1
  
  irrig_id = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Irrigation', subfolderX2 = 'Portugal_statistics', pattern = 'Crop_ids')
  irrig_methods = set_irrigation_methods_INE_codes()[, -2]
  
  # total irrigation requirement
  tot_irrig <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  tot_irrig[, yrs] = sapply(yrs, function(x) tot_irrig[,x] = 0)
  
  for (i in 1:nrow(irrig_id)) {
    
    main_param = irrig_id[i, 'Main_crop']
    param = irrig_id[i, 'Crop']
    param = ifelse(param == 'Other_industry','Tomato',param)
   
    # total irrigation crop requirement
    crop_tot_irrig <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
    crop_tot_irrig[, yrs] = sapply(yrs, function(x) crop_tot_irrig[,x] = 0)
    
    for (method in irrig_methods) {
      
      # get crop volume per each irrigation system and sum everything
      irrig_method_vol = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Irrigation', subfolder = 'Irrigation_requirements', subfolderX2 = method, subfolderX3 = main_param, pattern = param)
      crop_tot_irrig[, yrs] = sapply(yrs, function(x) crop_tot_irrig[,x] + irrig_method_vol[,x])
      tot_irrig[, yrs] = sapply(yrs, function(x) tot_irrig[,x] + irrig_method_vol[,x])
      
    }
    # export crop total irrigation!
    export_file(module = 'Nutrients', 
                folder = 'Irrigation', 
                subfolder = 'Irrigation_requirements', 
                subfolderX2 = 'Total', 
                subfolderX3 = main_param,
                file = crop_tot_irrig, 
                filename = param)
  }
  export_file(module = 'Nutrients', 
              folder = 'Irrigation', 
              subfolder = 'Irrigation_requirements', 
              subfolderX2 = 'Total', 
              subfolderX3 = 'Total',
              file = tot_irrig, 
              filename = 'Total')
}

