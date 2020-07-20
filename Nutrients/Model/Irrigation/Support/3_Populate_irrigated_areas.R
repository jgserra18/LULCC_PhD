source('./Main/Global_functions.R')
source('./Nutrients/Model/Irrigation/Support/2_Populate_downscale_irrigable_areas.R')
source('./Nutrients/Model/Irrigation/Support/1_Populate_2009_irrigated_areas.R')


# 1- correct the total irrigated areas of maize and potato based on the "Correct_data_Municipality"; adjust the acreage of the different irrigation methods accordingly
# 2 - compute total irrigated areas in mainland and agrarian region for 2009
# 3 - get share of irrigable areas (%UAA) in mainland and AR for 2009 (ID - 0003005) and intrapolate to 1987-2017
# 3.1 - Calculate irrigable areas (in hectare)
# 3.2 - Irrigable areas (in hectares) per municipality 1989,1999 and 2009 - ID 0004385 
# 4 - get share of irrigated areas in irrigable areas and calculate total irrigated areas per AR
# 5 - Downscale 



# 1- correct the total irrigated areas of maize and potato based on the "Correct_data_Municipality"; adjust the acreage of the different irrigation methods accordingly

compute_total_irrig_area_X2009 = function(main_param, param) {
  # computes the total irrigated area of a given crop
  # unit: ha
  
  store_main_param <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  store_main_param[, 'X2009'] = 0
  
  methods=c('furrows','other_gravity','sprinkler','gun','pivot','drip','microsprink')
  
  for (method in methods) {
    
    irrig_area_method = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Irrigation', subfolderX2 = 'Irrigated_areas', subfolderX3 = method, subfolderX4 = main_param, pattern = param)
    store_main_param[, 'X2009'] = store_main_param[, 'X2009'] + irrig_area_method[, 'X2009']
  }
  
  return(store_main_param)
  rm(list='irrig_area_method')
}


correct_irrigated_maize_potato_X2009 = function(main_param = 'Cereals', param = 'Maize') {
  # ONLY: Irrigated_maize and Irrigated_potato
  # corrects the area of the irrigated areas using different methods according to the area of Irrigated_maize and Irrigated_potato
  
  
  census_pattern = ifelse(param == 'Maize','Irrigated_maize','Irrigated_potato')
  # compute total adjustment factor
  AG_census_tot_irrig = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = main_param, pattern = census_pattern)
  X2009_tot_irrig = compute_total_irrig_area_X2009(main_param, param)
  
  adj_factor = X2009_tot_irrig
  adj_factor[,'X2009'] = round(X2009_tot_irrig[,'X2009'] / AG_census_tot_irrig[,'X2009'], 2)
  adj_factor = data_cleaning(adj_factor)
  
  methods=c('furrows','other_gravity','sprinkler','gun','pivot','drip','microsprink')
  
  for (method in methods) {
    
    irrig_area_method = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Irrigation', subfolderX2 = 'Irrigated_areas', subfolderX3 = method, subfolderX4 = main_param, pattern = param)
    irrig_area_method[,'X2009'] = round(irrig_area_method[,'X2009'] * adj_factor[,'X2009'], 1)
    
    export_file(module = 'Nutrients', 
                folder = 'Activity_data', 
                subfolder = 'Correct_data_Municipality', 
                subfolderX2 = 'Irrigation', 
                subfolderX3 = 'Irrigated_areas',
                subfolderX4 = method, 
                subfolderX5 = main_param, 
                filename = param, 
                file = irrig_area_method)  }
}

loop_correct_irrigated_maize_potato_X2009 = function() {
  
  correct_irrigated_maize_potato_X2009('Cereals','Maize')
  correct_irrigated_maize_potato_X2009('Potato','Potato')
}


# compute total irrigated areas based on the fraction in irrigable areas -------------------

set_FRAC_irrigated_in_irrigable = function() {
  # gets data from statistics portugal and linearly interpolates it to other years
  # unit: % irrigated areas in irrigable areas @mainland
  
  # from STATISTICS PORTUGAL
  yrs = c('1989','1999','2007','2009','2013','2016')
  yrs = as.numeric(yrs)
  FRAC_irrigated = c(72,77,72,87,87,87)
  df = data.frame(yrs=yrs, FRAC_irrigated = FRAC_irrigated)
  
  # interpolate to other years
  xout = c(c(1987,1988), seq(1990,1998), seq(2000,2006), 2008, seq(2010,2012), seq(2014,2015), 2017)
  main = data.frame(yrs = seq(1987,2017))
  ctr = 0
  for (yr in yrs) {
    ctr = ctr + 1  
    id = which(main$yrs == yr)
    main[id, 'FRAC_irrigated'] = df[ctr, 2]
  }
  new_df <- approx(x= as.numeric(main$yrs), y = main[,2], 
                   xout = xout, rule = 2)
  
  new_df = as.data.frame(new_df)
  names(new_df) = names(df)
  
  for (yr in xout) {
    main[which(main$yrs==yr), 2] = new_df[which(new_df$yrs==yr), 2]
  }
  
  main = data.table::transpose(main)
  main = main[-1, ]
  names(main) = paste0('X',seq(1987,2017))
  return(main)
}

compute_total_irrigated_areas = function() {
  # computes the total irrigated areas per municipality as:
  # irrig_areas = irrigable_areas * FRAC_irrigated
  # unit: hectares
  
  FRAC_irrigated = set_FRAC_irrigated_in_irrigable() 
  irrigable_areas_muni =  get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Irrigation', subfolderX2 = 'Irrigable_areas', subfolderX3 = 'Municipality', pattern = 'Total')
  total_irrig_areas = irrigable_areas_muni
  
  yrs = paste0('X',seq(1987,2017))
  total_irrig_areas[, yrs] = sapply(yrs, function(x) round(irrigable_areas_muni[, x] * FRAC_irrigated[,x]/100, 1))
  
  return(total_irrig_areas)
  rm(list=c('FRAC_irrigated','irrigable_areas_muni'))
}




# compute total irrigated areas in 2009 --------------------------------------------------------------

compute_total_X2009_irrigated_areas = function() {
  # computes the total irrigation requirement for a given crop for all municipalities
  # unit: m3 yr-1
  
  yrs = 'X2009'
  irrig_id = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Irrigation', subfolderX2 = 'Portugal_statistics', pattern = 'Crop_ids')
  irrig_methods = set_irrigation_methods_INE_codes()[, -2]
  
  # total irrigation requirement
  tot_irrig <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  tot_irrig[, yrs] = sapply(yrs, function(x) tot_irrig[,x] = 0)
  
  for (i in 1:nrow(irrig_id)) {
    
    main_param = irrig_id[i, 'Main_crop']
    param = irrig_id[i, 'Crop']
    param = ifelse(param == 'Other_industry','Tomato',param)
    
    
    for (method in irrig_methods) {
      
      # get crop volume per each irrigation system and sum everything
      irrig_method_vol =  get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Irrigation', subfolderX2 = 'Irrigated_areas', subfolderX3 = method, subfolderX4 = main_param, pattern = param)
      tot_irrig[, yrs] = sapply(yrs, function(x) tot_irrig[,x] + irrig_method_vol[,x])
    }
  }
  return(tot_irrig)
}


# correct modelled irrigated areas ------------------------------------------------------------------------------

#* Estimated areas are corrected based on 2009 ajdustment factor

adjustment_factor = function() {
  # computes an adjustment factor for 2009
  
  modelled = compute_total_irrigated_areas()[, c('Muni_ID','X2009')]
  statistics = compute_total_X2009_irrigated_areas()
  
  adj_factor = statistics
  adj_factor[, 'X2009'] = round(modelled[,'X2009'] / statistics[, 'X2009'], 2)
  
  adj_factor = data_cleaning(adj_factor)
  names(adj_factor)[4] = 'Adj_factor'
  
  return(adj_factor)
}



compute_adjusted_irrigated_areas = function() {
  # computes the adjusted irrigated areas, calibrated according to 2009 data
  # unit: hectares
  
  adj_factor = adjustment_factor()
  irrig_areas = compute_total_irrigated_areas()
  
  yrs = paste0('X',seq(1987,2017))
  irrig_areas[, yrs] = sapply(yrs, function(x) round(adj_factor[, 'Adj_factor'] * irrig_areas[, x], 1))
  
  return(irrig_areas)
}



# compute the fraction of irrigated areas and also different methods for 2009 at the municipality level ---------------------------------------


compute_X2009_FRAC_methods_per_crop = function(main_param, param, irrig_method, total_area_X2009) {
  # computes the fraction of a given irrigation method for a ccrop
  # unit: %total irrigated area
  
  irrig_area_method =  get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Irrigation', subfolderX2 = 'Irrigated_areas', subfolderX3 = irrig_method, subfolderX4 = main_param, pattern = param)

  yrs = 'X2009'
  FRAC_irrig_area_method = total_area_X2009
  FRAC_irrig_area_method[, yrs] = round(irrig_area_method[, yrs] / total_area_X2009[, yrs], 5)
  
  return(FRAC_irrig_area_method)
}


compute_total_crop_area_X2009 = function(main_param, param) {
  # unit: hectares
  
  irrig_methods = set_irrigation_methods_INE_codes()[, -2]
  
  yrs = 'X2009'
  # total crop irrigated areas
  tot_irrig <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  tot_irrig[, yrs] = sapply(yrs, function(x) tot_irrig[,x] = 0)
  
  for (method in irrig_methods) {
    
    irrig_area = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Irrigation', subfolderX2 = 'Irrigated_areas', subfolderX3 = method, subfolderX4 = main_param, pattern = param)
    tot_irrig[, yrs] = tot_irrig[, yrs] + irrig_area[, yrs]
  }
  
  return(tot_irrig)
}


compute_FRAC_crop_area_X2009 = function(main_param, param, total_area_X2009_df) {
  # computes the area fraction of a given crop compared to the total irrigated area in 2009
  # unit. %total irrigated area
  
  tot_irrig = compute_total_crop_area_X2009(main_param, param)
  
  # fraction 
  FRAC_crop_irrig = tot_irrig
  FRAC_crop_irrig[, yrs] = round(tot_irrig[, yrs] / total_area_X2009_df[, yrs], 4)
  
  return(FRAC_crop_irrig)
}



loop_X2009_FRAC_irrigated_areas_per_crop = function() {
  # computes for all crops the fraction of a irrigated crop area compared to the total irrigated area
  
  total_area_X2009 = compute_total_X2009_irrigated_areas()

  irrig_id = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Irrigation', subfolderX2 = 'Portugal_statistics', pattern = 'Crop_ids')
  
  for (i in 1:nrow(irrig_id)) {
    
    main_param = irrig_id[i, 'Main_crop']
    param = irrig_id[i, 'Crop']
    param = ifelse(param == 'Other_industry','Tomato',param)
    
    for (method in irrig_methods) {
      
      FRAC_crop_irrig = compute_FRAC_crop_area_X2009(main_param, param, total_area_X2009)
      FRAC_crop_irrig = data_cleaning(FRAC_crop_irrig)
      export_file(module = 'Nutrients', 
                  folder = 'Activity_data', 
                  subfolder = 'Correct_data_Municipality', 
                  subfolderX2 = 'Irrigation', 
                  subfolderX3 = 'Postprocessing_X2009',
                  subfolderX4 = 'Muni_Fraction_crop_areas',
                  subfolderX5 = main_param, 
                  filename = param, 
                  file = FRAC_crop_irrig)
    }
  }
}



loop_X2009_FRAC_irrigated_methods_per_crop = function() {
  # computes for all crops the fraction of a irrigated crop area for a given irrigation method compared to the total crop irrigated area
  

  irrig_id = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Irrigation', subfolderX2 = 'Portugal_statistics', pattern = 'Crop_ids')
  
  for (i in 1:nrow(irrig_id)) {
    
    main_param = irrig_id[i, 'Main_crop']
    param = irrig_id[i, 'Crop']
    param = ifelse(param == 'Other_industry','Tomato',param)
    
    tot_crop_irrig_area = compute_total_crop_area_X2009(main_param, param)
    
    for (method in irrig_methods) {
      
      FRAC_crop_irrig_method = compute_X2009_FRAC_methods_per_crop(main_param, param, method, tot_crop_irrig_area)
      FRAC_crop_irrig_method = data_cleaning(FRAC_crop_irrig_method)
      export_file(module = 'Nutrients', 
                  folder = 'Activity_data', 
                  subfolder = 'Correct_data_Municipality', 
                  subfolderX2 = 'Irrigation', 
                  subfolderX3 = 'Postprocessing_X2009',
                  subfolderX4 = 'Muni_Fraction_crop_methods',
                  subfolderX5 = method, 
                  subfolderX6 = main_param, 
                  filename = param, 
                  file = FRAC_crop_irrig_method)
    }
  }
}


# compute the fraction of irrigated areas and also different methods for 2009 at the agrarian region level ---------------------------------------
# hard coded; general functions can be further implemented

compute_X2009_FRAC_methods_per_crop_AR = function(main_param, param, irrig_method, total_area_X2009) {
  # computes the fraction of a given irrigation method for a ccrop
  # unit: %total irrigated area
  
  irrig_area_method =  get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Irrigation', subfolderX2 = 'Irrigated_areas', subfolderX3 = irrig_method, subfolderX4 = main_param, pattern = param)
  irrig_area_method = compute_temporal_sumIF_admin_df(admin = 'Agrarian_region', merge_df = irrig_area_method)
  
  yrs = 'X2009'
  FRAC_irrig_area_method = total_area_X2009
  FRAC_irrig_area_method[, yrs] = round(irrig_area_method[, yrs] / total_area_X2009[, yrs], 5)
  
  return(FRAC_irrig_area_method)
}



compute_total_crop_area_X2009_AR = function(main_param, param) {
  # unit: hectares
  
  irrig_methods = set_irrigation_methods_INE_codes()[, -2]
  
  yrs = 'X2009'
  # total crop irrigated areas
  tot_irrig <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  tot_irrig[, yrs] = sapply(yrs, function(x) tot_irrig[,x] = 0)
  
  for (method in irrig_methods) {
    
    irrig_area = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Irrigation', subfolderX2 = 'Irrigated_areas', subfolderX3 = method, subfolderX4 = main_param, pattern = param)
    tot_irrig[, yrs] = tot_irrig[, yrs] + irrig_area[, yrs]
  }
  
  tot_irrig = compute_temporal_sumIF_admin_df(admin = 'Agrarian_region', merge_df = tot_irrig)
  
  return(tot_irrig)
}

compute_FRAC_crop_area_X2009_AR = function(main_param, param, total_area_X2009_df) {
  # computes the area fraction of a given crop compared to the total irrigated area in 2009
  # unit. %total irrigated area
  
  tot_irrig = compute_total_crop_area_X2009(main_param, param)
  tot_irrig = compute_temporal_sumIF_admin_df(admin = 'Agrarian_region', merge_df = tot_irrig)
  
  # fraction 
  FRAC_crop_irrig = tot_irrig
  FRAC_crop_irrig[, yrs] = round(tot_irrig[, yrs] / total_area_X2009_df[, yrs], 4)
  
  return(FRAC_crop_irrig)
}


loop_X2009_FRAC_irrigated_areas_per_crop_AR()
loop_X2009_FRAC_irrigated_areas_per_crop_AR = function() {
  # computes for all crops the fraction of a irrigated crop area compared to the total irrigated area
  
  total_area_X2009 = compute_total_X2009_irrigated_areas()
  total_area_X2009 = compute_temporal_sumIF_admin_df(admin = 'Agrarian_region', merge_df = total_area_X2009)
  
  irrig_id = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Irrigation', subfolderX2 = 'Portugal_statistics', pattern = 'Crop_ids')
  
  for (i in 35:nrow(irrig_id)) {
    
    main_param = irrig_id[i, 'Main_crop']
    param = irrig_id[i, 'Crop']
    param = ifelse(param == 'Other_industry','Tomato',param)
    
    for (method in irrig_methods) {
      
      FRAC_crop_irrig = compute_FRAC_crop_area_X2009_AR(main_param, param, total_area_X2009)
      FRAC_crop_irrig = data_cleaning(FRAC_crop_irrig)
      export_file(module = 'Nutrients', 
                  folder = 'Activity_data', 
                  subfolder = 'Correct_data_Municipality', 
                  subfolderX2 = 'Irrigation', 
                  subfolderX3 = 'Postprocessing_X2009',
                  subfolderX4 = 'AR_Fraction_crop_areas',
                  subfolderX5 = main_param, 
                  filename = param, 
                  file = FRAC_crop_irrig)
    }
  }
}



loop_X2009_FRAC_irrigated_methods_per_crop_AR = function() {
  # computes for all crops the fraction of a irrigated crop area for a given irrigation method compared to the total crop irrigated area
  
  
  irrig_id = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Irrigation', subfolderX2 = 'Portugal_statistics', pattern = 'Crop_ids')
  
  for (i in 1:nrow(irrig_id)) {
    
    main_param = irrig_id[i, 'Main_crop']
    param = irrig_id[i, 'Crop']
    param = ifelse(param == 'Other_industry','Tomato',param)
    
    tot_crop_irrig_area = compute_total_crop_area_X2009_AR(main_param, param)
    
    for (method in irrig_methods) {
      
      FRAC_crop_irrig_method = compute_X2009_FRAC_methods_per_crop_AR(main_param, param, method, tot_crop_irrig_area)
      FRAC_crop_irrig_method = data_cleaning(FRAC_crop_irrig_method)
      export_file(module = 'Nutrients', 
                  folder = 'Activity_data', 
                  subfolder = 'Correct_data_Municipality', 
                  subfolderX2 = 'Irrigation', 
                  subfolderX3 = 'Postprocessing_X2009',
                  subfolderX4 = 'AR_Fraction_crop_methods',
                  subfolderX5 = method, 
                  subfolderX6 = main_param, 
                  filename = param, 
                  file = FRAC_crop_irrig_method)
    }
  }
}

