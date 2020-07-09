source('./Main/Global_functions.R')
source('./Nutrients/Model/Fodder_production/5_Nutrients_FeedingRations.R')


# THIS MODULE MUST BE IMPLEMENTED ONLY AFTER FODDER PRODUCTION IS
# THE UNDERLYING ASSUMPTION IS THAT THE TOTAL NUTRIENT REMOVAL REGARDING FODDER CROPS EQUALS TO THAT FED TO THE ANIMALS
# LOSSES ARE NOT ACCOUNTED




# disaggregate pasture products per typology ------------------------------------

compute_FRAC_pastures = function(param, main_param = 'Pastures') {
  # computes the FRACTion of a given pasture type vs the total pastureland area
  # unit: %
  
  yrs = paste0('X',seq(1987,2017))
  temp = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = main_param, pattern = 'Intensive_pasture')
  perma = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = main_param, pattern = 'Extensive_pasture')
  
  FRAC_past = perma
  
  if (param == 'Intensive_pasture') {
    
    FRAC_past[, yrs] = sapply(yrs, function(x) round(temp[,x] / (temp[,x] + perma[,x]), 3))
  }
  else {
    
    FRAC_past[, yrs] = sapply(yrs, function(x) round(perma[,x] / (temp[,x] + perma[,x]), 3))
  }
  
  return(FRAC_past)
  rm(list=c('temp','perma','yrs'))
}


disaggregate_grass_hay_perPasture = function(crop_product, roughage_feed, param, nutrient = 'N', main_param = 'Pastures') {
  # it is assumed that intensive pastures are twice more intensive !
  # a threshold of grassland N yield of 241 kg N ha-1 yr-1 according to the maximum N yield in Velthof et al 2009; the remaining N yield is assumed to come from concentrates
  # unit : kg nutrient yr-1
  
  yield_modifier = ifelse(param == 'Intensive_pasture', 2, 1)
  past_area = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = main_param, pattern = param)
  FRAC_pasture = compute_FRAC_pastures(param)
  fodder_nutrient_flows = compute_total_nutrient_roughage_feed_ForagePasturesFlows(crop_product, roughage_feed, nutrient)
  
  # allocate nutrient flows based on the area fraction of a given pasture
  yrs = paste0('X',seq(1987,2017))
  fodder_nutrient_flows[, yrs] = sapply(yrs, function(x) round(fodder_nutrient_flows[,x] * FRAC_pasture[,x] * yield_modifier / past_area[,x], 2))
  
  # correct values according to the threshold of 241 kg N ha-1 yr-1
  fodder_nutrient_flows = data_cleaning(fodder_nutrient_flows)
  
  for (c in 4:ncol(fodder_nutrient_flows)) {
    
    for (r in 1:nrow(fodder_nutrient_flows)) {
      
      if (fodder_nutrient_flows[r,c]>241) {
        fodder_nutrient_flows[r,c] = 241
      }
    }
  }
  # re multiply the correct area values per past area
  fodder_nutrient_flows[, yrs] = sapply(yrs, function(x) round(fodder_nutrient_flows[,x] * past_area[,x], 2))
  
  return(fodder_nutrient_flows)
}



# re allocate PASTURES production to main products and residues -----------------------------------------------------

reallocate_pasture_nutrient_flows = function(nutrient = 'N') {
  
  standard_params <- get_standard_params_list(main_param = 'Crops')
  standard_params =standard_params[which(standard_params[, 1] == 'Pastures'), ]
  crop_product = c('Main_crop','Residues')
  
  for (product in crop_product) {
    
    feed = ifelse(product == 'Main_crop','Fresh_grass','Hay')
    
    for (i in 1:nrow(standard_params)) {
      
      main_crop = standard_params[i, 1] 
      crop = standard_params[i, 2]
      
      # get roughage nutrient flows
      intensive_nutrient_flows = disaggregate_grass_hay_perPasture(crop_product = product, roughage_feed = feed, param = 'Intensive_pasture', nutrient)
      extensive_nutrient_flows = disaggregate_grass_hay_perPasture(crop_product = product, roughage_feed = feed, param = 'Extensive_pasture', nutrient)
      
      if (product == 'Main_crop') {
        export_file(module = 'Nutrients', 
                    file = intensive_nutrient_flows, 
                    filename = 'Intensive_pasture', 
                    folder = 'Crop_offtake', 
                    subfolder = nutrient, 
                    subfolderX2 = 'Pastures')
        export_file(module = 'Nutrients', 
                    file = extensive_nutrient_flows, 
                    filename = 'Extensive_pasture', 
                    folder = 'Crop_offtake', 
                    subfolder = nutrient, 
                    subfolderX2 = 'Pastures')
        
      }
      else {
        export_file(module = 'Nutrients', 
                    file = intensive_nutrient_flows, 
                    filename = 'Intensive_pasture', 
                    folder = 'Crop_residues', 
                    subfolder = 'Removed',
                    subfolderX2 = nutrient,
                    subfolderX3 = 'Pastures')
        export_file(module = 'Nutrients', 
                    file = extensive_nutrient_flows, 
                    filename = 'Extensive_pasture', 
                    folder = 'Crop_residues', 
                    subfolder = 'Removed',
                    subfolderX2 = nutrient,
                    subfolderX3 = 'Pastures')
      }
    }
  }
}



# re allocate FORAGE production to main products and residues --------------------------------------------------------

reallocate_forage_nutrient_flows = function(nutrient = 'N') {
  # reallocates the roughage nutrient flows from "Fodder_production" module to crop offtake (Main_crop products) and respective residues
  # unit: kg nutrient yr-1
  
  standard_params <- get_standard_params_list(main_param = 'Crops')
  standard_params =standard_params[which(standard_params[, 1] == 'Forage'), ]
  crop_product = c('Main_crop','Residues')
  
  for (product in crop_product) {
    
      for (i in 1:nrow(standard_params)) {
        
        main_crop = standard_params[i, 1] 
        crop = standard_params[i, 2]
        
        # get roughage nutrient flows
        fodder_nutrient_flows = compute_total_nutrient_roughage_feed_ForagePasturesFlows(crop_product = product, roughage_feed = crop, nutrient)
        
        if (product == 'Main_crop') {
          export_file(module = 'Nutrients', 
                      file = fodder_nutrient_flows, 
                      filename = crop, 
                      folder = 'Crop_offtake', 
                      subfolder = nutrient, 
                      subfolderX2 = main_crop)
        }
        else {
          export_file(module = 'Nutrients', 
                      file = fodder_nutrient_flows, 
                      filename =crop, 
                      folder = 'Crop_residues', 
                      subfolder =  'Removed', 
                      subfolderX2 = nutrient,
                      subfolderX3 = main_crop)
        }
      }
  }
}



compute_total_forage_offtake = function(reference_area, nutrient = 'N') {
  # computes the total nutrient offtake from fodder crops
  # unit: kg N-P yr-1
  
  standard_params <- get_standard_params_list(main_param = 'Crops')
  standard_params =standard_params[which(standard_params[, 1] == 'Forage'), ]
  
  # prepare total nutrient flows for a given roughage feed
  yrs = paste0('X',seq(1987,2017))
  total <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  total[, yrs] <- sapply(yrs, function(x) total[,x] = 0)
  
  for (i in 1:nrow(standard_params)) {
    
    main_crop = standard_params[i, 1] 
    crop = standard_params[i, 2]
    
    fodder_crop_offtake = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Crop_offtake', subfolder = nutrient, subfolderX2 = 'Forage', pattern = crop)
    total[, yrs] = sapply(yrs, function(x) round(fodder_crop_offtake[,x] + total[, x], 0))
  }
  return(total)
}


compute_total_forage_residues = function(nutrient = 'N') {
  # computes the total nutrients removed from forage crop residues
  # excludes pastures
  # unit: kg N-P yr-1
  
  standard_params <- get_standard_params_list(main_param = 'Crops')
  standard_params =standard_params[which(standard_params[, 1] == 'Forage'), ]
  
  # prepare total nutrient flows for a given roughage feed
  yrs = paste0('X',seq(1987,2017))
  total <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  total[, yrs] <- sapply(yrs, function(x) total[,x] = 0)
  
  for (i in 1:nrow(standard_params)) {
    
    main_crop = standard_params[i, 1] 
    crop = standard_params[i, 2]
    
    fodder_crop_res = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Crop_residues', subfolder = 'Removed', subfolderX2 = nutrient, subfolderX3 = 'Forage', pattern = crop)
    total[, yrs] = sapply(yrs, function(x) round(fodder_crop_res[,x] + total[, x], 0))
  }
  
  return(total)
}


# REFERENCE AREAS ---------------------------------------

allocation_forage_offtake_flows_per_referenceArea = function(reference_area, nutrient = 'N') {
  # allocates fodder offtake flows according to the reference area
  # unit: kg N-P yr-1
  
  if (reference_area == 'Cropland') {
    
    yrs = paste0('X',seq(1987,2017))
    forage_offtake = compute_total_forage_offtake(nutrient)
    intensive_past_offtake = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Crop_offtake', subfolder = nutrient, subfolderX2 = 'Pastures', pattern = 'Intensive_pasture')
    
    # sum both
    forage_cropland_offtake = intensive_past_offtake
    forage_cropland_offtake[, yrs] = sapply(yrs, function(x) round(intensive_past_offtake[,x] + forage_offtake[, x], 0))
    
    return(forage_cropland_offtake)
    rm(list=c('forage_offtake','intensive_past_offtake'))
  }
  else {
    
    extensive_past_offtake =  get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Crop_offtake', subfolder = nutrient, subfolderX2 = 'Pastures', pattern = 'Extensive_pasture')
    
    return(extensive)
  }
}


allocation_forage_residues_flows_per_referenceArea = function(reference_area, nutrient = 'N') {
  # allocates fodder residues flows according to the reference area
  # unit: kg N-P yr-1
  
  if (reference_area == 'Cropland') {
    
    yrs = paste0('X',seq(1987,2017))
    forage_offtake = compute_total_forage_residues(nutrient)
    intensive_past_offtake = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Crop_residues', subfolder = 'Removed', subfolderX2 = nutrient, subfolderX3 = 'Pastures', pattern = 'Intensive_pasture')
    
    # sum both
    forage_cropland_offtake = intensive_past_offtake
    forage_cropland_offtake[, yrs] = sapply(yrs, function(x) round(intensive_past_offtake[,x] + forage_offtake[, x], 0))
    
    return(forage_cropland_offtake)
    rm(list=c('forage_offtake','intensive_past_offtake'))
  }
  else {
    
    extensive_past_offtake =  get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Crop_residues', subfolder = 'Removed', subfolderX2 = nutrient, subfolderX3 = 'Pastures', pattern = 'Extensive_pasture')
    
    return(extensive)
  }
}


