source('./Nutrients/Model/Fodder_production/5_Nutrients_FeedingRations.R')


# ONLY FOR MAIN CROP PRODUCTS ------------------------------------------------------\

# correct fodder N according to the respective crop areas;; any change is added to a "surplus" df which will be allocated to the concentrate N
# do the fodder allocation and calculate N yield for each crop (kg N ha-1 yr-1)
# convert yield to dry matter using DM contents (N_yield / DM_content * 1000)
# define threshold according to the maximum values in statistics portugal
# forage oat and forage maize for their respective crops; remaining forage crops according to the total forage production
# for grass, establish max possible yield according to toth et al 2008

# adjust the N intake according to these modifications and store the "surplus" yield
# this surplus yield will add to the remaining crop residues FRAC in the feeding rations



set_forage_params  = function(crop_product, roughage_feed) {
  # from GLEAMS model
  # units: % FM and g N kg DM-1
  
  if (crop_product  == 'Main_crop') {
    
    FRAC_DM = 0.9
    FRAC_N = 15
  }
  
  else if (crop_product == 'Residues') {
    
    FRAC_DM = 0.9
    FRAC_N = 7 
  }
  
  return(list(DM=FRAC_DM, N = FRAC_N))
}


set_pasture_params = function(crop_product, roughage_feed) {
  # from GLEAMS model
  
  FRAC_DM = 0.9
    
  if (crop_product  == 'Main_crop') { 
    
    FRAC_N = ifelse(roughage_feed == 'Intensive_pasture',31,22)
  }
  else {
    
    FRAC_N = ifelse(roughage_feed == 'Intensive_pasture',17,26)
  }
  
  return(list(DM=FRAC_DM, N = FRAC_N))
}



set_fodder_crop_areas_0 = function(main_param, roughage_feed) {
  # seta fodder crop areas to 1 if they exist; otherwise set to 0
  # used to identify where fodder Nutrient flows exist but shouldn't
  
  area = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = main_param, pattern = roughage_feed)
  
  for (c in 4:ncol(area)) {
    for (r in 1:nrow(area)) {
      
      if (area[r,c]>0) {
        area[r,c] = 1
      }
      else {
        area[r,c] = 0
      }
    }
  }
  
  return(area)
}


# FORAGE CROPS -------------------------------------------------------------------------------------------------------------------------------------------------

# 1 - correct nutrient flows where crop_area = 0 -------------------------------------------------------------------------------------------

correct_nutrient_forage_flows_area = function(crop_product = 'Main_crop', roughage_feed, nutrient = 'N') {
  # corrects nutrient flows from feeding (forage crops)
  # corrects it based on the existence of crop areas for each municipality and years
  # if not, it is set to 0 and the difference is allocated to concentrate feeding
  # unit: kg N-P yr-1
  
  yrs = paste0('X',seq(1987,2017))
  surplus_concentrate = get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  surplus_concentrate[, yrs] = sapply(yrs, function(x) surplus_concentrate[,x] = 0)
  
  if (roughage_feed == 'Fresh_grass' | roughage_feed == 'Hay') {
    stop('Only forage crops.')
  }
  else {
    
    area = set_fodder_crop_areas_0('Forage',roughage_feed)
    nut_feed = compute_total_nutrient_roughage_feed_ForagePasturesFlows(crop_product = crop_product, roughage_feed = roughage_feed, nutrient)
    
    # update correct nutrient feeding
    updated_nut_feed = nut_feed
    updated_nut_feed[, yrs] = sapply(yrs, function(x) round(area[,x] * nut_feed[,x], 1))
    updated_nut_feed = data_cleaning(updated_nut_feed)
    
    # calculate the difference; to be allocated to concentrate feeding
    surplus_concentrate[, yrs] = sapply(yrs, function(x) round(nut_feed[,x] - updated_nut_feed[,x], 1))
    surplus_concentrate = data_cleaning(surplus_concentrate)
  }
  
  return(list(updated_feeding = updated_nut_feed, surplus_concentrate = surplus_concentrate))
  rm(list=c('area','nut_feed'))
}


# 2 - calculate crop DM yield and correct it according to the definition of a maximum threshold ----------------------------------------------


define_forage_Nyield_threshold = function(roughage_feed) {
  # define maximum Nyields according to an expert-judgement defined maximum yield_DM according to statistics portugal for forage maize and oat
  # unit: kg N ha-1 yr-1
  
  if (roughage_feed == 'forage_maize') {
    
    max_Nyield = 900 # kg N ha-1 yr-1 using FRAC_N of 15 g N kg DM-1 and a max yield DM of 60,000 kg ha-1 yr-1
  } 
  else {
    
    max_Nyield = 450 # kg N ha-1 yr-1 using FRAC_N of 15 g N kg DM-1 and a max yield DM of 30,000 kg ha-1 yr-1
  }
  return(max_Nyield)
}


correct_forage_Nyields = function(crop_product = 'Main_crop', roughage_feed, nutrient = 'N') {
  # corrects nutrient N flows from forage crops according to maximum Nyields
  # updates concentrate surplus df
  # unit: kg N-P yr-1
  
  # gets the conceentrate surplus df
  yrs = paste0('X',seq(1987,2017))
  surplus_concentrate = correct_nutrient_forage_flows_area(crop_product, roughage_feed, nutrient)[[2]]
  
  if (roughage_feed == 'Fresh_grass' | roughage_feed == 'Hay') {
    stop('Only forage crops.')
  }
  else {
    
    # compute Nyield
    area = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = 'Forage', pattern = roughage_feed)
    nut_feed  = correct_nutrient_forage_flows_area(crop_product, roughage_feed, nutrient)[[1]]
    N_yield = nut_feed
    N_yield[, yrs] = sapply(yrs, function(x) round(nut_feed[,x] / area[,x], 1))
    N_yield = data_cleaning(N_yield)
    
    # correct according to the maximum Nyield allowed
    max_Nyield  = define_forage_Nyield_threshold(roughage_feed)
    
    for (c in 4:ncol(N_yield)) {
      for (r in 1:nrow(N_yield)) {
        
        if (N_yield[r,c] > max_Nyield) { 
          # correct based on max N yield
          N_yield[r,c] = max_Nyield  
          # recalculate total nutrient flows
          N_yield[r,c] = round(N_yield[r,c] * area[r,c], 1)
          # updated surplus concentrate
          surplus_concentrate[r,c] = round(surplus_concentrate[r,c] + (nut_feed[r,c] - N_yield[r,c]), 1)
        }
      }
    }

    
    return(list(updated_feeding = N_yield, surplus_concentrate = surplus_concentrate))
  }
}



reallocate_forage_nutrient_flows = function(nutrient = 'N') {
  # reallocates the roughage nutrient flows from "Fodder_production" module to crop offtake (Main_crop products) and respective residues
  # unit: kg nutrient yr-1
  
  standard_params <- get_standard_params_list(main_param = 'Crops')
  standard_params =standard_params[which(standard_params[, 1] == 'Forage'), ]
  crop_product = c('Main_crop')
  
  for (product in crop_product) {
    
    for (i in 1:nrow(standard_params)) {
      
      main_crop = standard_params[i, 1] 
      crop = standard_params[i, 2]
      
      # get roughage nutrient flows
   #   fodder_nutrient_flows = compute_total_nutrient_roughage_feed_ForagePasturesFlows(crop_product = product, roughage_feed = crop, nutrient)[[1]]
      
      if (product == 'Main_crop') {
        fodder_nutrient_flows = correct_forage_Nyields('Main_crop', crop, nutrient)[[1]]
        export_file(module = 'Nutrients', 
                    file = fodder_nutrient_flows, 
                    filename = crop, 
                    folder = 'Crop_offtake', 
                    subfolder = nutrient, 
                    subfolderX2 = main_crop)
      }
      else {
        fodder_nutrient_flows = adjust_scaling_forage_residues(crop_product = 'Residues', roughage_feed = crop, nutrient)[[1]]
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




# PASTURE CROPS -------------------------------------------------------------------------------------------------------------------------------------------------

# disaggregate into intensive and extensive pasture ----------------------------------------------------------------------------------------

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
  
  FRAC_past = data_cleaning(FRAC_past)
  
  return(FRAC_past)
  rm(list=c('temp','perma','yrs'))
}



disaggregate_areal_grass_hay_perPasture = function(crop_product, roughage_feed, param, nutrient = 'N', main_param = 'Pastures') {
  # disaggregates pasture crop products according to the fraction 
  # unit : kg nutrient yr-1
  
  FRAC_pasture = compute_FRAC_pastures('Extensive_pasture')
  fodder_nutrient_flows = compute_total_nutrient_roughage_feed_ForagePasturesFlows(crop_product, roughage_feed, nutrient)
  
  # allocate nutrient flows based on the area fraction of a given pasture
  yrs = paste0('X',seq(1987,2017))
  fodder_nutrient_flows[, yrs] = sapply(yrs, function(x) round(fodder_nutrient_flows[,x] * FRAC_pasture[,x], 2))
  
  return(fodder_nutrient_flows)
}


compute_disaggregated_grass_yield = function(crop_product = 'Main_crop', roughage_feed = 'Fresh_grass', param, nutrient = 'N', main_param = 'Pastures') {
  # unit: kg DM ha-1 yr-1
  
  params = set_pasture_params(crop_product, roughage_feed)
  FRAC_DM = params[[1]]
  FRAC_N = params[[2]]
  
  yrs = paste0('X',seq(1987,2017))
  nut_grass = disaggregate_areal_grass_hay_perPasture(crop_product, roughage_feed, param, nutrient)
  past_area = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = main_param, pattern = param)
  
  yield_DM = nut_grass
  yield_DM[, yrs] = sapply(yrs, function(x) round(
    (nut_grass[,x] / past_area[,x]) / (FRAC_DM * (FRAC_N / 1000)), 0))
  
  yield_DM = data_cleaning(yield_DM)
  return(yield_DM)
}






adjust_intensive_pasture_yields = function() {
  # unit: kg DM ha-1 yr-1
  
  # create surplus dfs
  yrs = paste0('X',seq(1987,2017))
  store = get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  store[, yrs] = sapply(yrs, function(x) store[,x] = 0)
  surplus_nutrient_int = store
  
  int_yield = compute_disaggregated_grass_yield('Main_crop', 'Fresh_grass', 'Intensive_pasture')
  max_int_yield = set_max_pasture_yield('Intensive_pasture')
  adjusted_int_yield = int_yield
  
  for (c in 4:ncol(int_yield)) {
    for (r in 1:nrow(int_yield)) {
      
      # first intensive pasture yields is adjusted, ie given priority
      # adjust the yield
      if (int_yield[r,c] > max_int_yield[r,4]) {
        
        adjusted_int_yield[r,c] = max_int_yield[r,4]
        
        # calculate remaining unallocated nutrient
        # these are the nutrient flows to allocate to extensive pasture until its maximum yields are attained
        surplus_nutrient_int[r,c] = int_yield[r,c] - adjusted_int_yield[r,c]
      }
      else {
        next
      }
    }
  }
  return(list(adjusted_int_yield = adjusted_int_yield, surplus_df = surplus_nutrient_int))
  rm(list=c('max_int_yield','int_yield'))
}


adjust_extensive_pasture_yields = function(int_pasture_list) {
  # unit: kg DM ha-1 yr-1
  
  # create surplus dfs
  yrs = paste0('X',seq(1987,2017))
  store = get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  store[, yrs] = sapply(yrs, function(x) store[,x] = 0)
  surplus_nutrient_ext = store
  surplus_nutrient_int = int_pasture_list[[2]]
  yield_difference = store
  
  ext_yield = compute_disaggregated_grass_yield('Main_crop', 'Fresh_grass', 'Extensive_pasture')
  max_ext_yield = set_max_pasture_yield('Extensive_pasture')
  adjusted_ext_yield = ext_yield
  

  for (c in 4:ncol(ext_yield)) {
    for (r in 1:nrow(ext_yield)) {
      
      # check if extensive -pasture maximum yield is achieved
      # if yes, set it to the maximum and store the surplus
      # else allocate the difference until this is achieved if possible
      if (ext_yield[r,c] >=  max_ext_yield[r,4]) {
        
        adjusted_ext_yield[r,c] = max_ext_yield[r,4]
        
        # calculate remaining unallocated nutrient
        # these are the nutrient flows to allocate to extensive pasture until its maximum yields are attained
        surplus_nutrient_ext[r,c] = ext_yield[r,c] - adjusted_ext_yield[r,c]
      }
      # if max_ext_yield > ext_yield
      else if (ext_yield[r,c] < max_ext_yield[r,4]) {
        # calculate difference between, ie nutrient flow to allocate from intensive pastures
        yield_difference[r,c] = max_ext_yield[r, 4] - ext_yield[r,c]
        
        # if the yield difference is larger than the available nutrients to allocate, allocate all
        # else allocate only what's needed
        if (yield_difference[r,c]<surplus_nutrient_int[r,c]) {
          
          adjusted_ext_yield[r,c] = yield_difference[r,c] + adjusted_ext_yield[r,c]
          # update surplus nutrient that is allocated as concentrate feeding, ie not needed
          surplus_nutrient_int[r,c] = surplus_nutrient_int[r,c] - yield_difference[r,c]
        }
        else if (yield_difference[r,c] == surplus_nutrient_int[r,c]) {
          
          adjusted_ext_yield[r,c] = yield_difference[r,c] + adjusted_ext_yield[r,c]
          surplus_nutrient_int[r,c] = 0
        }
        else {
          adjusted_ext_yield[r,c]  = surplus_nutrient_int[r,c] + adjusted_ext_yield[r,c]
          
          surplus_nutrient_int[r,c] = 0
        }
      }
    }
  }
  surplus_concentrate_df = surplus_nutrient_int
  surplus_concentrate_df[, yrs] = sapply(yrs, function(x) surplus_nutrient_int[,x] + surplus_nutrient_ext[,x])
  
  return(list(adjusted_ext_yield=adjusted_ext_yield, surplus_nutrients_concentrate = surplus_concentrate_df))
}




compute_pasture_Nyields = function()