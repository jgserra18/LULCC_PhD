source('./Main/Global_functions.R')
source('./Nutrients/Model/Fertilization/3_1_CropManureAllocation.R')
source('./Nutrients/Model/Fertilization/2_Biosolids.R')

# /// CURRENTLY ONLY METHOD I IS PROPERLY IMPLEMENTED


# SLUDGE NUTRIENT DISTRIBUTION PRINCIPLES ------------------------------------------------------------------------------------------------
#* Because there aren't any specifications on biosolid crop application in Portugal, Serra et al 2019 crop assumptions are here applied
#* The algorithm used to allocate biosolid nutrient to crops is based on the relative area fraction in arable land


## MAIN ASSUMPTION -----------------------------------------------------------------------------------------------------------------------
#* BIOSOLID SURPLUS IS NOT RE-DISTRIBUTED
#* ONLY IMPLEMENTED FOR METHOD I FOR MANURE ALLOCATION



# ALLOCATE BIOSOLID TO THE DIFFERENT CROPS BASED ON ACREAGE FRACTION ---------------------------------------------------------------------


get_sludge_distribution_crops = function() {
  # alternative function to get_standard_params_list(main_param = 'Crops')
  
  params = get_standard_params_list(main_param = 'Crops')
  params = subset(params, Main_crop != 'Horticulture' & Main_crop != 'Industry_crops' & Crop != 'Extensive_pasture')
  
  return(params)
}


compute_biosolid_crop_application_fraction = function(main_param, param) {
  # calculates the acreage fraction of a given crop within the potential area with biosolid application
  
  arable_application_area = compute_arable_land_sludge_application()
  crop_area = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = main_param, pattern = param)
  
  yrs = paste0('X', seq(1987,2017))
  FRAC_crop_app = crop_area
  FRAC_crop_app[, yrs] = sapply(yrs, function(x) round( crop_area[, x] / arable_application_area[, x], 2))
  FRAC_crop_app = data_cleaning(FRAC_crop_app)
  
  return(FRAC_crop_app)
  rm(list=c('arable_application_area', 'crop_area','yrs'))
}



compute_biosolid_crop_application = function(nutrient, main_param, param) {
  # allocates nutrients from biosolids to a given crop according to the fraction of area with potentially biosolid application
  # unit: kg nutrient yr-1
  
  FRAC_crop_app = compute_biosolid_crop_application_fraction(main_param, param)
  biosolid_nutrient_muni = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Fertilisation', subfolder = 'N', subfolderX2 = 'Biosolids', subfolderX3 = 'Total', pattern = 'Total')
  biosolid_FRNV = set_N_FRV(nutrient, 'Biosolids')
  
  yrs = paste0('X', seq(1987,2017))
  crop_biosolid_nutrient_muni = biosolid_nutrient_muni
  crop_biosolid_nutrient_muni[, yrs] = sapply(yrs, function(x) round( biosolid_nutrient_muni[, x] * biosolid_FRNV * FRAC_crop_app[,x] , 1 ))
  
  return(crop_biosolid_nutrient_muni)
  rm(list=c('FRAC_crop_app','biosolid_nutrient_muni','yrs'))
}



# COMPUTE BIOSOLID CROP APPLICATION RATE, REMAINING NUTRIENT DEMAND AND BIOSOLID SURPLUS---------------------------------------------------------------------


select_RemNutrientRequirements_manure_method_approach = function(nutrient, main_param, param, manure_surplus_fills, manure_method) {
  # selects nutrient demand following solid manure applciation (calc2)
  #unit: kg nutrient yr-1
  
  
  if (manure_surplus_fills == TRUE) { folder_div = 'With_ManSurplus'} else { folder_div = 'Without_ManSurplus'}
  
  if (manure_method == 'Method 1') {
    
    rem_nutrient_req = calc2_nutrientDemand_solid_application(nutrient, main_param, param)[[1]]
  }
  else {
    
    rem_nutrient_req =  get_activity_data(module = 'Nutrients',
                                          mainfolder = 'Output',
                                          folder = 'Fertilisation', 
                                          subfolder = nutrient, 
                                          subfolderX2 = 'Nutrient_demand_afterManure',
                                          subfolderX3 = 'Method 2', 
                                          subfolderX4 = folder_div, 
                                          subfolderX5 = main_param, 
                                          pattern = param)

  }
  return(rem_nutrient_req)
}


crop_nutrientReq_minus_Biosolid = function(nutrient, main_param, param, manure_surplus_fills, manure_method) {
  # computes the remaining nutrient demand following biosolid
  # 1 - calls the remaining nutrient demand after manure application, corredcted to 0s
  # 2 - allocates nutrient biosolid application for a given crop
  # returns two things:
  #* index1 -->the remaining nutrient demand, if any (#1)
  #* index2 --> biosolid surplus if it exists
  #* unit: kg nutrient yr-1
  #* 

  rem_nutrient_req = select_RemNutrientRequirements_manure_method_approach(nutrient, main_param, param, manure_surplus_fills, manure_method)
  biosolid_crop_app = compute_biosolid_crop_application(nutrient, main_param, param)
  
  yrs = paste0('X', seq(1987,2017))
  biosolid_crop_surplus <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  biosolid_crop_surplus[, yrs] = sapply(yrs, function(x) biosolid_crop_surplus[, x] = 0)
  
  for (j in 4:ncol(rem_nutrient_req)) {
    
    for (i in 1:nrow(rem_nutrient_req)) {
      
      if ( rem_nutrient_req[i,j] < biosolid_crop_app[i,j] ) {
        
        biosolid_crop_surplus[i,j] = round (  biosolid_crop_app[i,j] - rem_nutrient_req[i,j], 1)
        rem_nutrient_req[i,j] = 0
      }
      
      else if ( rem_nutrient_req[i,j] == biosolid_crop_app[i,j] ) {
        
        biosolid_crop_surplus[i,j] = 0
        rem_nutrient_req[i,j] = 0
      }
      else {
        
        rem_nutrient_req[i,j] = round ( rem_nutrient_req[i,j] - biosolid_crop_app[i,j] , 1)
        biosolid_crop_surplus[i,j] = 0
      }
    }
  }
  
  return(list(rem_nutrient_req = rem_nutrient_req, biosolid_crop_surplus = biosolid_crop_surplus ))
  rm(list=c('biosolid_crop_app','yrs'))
}



compute_biosolid_crop_application_rate = function(nutrient, main_param, param, manure_surplus_fills, manure_method, nutrientReq_minus_Biosolid_df) {
  # computes the biosolid crop application rate for a given crop
  # the nutrientReq_minus_Biosolid_df can be specified when updating biosolid crop application rate following biosolid surplus
  # unit: kg nturient yr-1
  
  man_nutrient_req = calc2_nutrientDemand_solid_application(nutrient, main_param, param)[[1]]
  
  if (missing(nutrientReq_minus_Biosolid_df)==TRUE) {
    rem_nutrient_req = crop_nutrientReq_minus_Biosolid(nutrient, main_param, param, manure_surplus_fills, manure_method)[[1]]
  }
  else {
    rem_nutrient_req = nutrientReq_minus_Biosolid_df
  }
  area = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = main_param, pattern = param)

  yrs = paste0('X', seq(1987,2017))
  man_nutrient_req[, yrs] = sapply(yrs, function(x) round( (man_nutrient_req[, x] - rem_nutrient_req[, x] ) / area [, x] , 1))
  man_nutrient_req = data_cleaning(man_nutrient_req)
  
  return(man_nutrient_req)
  rm(list=c('man_nutrient_req','rem_nutrient_req','area','yrs'))
}



allocate_crop_biosolid_1teraction = function(nutrient, manure_method, manure_surplus_fills) {
  #* 1 --> Allocates biosolid nutrient content for the different crops and years
  #* 2 --> Iterates the biosolid surplus (if it exists) following application and stores it into a dataframe
  #* 3 --> Computes biosolid crop application WITHOUT further allocation of the biosolid surplus
  #* unit: kg nutrient yr-1
  
  if (manure_surplus_fills == TRUE) { folder_div = 'With_ManSurplus'} else { folder_div = 'Without_ManSurplus'}
  biosolid_crops = get_sludge_distribution_crops()
  
  # store main dataframe to update biosolid_surplus after crop application
  yrs = paste0('X', seq(1987,2017))
  main_biosolid_surplus = get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  main_biosolid_surplus[, yrs] = sapply(yrs, function(x) main_biosolid_surplus[,x] = 0)
  
  for (i in 1:nrow(biosolid_crops)) {
    
    main_param = biosolid_crops[i, 'Main_crop']
    param = biosolid_crops[i, 'Crop']
    
    crop_rem_nutrient = crop_nutrientReq_minus_Biosolid(nutrient, main_param, param, manure_surplus_fills, manure_method)[[1]]
    biosolid_surplus = crop_nutrientReq_minus_Biosolid(nutrient, main_param, param, manure_surplus_fills, manure_method)[[2]]
    crop_biosolid_rate = compute_biosolid_crop_application_rate(nutrient, main_param, param, manure_surplus_fills, manure_method, crop_rem_nutrient)
    
    export_file(module = 'Nutrients', 
                file = crop_biosolid_rate, 
                filename = param, 
                folder = 'Fertilisation', 
                subfolder = nutrient, 
                subfolderX2 = 'Biosolids_application_rates', 
                subfolderX3 = manure_method, 
                subfolderX4 = folder_div, 
                subfolderX5 = main_param)
    
    # update biosolid surplus
    main_biosolid_surplus[, yrs] = sapply(yrs, function(x) round(main_biosolid_surplus[, x] + biosolid_surplus[, x], 1))
  }
  export_file(module = 'Nutrients', 
              file = main_biosolid_surplus, 
              filename = 'Biosolids_surplus', 
              folder = 'Fertilisation', 
              subfolder = nutrient, 
              subfolderX2 = 'Biosolids_surplus',
              subfolderX3 = manure_method, 
              subfolderX4 = folder_div)
}



loop_crop_biosolid_1teraction_ManureMethod = function(nutrient, manure_surplus_fills) {
  
  method = c('Method 1', 'Method 2')
  sapply(method, function(x) allocate_crop_biosolid_1teraction(nutrient, x, manure_surplus_fills))
}


