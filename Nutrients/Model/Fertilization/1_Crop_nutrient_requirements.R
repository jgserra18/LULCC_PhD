source('./Nutrients/Model/Crop_production/Compute_crop_nutrient_offtake.R')

## GENERAL PURPOSE ---------------------------------------------------------------------------------- 

get_fertiliser_modifier <- function(nutrient, main_param, param) {
  # note: this is the equivalent to the crop nutrient offtake 
  # unit: kg N ton DM-1 yr-1
  
  ifelse(nutrient == 'N', 
         fert_modifier <- get_activity_data(module = 'Nutrients', folder = 'Nutrient_params', subfolder = nutrient, subfolderX2 = 'Crops', subfolderX3 = 'Offtake', pattern = 'offtake'),
         fert_modifier <-  convert_offtake_P2O5_to_P())
  fert_modifier <- find_crop_variable(df = fert_modifier, param_col = 'crop', param = param, var = 'Avg_offtake')
  
  return(fert_modifier)
}

## YIELD DIFFERENCES ---------------------------------------------------------------------------------- 

get_spatially_disaggagregated_reference_yields <- function(main_param, param) {
  
  ref_yield <- get_activity_data(module = 'Nutrients', folder = 'Nutrient_params', subfolder = 'N', subfolderX2 = 'Crops', subfolderX3 = 'Fertilization', subfolderX4 = 'Reference_Yields', pattern = main_param)
  names(ref_yield)[1:2] <- c('agrarian_region','agrarian_region_id')
  
  ref_yield <- ref_yield[, c('agrarian_region_id', param)]
  
  # spatially disaggregate 
  disagg_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Spatial_disaggregation')
  ref_yield <- plyr::join(x = disagg_df, y = ref_yield, by = 'agrarian_region_id')
  ref_yield <- ref_yield[, -c(seq(4,8))]
  
  return(ref_yield)
}


compute_yield_difference <- function(main_param, param) {
  # unit: tonnes FM ha-1 yr-1
  
 # FRAC_DM <- get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Crops', subfolderX2 = 'Offtake', pattern = 'DM_content')
 # FRAC_DM <- find_crop_variable(df = FRAC_DM, param_col = 'crop', param = param, var = 'DM_frac')
  
  crop_yield <- get_spatially_disaggagregated_yields(main_param, param)
  ref_yield <- get_spatially_disaggagregated_reference_yields(main_param, param)[1, param]

  # compute DM production (in kg DM yr-1)
  yrs <- paste0('X', seq(1987,2017))
  
  for (i in 1:nrow(crop_yield)) {
    
    for (j in 4:ncol(crop_yield)) {

      if ((crop_yield[i,j] > 0)==TRUE) {
        crop_yield[i,j] <- round( (crop_yield[i,j] - ref_yield) / 1000, 1)
      }
    }
  }
  
  return(crop_yield)
  rm(list=c('FRAC_DM','ref_yield'))
}


general_func_crop_fertiliser_rate <- function(nutrient, main_param, param) {
  # computes crop fertiliser rates updated based on yield differences
  # updated fert rate = rec fert rate + (nutrient uptake * yield dif)
  # unit: kg nutrient ha-1 yr-1
  
  yield_dif <- compute_yield_difference(main_param, param)
  
  fert_modifier <-  get_activity_data(module = 'Nutrients', folder = 'Nutrient_params', subfolder = nutrient, subfolderX2 = 'Crops', subfolderX3 = 'Fertilization', subfolderX4 = 'Fertiliser_modifier', pattern = main_param)
  fert_modifier <- fert_modifier[1, param]
  
  rec_fertiliser <-  get_activity_data(module = 'Nutrients', folder = 'Nutrient_params', subfolder = nutrient, subfolderX2 = 'Crops', subfolderX3 = 'Fertilization', subfolderX4 = 'Recommended_fertiliser', pattern = main_param)
  rec_fertiliser <- rec_fertiliser[1, param]
  
  yrs <- paste0('X', seq(1987,2017))
  crop_fert <- yield_dif
  
  crop_fert[, yrs] <- sapply(yrs, function(x) round(rec_fertiliser + fert_modifier * yield_dif[,x], 1))
  
  if (identical(integer(0), which(crop_fert<0))==FALSE) {
    
    # if pulses are negative, correct to 0 ;; otherwise, it may be an error
      
    neg_ids <- which(crop_fert<0, arr.ind = TRUE)
    
    for (i in 1:nrow(neg_ids)) {
      
      r <- neg_ids[i,1]
      c <- neg_ids[i, 2]
      
      crop_fert[r,c] <- 0
    }
   print('Something fishy.')
  
  }
  
  return(crop_fert)
  rm(list=c('yield_dif','fert_modifier','rec_fertiliser','yrs'))
}

## fertilization specification (Pastures, Horticulture) --------------------------------------

compute_crop_fertiliser_rate <- function(nutrient, main_param, param) {
  # computes fertiliser rates based on yield differences and crop specifications
  # unit: kg N ha-1 yr-1
  
  
  if (param == 'Horticulture'| (main_param == 'Pastures' & param == 'Intensive_pasture')) {
    
    yrs <- paste0('X', seq(1987,2017))
    df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
    df[, yrs] <- sapply(yrs, function(x) df[,x] <- 0)
    
    rec_fert <-  get_activity_data(module = 'Nutrients', folder = 'Nutrient_params', subfolder = nutrient, subfolderX2 = 'Crops', subfolderX3 = 'Fertilization', subfolderX4 = 'Recommended_fertiliser', pattern = main_param)
    df[, yrs] <- sapply(yrs, function(x) df[,x] <- rec_fert[1, param])
    crop_fert <- df
  }
  
  else if (main_param == 'Pastures' & param == 'Extensive_pasture') {
    
     next 
  }
  else {
    
    crop_fert <- general_func_crop_fertiliser_rate(nutrient, main_param, param)
  }
  
  return(crop_fert)
}



loop_crop_fertiliser_rates <- function(nutrient) {
  
  standard_params <- get_standard_params_list('Crops')
  standard_params
  for (i in 1:nrow(standard_params)) {
    
    main_param <- standard_params[i, 'Main_crop']
    param <- standard_params[i, 'Crop']
    
    if (param == 'Extensive_pasture') {
      next 
    }
    else {
      
      crop_fert_rate <- compute_crop_fertiliser_rate(nutrient, main_param, param)
      export_file(module = 'Nutrients', 
                  folder = 'Activity_data',
                  subfolder = 'Nutrient_params', 
                  subfolderX2 = nutrient, 
                  subfolderX3 = 'Crops', 
                  subfolderX4 = 'Fertilization', 
                  subfolderX5 = 'Updated_fertiliser_rates', 
                  subfolderX6 = main_param,
                  filename = param, 
                  file = crop_fert_rate)
    }
  }
}


# CROP NUTRIENT REQUIREMENTS -----------------------------------------------------

compute_crop_nutrient_requirements <- function(nutrient, crop_area = NULL) {
  # computes total nutrient requirements for each crop
  # unit: kg nutrient yr-1
  
  standard_params <- get_standard_params_list('Crops')
  
  for (i in 1:nrow(standard_params)) {
    
    main_param <- standard_params[i, 'Main_crop']
    param <- standard_params[i, 'Crop']
    
    if (param == 'Extensive_pasture') {
      next 
    }
    else {
      
      crop_fert_rate <- compute_crop_fertiliser_rate(nutrient, main_param, param)
      crop_area <- get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = main_param, pattern = param)
      
      yrs <- paste0('X', seq(1987,2017))
      crop_area[,yrs] <- sapply(yrs, function(x) round(crop_area[,x] * crop_fert_rate[,x], 1))
      export_file(module = 'Nutrients', 
                  file = crop_area, 
                  filename = param, 
                  folder = 'Fertilisation', 
                  subfolder = nutrient, 
                  subfolderX2 = 'Crop_requirements', 
                  subfolderX3 = main_param)
    }
  }
}
