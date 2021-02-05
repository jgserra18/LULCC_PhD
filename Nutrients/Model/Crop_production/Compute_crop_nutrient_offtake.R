source('./Main/Global_functions.R')


# 1 - disaggregate crop yields from the AR to the municipality scale

# 2 - loop each crop param and calculate production (in tonnes DM)

# 3 - Calculate crop N and P offtake 
  # DON'T FORGET TO CONVERT P205 OFFTAKE TO P

# call crop C/N ration and calculate C offtake


##  SUPPORT FUNCTIONS ---------------------------------------------------------------------------

get_crop_yields <- function(main_param, param) {
  
  if (param == 'Other_dried_pulses') {
    
    AR_yields <-  get_activity_data(module = 'Nutrients', folder = 'Raw_data_Agrarian', subfolder = 'Yields', subfolderX2 = main_param, pattern = 'Beans')
  }
  else if (param == 'other_fresh') {
    
    AR_yields <-  get_activity_data(module = 'Nutrients', folder = 'Raw_data_Agrarian', subfolder = 'Yields', subfolderX2 = main_param, pattern = 'Cherry')
  }
  else {
    AR_yields <-  get_activity_data(module = 'Nutrients', folder = 'Raw_data_Agrarian', subfolder = 'Yields', subfolderX2 = main_param, pattern = param)
  }
  return(AR_yields)
}

get_spatially_disaggagregated_yields <- function(main_param, param) {
  # spatially disaggregates crop yields into the municipality level
  # format: Muni_ID, Muni, ID, 1987, ....
  
  # get AT yields
  # specification: Other_dried_pulses --> Beans
  AR_yields <- get_crop_yields(main_param, param)
  names(AR_yields)[1] <- 'agrarian_region_id'
  
  # spatially disaggregate 
  disagg_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Spatial_disaggregation')
  AR_yields <- plyr::join(x = disagg_df, y = AR_yields, by = 'agrarian_region_id')
  AR_yields <- AR_yields[, -c(seq(4,8))]
  
  return(AR_yields)
  rm(list='disagg_df')
}


find_crop_variable <- function(df, param_col, param, var) {
  # var is the variable to subset the dataset
  # e.g., find_crop_variable(df = FRAC_DM, param_col = 'crop', param = 'Oat', var = 'DM_frac')
  
  find_crop <- df[which(df[, param_col]==param), ] 
  select_var <- find_crop[, var]
  return(select_var)
  rm(list='find_crop')
}




## COMPUTE DRY-MATTER PRODUCTION ---------------------------------------------------------------------------


compute_crop_DM_production <- function(main_param, param) {
  # computes the dry-matter production for a given crop
  # unit: tonnes DM yr-1
  
  
  # get data ------------------------------------------------
  FRAC_DM <- get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Crops', subfolderX2 = 'Offtake', pattern = 'DM_content')
  FRAC_DM <- find_crop_variable(df = FRAC_DM, param_col = 'crop', param = param, var = 'DM_frac')
  
  areas <- get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = main_param, pattern = param)
  yields <- get_spatially_disaggagregated_yields(main_param, param)
  
  # compute DM production (in kg DM yr-1)
  yrs <- paste0('X', seq(1987,2017))
  Prod_DM <- yields
  Prod_DM[, yrs] <- sapply(yrs, function(x) round(yields[, x] * areas[, x] * FRAC_DM / 1000, 1))
  
  return(Prod_DM)
  rm(list=c('FRAC_DM','areas','yields','yrs'))
}

## COMPUTE CROP NUTRIENT OFFTAKE OF DRY MATTER PRODUCTION ---------------------------------------------------------------------------

convert_offtake_P2O5_to_P <- function() {
  # conversion factor : P2O5 to P = 0.4364
  
  P2O5_offtake <- get_activity_data(module = 'Nutrients', folder = 'Nutrient_params', subfolder = 'P', subfolderX2 = 'Crops',subfolderX3 = 'Offtake', pattern = 'offtake')
  P_offtake <- P2O5_offtake
  P_offtake[,3] <- P_offtake[,3] * 0.4364
  
  return(P_offtake)
  rm(list='P2O5_offtake')
}


compute_nutrient_offtake <- function(main_param, param, nutrient) {
  # calculates the nutrient offtake of a given crop
  # unit: kg Nutrient yr-1 
  
  
  print(paste0('===========Computing ', nutrient, ' offtake for ', param))
  Prod_DM <- compute_crop_DM_production(main_param, param)
  
  # select appropriate offtake coeficcient
  if (nutrient == 'N') {
    
    nutrient_offtake <- get_activity_data(module = 'Nutrients', folder = 'Nutrient_params', subfolder = nutrient, subfolderX2 = 'Crops', subfolderX3 = 'Offtake', pattern = 'offtake')
  }
  else if (nutrient == 'P') {
    
    nutrient_offtake <- convert_offtake_P2O5_to_P()
  }
  else {
    
    nutrient_offtake <- get_activity_data(module = 'Nutrients', folder = 'Nutrient_params', subfolder = nutrient, subfolderX2 = 'Crops', subfolderX3 = 'Offtake', pattern = 'offtake')
  }
  
  # select nutrient offtake for the crop
  # Unit: kg Nutrient ton DM-1 yr-1
  nutrient_offtake_var <- find_crop_variable(df = nutrient_offtake, param_col = 'crop', param = param, var = 'Avg_offtake')
  
  yrs <- paste0('X', seq(1987,2017))
  nutrient_offtake <- Prod_DM
  nutrient_offtake[, yrs] <- sapply(yrs, function(x) round(nutrient_offtake_var * Prod_DM[, x], 1))
  
  return(nutrient_offtake)
  rm(list=c('Prod_DM','yrs'))
}


compute_all_crop_nutrient_offtake <- function(nutrient) {
  # computes nutrient offtake for each crop for the specified nutrient (P,C,N)
  # unit: kg nutrient yr-1
  
  standard_params <- get_activity_data(module = 'Nutrients', folder = 'General_params', pattern = 'Params_list')

  param_col <- 'Crop'
  main_param_col <- 'Main_crop'
  
  
  for (i in 1:nrow(standard_params)) {
    
    main_param <- standard_params[i, main_param_col]
    param <- standard_params[i, param_col]
    
    if (main_param != 'Pastures') {
      
      param_interpol <- compute_nutrient_offtake(main_param, param, nutrient)
      export_file(module = 'Nutrients', 
                  file = param_interpol, 
                  filename = param, 
                  folder = 'Crop_offtake', 
                  subfolder = nutrient, 
                  subfolderX2 = main_param)
    }

  }
  rm(list=c('standard_params','param_col','main_param_col','param_interpol'))
}





# COMPUTE PASTURE NUTRIENT OFFTAKE ------------------------------------------------------------------


compute_mean_pasture_productivity_municipality = function() {
  # compute avg grassland productivity at the municipality scale
  # grass yield drived from climatic zones 
  # https://datashare.is.ed.ac.uk/handle/10283/3091
  # Metzger, Marc J. (2018). The Environmental Stratification of Europe, [dataset]. University of Edinburgh. https://doi.org/10.7488/ds/2356.
  # unit: kg DM ha-1 yr-1
  
  grass_yield = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Crops', subfolderX2 = 'Grass_yield', pattern = 'ENV_yields')
  pt_muni = get_activity_data(module = 'LULCC', folder = 'Admin', pattern = 'Municipality.shp')
  
  pt_muni$grass_yield = round(exactextractr::exact_extract(grass_yield, pt_muni, 'mean'), 1)
  
  df = as.data.frame(pt_muni)
  df = df[, c('Admin_id','grass_yield')]
  names(df)[1] = 'Muni_ID'
  
  muni_yield <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  muni_yield = plyr::join(muni_yield, df, 'Muni_ID')
  
  return(muni_yield)
  rm(list=c('grass_yield','pt_muni','df'))
}


compute_pasture_nutrient_offtake = function(main_param = 'Pastures', param, nutrient) {
  # computes the nutrient offtake following pasture production
  # nut_prod = yield * FRAC_DM * nut_content
  # unit: kg N-P yr-1
  
  
  if (param != 'Intensive_pasture' & param != 'Extensive_pasture') {
    stop('Only pastures!')
  }
  else {
    
    FRAC_DM = 0.9
    grass_yield = compute_mean_pasture_productivity_municipality()
    area = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = main_param, pattern = param)
    
    # select appropriate offtake coeficcient
    if (nutrient == 'N') {
      
      nutrient_offtake <- get_activity_data(module = 'Nutrients', folder = 'Nutrient_params', subfolder = nutrient, subfolderX2 = 'Crops', subfolderX3 = 'Offtake', pattern = 'offtake')
    }
    else if (nutrient == 'P') {
      
      nutrient_offtake <- convert_offtake_P2O5_to_P()
    }
    
    # select nutrient offtake for the crop
    # Unit: kg Nutrient ton DM-1 yr-1
    nutrient_offtake_var <- find_crop_variable(df = nutrient_offtake, param_col = 'crop', param = param, var = 'Avg_offtake')
    nutrient_offtake_var
    
    yrs = paste0('X',seq(1987,2017))
    nut_offtake = area
    nut_offtake[, yrs] = sapply(yrs, function(x) round(grass_yield[, 'grass_yield']/1000 * area[,x] * nutrient_offtake_var * FRAC_DM, 1))
    
    return(nut_offtake)
    rm(list=c('grass_yield','area','nutrient_offtake','nutrient_offtake_var'))
  }
}



loop_pastures_nutrient_flows = function() {
  
  nutrients = c('N','P')
  params = c('Intensive_pasture','Extensive_pasture')
  
  for (nut in nutrients) {
    for (param in params) {
      
      nut_flow = compute_pasture_nutrient_offtake(main_param = 'Pastures', param = param, nutrient = nut)
      export_file(module = 'Nutrients', 
                  file = nut_flow, 
                  filename = param, 
                  folder = 'Crop_offtake', 
                  subfolder = nut, 
                  subfolderX2 = 'Pastures')
    }
  }
}


# sum each main param -------------------------------------------------------------------------

compute_total_nutrient_offtake_mainParam <- function(nutrient) {
  
  
  standard_params <- get_standard_params_list(main_param = 'Crops')
  
  main_params = standard_params[, 1]
  
  for (main_param in main_params) {
    
    yrs <- paste0('X', seq(1987,2017))
    store_main_param <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
    store_main_param[, yrs] <- sapply(yrs, function(x) store_main_param[,x] <- 0)
    
    sb_df = subset(standard_params, Main_crop == main_param)
    
    for (i in 1:nrow(sb_df)) {
      
      param <- sb_df[i,2]
      crop_offtake <- get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Crop_offtake', subfolder = nutrient, subfolderX2 = main_param, pattern = param)
      store_main_param[, yrs] = sapply(yrs, function(x) round(store_main_param[,x] + crop_offtake[,x], 0))
    }
    export_file(module = 'Nutrients', 
                file = store_main_param, 
                filename = main_param, 
                folder = 'Crop_offtake', 
                subfolder = nutrient, 
                subfolderX2 = 'Total')
    
  }
}



## TOTALS -------------------------


compute_total_nutrient_offtake <- function(nutrient) {
  
  yrs <- paste0('X', seq(1987,2017))
  store_main_param <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  store_main_param[, yrs] <- sapply(yrs, function(x) store_main_param[,x] <- 0)
  
  standard_params <- get_standard_params_list(main_param = 'Crops')
  
  for (i in 1:nrow(standard_params)) {
    
    main_param <- standard_params[i, 1]
    param <- standard_params[i,2]
    crop_offtake <- get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Crop_offtake', subfolder = nutrient, subfolderX2 = main_param, pattern = param)
    store_main_param[, yrs] <- sapply(yrs, function(x) round(store_main_param[,x] + crop_offtake[,x], 1))
  }
  export_file(module = 'Nutrients', 
              file = store_main_param, 
              filename = 'Total_sum', 
              folder = 'Crop_offtake', 
              subfolder = nutrient, 
              subfolderX2 = 'Total')
}



loop_nutrient_offtake <- function() {
  
  nutrient <- c('N','P','C')
  sapply(nutrient, function(x) compute_all_crop_nutrient_offtake(x) )
}





# AGGREGATE NUTRIENT FLOWS PER REERENCE AREA ------------------------------------------


compute_totals_nutrient_offtake = function(crop_type = 'Fodder', nutrient = 'N') {
  
  yrs  = paste0('X',seq(1987,2017))
  store <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  store[, yrs] <- sapply(yrs, function(x) store[,x] <- 0)
  
  standard_params <- get_standard_params_list(main_param = 'Crops')
  
  if (crop_type == 'Fodder') {
    main_crops= c('Forage','Pastures')
    standard_params = subset(standard_params, standard_params==main_crops)
  }
  else {
    
    main_crops =  c('Cereals','Horticulture','Industry_crops','Potato', 'Pulses')
    standard_params = subset(standard_params, Main_crop %in% main_crops)
  }
  for (i in 1:nrow(standard_params)) {
    
    main_param = standard_params[i, 1]
    param = standard_params[i, 2]
    res_flows = get_activity_data(module = 'Nutrients',mainfolder = 'Output', folder = 'Crop_offtake', subfolder = nutrient, subfolderX2 = main_param,  pattern = param) 
    store[, yrs] = sapply(yrs, function(x) round(store[,x] + res_flows[,x], 1))
  }
  
  return(store)
}


compute_total_crop_offtake_flows_referenceArea = function(crop_type = 'Fodder', 
                                                           reference_area = 'Cropland',
                                                           nutrient = 'N') {
  # could be improved (from Compute_crop_residues.R)
  # unit: kg N-P yr-1
  
  yrs  = paste0('X',seq(1987,2017))
  store <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  store[, yrs] <- sapply(yrs, function(x) store[,x] <- 0)
  
  standard_params <- get_standard_params_list(main_param = 'Crops')
  
  if (crop_type == 'Fodder') {
    
    main_crops= ifelse(reference_area == 'Cropland', c('Forage','Pastures'),  c('Pastures'))
    standard_params = subset(standard_params, standard_params==main_crops)
  }
  else {
    
    main_crops =  c('Cereals','Horticulture','Industry_crops','Potato', 'Pulses')
    standard_params = subset(standard_params, Main_crop %in% main_crops)
  }
  
  for (i in 1:nrow(standard_params)) {
    
    main_param = standard_params[i, 1]
    param = standard_params[i, 2]
    
    if (crop_type == 'Fodder' & reference_area == 'Cropland' & param == 'Extensive_pasture') { 
      next
    }
    else if (crop_type == 'Fodder' & reference_area =='Grassland' & param == 'Intensive_pasture') {
      next 
    }
    else {
      print(param)
      res_flows = get_activity_data(module = 'Nutrients',mainfolder = 'Output', folder = 'Crop_offtake', subfolder = nutrient, subfolderX2 = main_param,  pattern = param) 
      store[, yrs] = sapply(yrs, function(x) round(store[,x] + res_flows[,x], 1))
    }
  }
  
  return(store)
  rm(list=c('main_param','param','res_flows','main_crops'))
}


