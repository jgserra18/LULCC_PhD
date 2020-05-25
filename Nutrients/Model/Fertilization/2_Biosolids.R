source('./Main/Global_functions.R')

# LINEARLY INTERPOLATED SLUDGE BIOMASS DATA --------------------------------------------------------------------------------------------------- 

linearly_interpolate_sludge_biomass_municipality <- function(existing_years = seq(2006,2017), xout = seq(1987,2005)) {
  # this linearly interpolates sludge data from 2006-2017 to 1987-2005
  # NOTE: NOT CALIBRATED BASED ON NATIONAL DATA
  # gets the data of sludge amount for a given subfolder_name (i.e., Municipality)
  # unit: tonnes dm yr-1 (Municipality) 
  

  # 2006 - 2017, data from APA
  sludge_muni <- get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Crops', subfolderX2 = 'Fertilisers', subfolderX3 = 'Biosolids', pattern = 'Sludge_municipality')
  
  new_file <- sludge_muni[, -c(1,2,3)]
  names(new_file) <- gsub('X','',names(new_file))
  store <- sludge_muni[, c(1,2,3)]
  
  calc_cols <- paste0('X', seq(1987,2017))
  store[, calc_cols] <- sapply(calc_cols, function(x) store[,] <- NA)
  
  populate_cols <- paste0('X', seq(2006,2017))
  store[, populate_cols] <- sapply(populate_cols, function(x) store[,x] <- sludge_muni[,x])
  
  for (i in 1:nrow(new_file)) {
    
    calc_df <- data.frame(y = c(new_file[i,1], new_file[i,2], new_file[i,3]), x = seq(2006,2017))
    lm_model <- lm(y~x, calc_df)
    lm_prediction <- round(
      predict(lm_model, newdata =  data.frame(x =  seq(1987,2005))), 2)
    
    names(lm_prediction) <- paste0('X',  seq(1987,2005))
    lm_prediction <- ifelse(lm_prediction<0, 0, round(lm_prediction, 2))
    store[i, names(lm_prediction)] <- lm_prediction
  }
  
  return(store)
  rm(list=c('sludge_muni','new_file','calc_cols', 'populate_cols', 'calc_df', 'lm_model', 'lm_prediction'))
}





# CALIBRATE SLUDGE NUTRIENT CONTENT DATA --------------------------------------------------------------------------------------------------- 

calibrate_sludge_N_municipality <- function() {
  # this is based on APA (2017) sludge N content of 0.0363 kg N kg dm-1
  # calibrates sludgeN based on uncalibrated and interpolated sludge biomass
  # this will be used to convert the sludge to PHOSPHORUS and CARBON subsequently
  # unit: tonnes N yr-1
  
  unit_conversion = 0.0363 # kg N kg DM-1
  
  sludge_muni = linearly_interpolate_sludge_biomass_municipality()
  sludgeN_mainland = get_activity_data(module = 'Nutrients', folder = 'Nutrient_params', subfolder = 'N', subfolderX2 = 'Fertilisers', subfolderX3 = 'Biosolids', pattern = 'Sludge_national')
  
  # sum and convert biomass to kg N -----
  yrs = paste0('X', seq(1987,2017))
  sludge_muni[, yrs] = sapply(yrs, function(x) round(sludge_muni[, x] * unit_conversion, 1))
  
  # calibrate sludge_muni (kg N) --------
  for (i in yrs) {
    
    uncalibrated_sumN = sum(sludge_muni[, i])
    sumN = sludgeN_mainland[which(sludgeN_mainland[, 1] == i), 2] 
    
    sludge_muni[, i] <- round( sludge_muni[, i] * sumN / uncalibrated_sumN ,1)
  }
  
  return(sludge_muni)
  rm(list=c('sludge_mainland','yrs','uncalibrated_sumN','sumN'))
}


compute_sludge_P_N_C_content <- function(nutrient) {
  # computes sludge nutrient content for NPC
  # unit: tonnes nutrient yr-1
  
  # kg N kg P-1 or kg N kg C-1 based on Le Noe et al (2016) sludge data for the whole France
  if (nutrient == 'P') { unit_conversion = 1.074074 } else { unit_conversion = 0.1 }
  
  sludgeN_muni = calibrate_sludge_N_municipality()
  
  if (nutrient != 'N') {
    
    yrs = paste0('X', seq(1987,2017))
    sludgeN_muni[, yrs] <- sapply(yrs, function(x) round( sludgeN_muni[, x] / unit_conversion, 1))
  }
 
  return(sludgeN_muni) 
}



# SPREAD SLUDGE FROM URBAN AREAS TO ADJACENT MUNICIPALITIES  --------------------------------------------------------------------------------------------------- 

## algorith will be based on the fraction of arable land with potential application of biosolids 
# (i.e., all crops minus horticulture and industry crops)


subset_df_lisbon_porto <- function(df, merge_col, urban_area) {
  # func to subset the municipakities within AML and AMP
  # used to distribute the sludge produced 
  
  if (urban_area =='Lisbon') { id = '170'} else { id = '11A' }
  disagg_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Spatial_disaggregation')
  
  new_df = plyr::join(df, disagg_df, merge_col)
  new_df = subset(new_df, nuts3_ID == id)
  new_df <- new_df[, -seq(ncol(new_df)-6, ncol(new_df))]
  
  return(new_df)
  rm(list=c('nuts3_id','disagg_df'))
}


compute_arable_land_sludge_application <- function() {
  # computes the potential arable land where biosolids can be applied
  # unit: hectare
  
  standard_params = get_standard_params_list('Crops')
  
  yrs <- paste0('X', seq(1987,2017))
  store <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  store[, yrs] <- sapply(yrs, function(x) store[,x] <- 0)
  
  
  for (i in 1:nrow(standard_params)) {
    
    main_param <- standard_params[i, 'Main_crop']
    param <- standard_params[i, 'Crop']
    
    if (main_param == 'Horticulture' | main_param == 'Industry_crops') {
      break
    }
    else {
      
      yrs <- paste0('X', seq(1987,2017))
      crop_area = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = main_param, pattern = param)
      store[, yrs] = sapply(yrs, function(x) round(store[,x] + crop_area[,x], 1))
    }
  }
  return(store)
}


compute_arable_land_sludge_FRAC_UAA = function(urban_area) {
  # computes the fraction of the potential area to apply biosolids in terms of UAA for each municipality
  # unit: dimensionless
  
  application_area = compute_arable_land_sludge_application()
  application_area = subset_df_lisbon_porto(df = application_area, merge_col = 'Muni_ID', urban_area = urban_area)
  
  yrs <- paste0('X', seq(1987,2017))
  application_area[, yrs] <- sapply(yrs, function(x) round(application_area[,x] / sum(application_area[,x]), 3))
  
  application_area <- data_cleaning(application_area)
  return(application_area)
}



distribute_sludge_lisbon_porto <- function(nutrient, urban_area) {
  # algorithm: distributes the sludge produced in Porto and Lisbon municipalities over the respective NUTS3 region
  # the sludge is distributed according to the fraction of the potential area (arable land - horticultural and industrial crops) comparing to the UAA of the NUTS3
  # updates the amounts of sludge per municipality
  # unit: tonnes nutrient yr-1
  
  if (urban_area == 'Lisbon') { muni_id = '1106' } else { muni_id = '1312' }
  
  # subset the total amount of sludge produced in either Lisbon or Porto
  sludge_muni = compute_sludge_P_N_C_content(nutrient)
  city_sludge = sludge_muni[which(sludge_muni[, 1] == muni_id), ]
  
  # potential application area fraction and subset it to the respective urban area
  FRAC_Parea = compute_arable_land_sludge_FRAC_UAA(urban_area)
  
  # computes the total amounts of sludge to be distributed ----------------------------------------
  add_city_sludge = city_sludge
  yrs <- paste0('X', seq(1987,2017))
  add_city_sludge[, yrs] <- sapply(yrs, function(x) round(FRAC_Parea[, x] * add_city_sludge[, x], 2))
  
  # computes the updated amounts of sludge pr municipality ----------------------------------------
  city_sludge[, yrs] <- sapply(yrs, function(x) round(add_city_sludge[, x] + city_sludge[, x], 1))

  return(city_sludge)
}



compute_updated_distributed_sludge_nutrient_content <- function(nutrient) {
  # distributes the urban sludge to their respective NUTS3 region
  # updates municipality rates to the whole country
  # unit: tonnes nutrient yr-1
  
  
  # creates a dataframe with the updated amounts of sludge in the big main urban areas in Portugal 
  AML_sludge = distribute_sludge_lisbon_porto(nutrient, 'Lisbon')
  AMP_sludge = distribute_sludge_lisbon_porto(nutrient, 'Porto')
  urban_sludge = rbind(AML_sludge, AMP_sludge)
  urban_ids = urban_sludge[, 1]
  
  # calls the old amounts of sluge
  muni_sludge = compute_sludge_P_N_C_content(nutrient)
  
  # updates the new sludge --------------------------------------------- 
  yrs <- paste0('X', seq(1987,2017))
  
  for (i in 1:length(urban_ids)) {
    
    row = which(muni_sludge[, 1] == as.integer(urban_ids[i]))
    
    for (j in yrs) {
      
      muni_sludge[row, j] <- urban_sludge[i, j]
    }
  }
  
  return(muni_sludge)
  rm(list=c('AML_sludge','AMP_sludge','urban_sludge','urban_ids','yrs'))
}




loop_sludge_nutrient_content <- function() {
  # unit: kg nutrient yr-1 
  
  nutrient = c('N','P','C')
  
  for (i in nutrient) {
    
    sludge_nutrient = compute_updated_distributed_sludge_nutrient_content(i)
    
    yrs <- paste0('X', seq(1987,2017))
    sludge_nutrient[, yrs] <- sapply(yrs, function(x) round(sludge_nutrient[, x] * 1000, 1))
    
    export_file(module = 'Nutrients', 
                file = sludge_nutrient, 
                filename = 'Total_sum', 
                folder = 'Fertilisation', 
                subfolder = i, 
                subfolderX2 = 'Biosolids', 
                subfolderX3 = 'Total')
  }
}





