source('Main/Global_functions.R')




# GRAZING NET N RETURNED TO SOIL ----------------------------------------------------------------------------------------------- 

compute_net_grazing_soil = function(nutrient) {
  # returns the total net nutrient amounts to soil from animal grazing
  # it is assumed no P losses while for N NH3 emissions must be excluded
  # unit: kg nutrient yr1-
  
  tot_nut_grazing = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gross_manure', subfolder = nutrient, subfolderX2 = 'Total', subfolderX3 = 'Total', pattern = 'Total')
  
  if (nutrient == 'N') {
    
    tot_nh3_grazing = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gas_N_emissions', subfolder = 'NH3', subfolderX2 = 'Grazing',subfolderX3 = 'Total', subfolderX4 = 'Total', pattern = 'Total_sum')
    
    yrs = paste0('X', seq(1987,2017))
    tot_nut_grazing[, yrs] = sapply(yrs, function(x) round(tot_nut_grazing[, x] - tot_nh3_grazing[, x], 1))
  }
  
  return(tot_nut_grazing)
}




# ORGANIC and INORGNAIC FERTILISERES NET NUTRIENT RETURNED TO SOIL ---------------------------------------------------------------------------

source('./Nutrients/Model/Fertilization/3_1_CropManureAllocation.R_CropManureAllocation.R')
source('./Nutrients/Model/Fertilization/4_BiosolidsAllocation.R')
source('./Nutrients/Model/Soil_losses/Gas_emissions/NH3_other_emissions.R')


compute_crop_net_N_fertilisers = function(main_param, param, fert_type, nutrient = 'N', manure_surplus_fills_nutDemand = F, manure_method = 'Method 1') {
  # cmputes the net N returned to the soil following the application of  fertilisers and NH3 emissions
  # unit: kg N yr-1
  
  if (manure_surplus_fills_nutDemand == TRUE) { folder_div = 'With_ManSurplus'} else { folder_div = 'Without_ManSurplus'}
  
  
  if (fert_type == 'Manure') {
    
    app_rate = 'Manure_application_rates'
    app_nh3 = 'Manure_application'
    crop_app_rate = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Fertilisation',  subfolder = nutrient, subfolderX2 = app_rate, subfolderX3 = manure_method, subfolderX4 = folder_div, subfolderX5 = 'Total', subfolderX6 = main_param, pattern = param)
    crop_app_NH3 =   get_activity_data(module = 'Nutrients', 
                                       mainfolder =  'Output', 
                                       folder = 'Gas_N_emissions', 
                                       subfolder = 'NH3', 
                                       subfolderX2 = app_nh3,
                                       subfolderX3 = manure_method, 
                                       subfolderX4 = folder_div,
                                       subfolderX5 = 'Total', 
                                       subfolderX6 = main_param, 
                                       pattern = param)
  }
  else if (fert_type == 'Inorganic') {
    
    app_rate = 'Fertiliser_application_rates'
    app_nh3 = 'Inorganic_fertiliser'
    crop_app_rate = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Fertilisation',  subfolder = nutrient, subfolderX2 = app_rate, subfolderX3 = manure_method, subfolderX4 = folder_div, subfolderX5 = main_param, pattern = param)
    crop_app_NH3 =   get_activity_data(module = 'Nutrients', 
                                       mainfolder =  'Output', 
                                       folder = 'Gas_N_emissions', 
                                       subfolder = 'NH3', 
                                       subfolderX2 = app_nh3,
                                       subfolderX3 = manure_method, 
                                       subfolderX4 = folder_div,
                                       subfolderX5 = 'Total',
                                       subfolderX6 = main_param, 
                                       pattern = param)
  }
  else {
    
    app_rate = 'Biosolids_application_rates'
    app_nh3 = 'Biosolid_application'
    crop_app_rate = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Fertilisation',  subfolder = nutrient, subfolderX2 = app_rate, subfolderX3 = manure_method, subfolderX4 = folder_div, subfolderX5 = main_param, pattern = param)
    crop_app_NH3 =   get_activity_data(module = 'Nutrients', 
                                       mainfolder =  'Output', 
                                       folder = 'Gas_N_emissions', 
                                       subfolder = 'NH3', 
                                       subfolderX2 = app_nh3,
                                       subfolderX3 = manure_method, 
                                       subfolderX4 = folder_div,
                                       subfolderX5 = main_param, 
                                       pattern = param)
  }

  crop_area =  get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = main_param, pattern = param)

  
  yrs <- paste0('X', seq(1987,2017))
  crop_nutrient = crop_app_rate
  crop_nutrient[, yrs] = sapply(yrs, function(x) round( crop_app_rate[, x] * crop_area[, x] - crop_app_NH3[,x], 1))
  
  return(crop_nutrient)
  rm(list=c('crop_app_rate','crop_area','crop_app_NH3'))
}



compute_total_net_fert_nutrient = function(nutrient, fert_type, manure_surplus_fills_nutDemand = F, manure_method = 'Method 1') {
  

  # store everything
  yrs <- paste0('X', seq(1987,2017))
  store_nut <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  store_nut[, yrs] <- sapply(yrs, function(x) store_nut[,x] <- 0)
  
  # get data according to the organic fertiliser 
  
  if (fert_type == 'Manure' | fert_type == 'Inorganic') {
    
    standard_params = get_standard_params_list(main_param = 'Crops')
  }
  else {
    
    standard_params = get_sludge_distribution_crops()
  }
  
  # loop around all the allocated crops and store in "store_nut
  for (i in 1:nrow(standard_params)) {
    
    main_param = standard_params[i, 1]
    param = standard_params[i, 2]
    
    if (fert_type == 'Manure' & (main_param == 'Pastures' | main_param == 'Forage' | param == 'Tomato' | main_param == 'Horticulture')) {
      next
    }
    else if (fert_type == 'Inorganic' & param == 'Extensive_pasture') {
      next 
    }
    else {
  
      crop_net_N = compute_crop_net_N_fertilisers(main_param, param, fert_type, nutrient, manure_surplus_fills_nutDemand, manure_method)
      store_nut[, yrs] = sapply(yrs, function(x) round (store_nut[,x] + crop_net_N[, x], 1))
    }
  }
  
  return(store_nut)
  rm(list=c('yrs','standard_params','files_path','crop_net_N'))
}


# MASK RUNOFF ACCORDING TO CROPLAND AND GRASSLAND --------------------------------------------------------------------------------------------

source('./Nutrients/Model/GIS_computations/LULCC_agriculture/Build_CroplandGrassland.R')

d = create_mainland_annual_NUTS2_raster_mosaic('500','2000')
cellStats(dd, 'sum') /1000000

grazing_lu = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'LULCC', subfolderX2 = 'Grazing', pattern = 'CLC_grazing')
dd = reclassify(d, as.matrix(grazing_lu))
dd = resample(dd, rf, 'ngb')

rf = raster('./Nutrients/Output/MITERRA_fractions/FRAC_runoff/Runoff_fraction/RF_2016.tif')
rf = rf * dd
rf[rf==0] = NA
require(tmap)
tmap_mode('plot')

tm_shape(rf) + 
  tm_raster(breaks = c(0, 0.0001, 0.01, 0.05, 0.10, 0.15, +Inf), palette = c('grey4','blue1','green1','yellow1','orange1','red1'))
