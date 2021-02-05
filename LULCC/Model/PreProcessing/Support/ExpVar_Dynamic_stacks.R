library(doParallel)




# define time period --------------------------------------------------------------------------

yrs = paste0('X',seq(1987,2017))
spatial_res = '500'


# total crop areas -----------------------------------------------------------------------------

set_LU_crops = function(LU_name) {
  
  if (LU_name == 'Rice') { crops = 'Rice' }
  else if (LU_name == 'Vineyards') { crops = 'Vineyard' }
  else if (LU_name== 'Olive_groves') { crops = 'Olive_grove' }
  else if (LU_name == 'Fruit_trees') { crops = c('Citrus','Dried_nuts','Fresh_fruits') }
  else if (LU_name == 'Permanently_irrigated') { crops = c('Oat','Irrigated_potato','Irrigated_maize','Horticulture','Intensive_pasture','Forage', 'Industry_crops','Pulses') }
  else if (LU_name == 'Pastures' | LU_name == 'Natural_grasslands' | LU_name == 'AgroForestry') { crops = 'Extensive_pasture' }
  else if (LU_name == 'Non_irrigated') {crops = c('Rye','Barley','Wheat','Pulses','Triticale')}
  
  return(crops)
}


get_all_crop_area_files = function(LU_name) {
  
  if (LU_name != 'Permanently_irrigated') {
    all_crops = list.files(path = './Nutrients/Activity_data/Correct_data_Municipality/Areas/', full.names = T, recursive = T)
  }
  else {
    all_crops = list.files(path = './Nutrients/Activity_data/Correct_data_Municipality/Irrigation/Crop_irrigated_areas/Total/', full.names = T, recursive = T)
  }
  
  return(all_crops)
}

subset_LU_crop_files = function(LU_name) {
  # store into a list the path of crop areas for a given LU
  crops = set_LU_crops(LU_name)
  all_crops = get_all_crop_area_files(LU_name)
  
  store = list()
  for (crop_name in crops) {
    
    crop = subset(all_crops, grepl(crop_name, all_crops)==TRUE)
    store = append(store, crop)
  }
  
  return(unlist(store))
  rm(list=c('crops','all_crops','crop'))
}

sum_LU_crop_files = function(LU_name) {
  # sum the total crop areas for a given LU
  # unit: ha yr-1
  
  # create calculation df for the ith management practice
  store_manag <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  store_manag[, yrs] <- sapply(yrs, function(x) store_manag[,x] <- 0)
  
  crops_LU = subset_LU_crop_files(LU_name)
  crops_LU = lapply(crops_LU, read.csv)
  
  for (i in 1:length(crops_LU)) {
    
    store_manag[, yrs] = store_manag[,yrs] + crops_LU[[i]][, yrs]
  }
  
  return(store_manag)
  rm(list=c('crops_LU'))
}


create_LU_timestep_stack = function(LU_name, write=FALSE) {
  # creates a raster stack for each timestep (1987,2017)
  # to be fed to the ObsLUstack
  # unit: hectares
  
  crop_sum_LU = sum_LU_crop_files(LU_name)
  names(crop_sum_LU)[1] = 'Admin_id'
  store = list() # prepare list to stack data
  
  cl = makePSOCKcluster(2)
  registerDoParallel(cl)
  
  store = foreach(yr=yrs, .combine = append, 
                  .packages = c('fasterize','sf','raster'), 
                  .export = c('fasterize_admin_statistical', 'get_activity_data', 
                              'get_mainfolder_sub', 'identify_read_fileclass')) %dopar% {
    
                    yr_file = fasterize_admin_statistical(module = 'LULCC', admin_res = 'Municipality', file = crop_sum_LU, name_id = 'Admin_id', name_field = yr, spatial_res = 500) # ha
                    append(store, yr_file)
                  }
  stopCluster(cl) 
  store = stack(store)
  names(store) = paste0(LU_name, '_', seq(1:length(yrs)))
  
  
  if (write==TRUE) {
    export_file(module = 'LULCC', folder = 'Exploratory_variables', subfolder = 'Dynamic_stack', filename = paste0(LU_name,'.tif'), file = store)
  }
  else {
    return(store)
  }
  rm(list=c('crop_sum_LU','yr_file'))
}


loop_LU_dynamic_stacks = function() {
  
  LU_names = c('Rice','Vineyards','Olive_groves','Fruit_trees','Permanently_irrigated','Pastures', 'Non_irrigated')
  sapply(LU_names, function(x) create_LU_timestep_stack(LU_name = x, write = T))
}



