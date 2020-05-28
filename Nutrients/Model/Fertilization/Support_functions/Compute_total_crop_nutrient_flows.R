source('./Main/Global_functions.R')



compute_crop_totals_mainland <- function(main_folder, nutrient, pathway) {
  
  standard_params <- get_standard_params_list(main_param = 'Crops')
  yrs <- paste0('X', seq(1987,2017))
  
  main_params <- unique(standard_params$Main_crop)
  
  store_main_param <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  store_main_param[, yrs] <- sapply(yrs, function(x) store_main_param[,x] <- 0)
  
  for (i in main_params) {
    
    main_param_rows <- which(standard_params[, 'Main_crop'] == i)
    params <- standard_params[main_param_rows, 'Crop']
    
    store_param <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
    store_param[, yrs] <- sapply(yrs, function(x) store_param[,x] <- 0)
    
    
    for (z in params) {
      # add each dataframe within each main_param in a list
      # calculate total sum for a given main_param
      if (z == 'Extensive_pasture') {
        break
      }
      else {
        
        nutrient_flows <- get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = main_folder, subfolder = nutrient, subfolderX2 = pathway,  subfolderX3 = i, pattern = z)
        store_param[, yrs] <- sapply(yrs, function(x) nutrient_flows[, x] + store_param[,x])
      }
    }
    # store sum of main_param in main_param_df
    store_main_param[, yrs] <- sapply(yrs, function(x) store_main_param[, x] + store_param[,x])
  }
  export_file(module = 'Nutrients', 
              file = store_main_param, 
              filename = 'Total_sum', 
              folder = main_folder, 
              subfolder = nutrient,
              subfolderX2 = pathway, 
              subfolderX3 = 'Total')
}


