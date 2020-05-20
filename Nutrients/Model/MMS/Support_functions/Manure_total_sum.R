source('./Nutrients/Model/MMS/Gross_manure/Compute_Nutrient_excretion.R')


compute_manure_type_total_flows('Gross_manure','N','Transported_IntraMunicipality','Slurry')
compute_manure_type_total_flows <- function(main_folder, nutrient, pathway, manure_type) {
  
  standard_params <- get_standard_params_list(main_param = 'Animals')
  yrs <- paste0('X', seq(1987,2017))
  
  main_params <- unique(standard_params$Main_animals)
  
  store_main_param <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  store_main_param[, yrs] <- sapply(yrs, function(x) store_main_param[,x] <- 0)
  
  for (i in main_params) {
    
    animal_rows <- which(standard_params[, 'Main_animals'] == i)
    params <- standard_params[animal_rows, 'Animals']
    
    store_param <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
    store_param[, yrs] <- sapply(yrs, function(x) store_param[,x] <- 0)
    
    
    for (z in params) {
      # add each dataframe within each main_param in a list
      # calculate total sum for a given main_param
      man_flow <- get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = main_folder, subfolder = nutrient, subfolderX2 = pathway, subfolderX3 = manure_type, subfolderX4 = i, pattern = z)
      store_param[, yrs] <- sapply(yrs, function(x) man_flow[, x] + store_param[,x])

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
              subfolderX3 = manure_type,
              subfolderX4 = 'Total')
}




compute_total_nutrient_manure_flows <- function(main_folder, nutrient, pathway) {
  #main_folder is e.g., Gross_manure
   
  man_type <- c('Solid','Slurry')
  standard_params <- get_standard_params_list(main_param = 'Animals')
  yrs <- paste0('X', seq(1987,2017))
  
  main_params <- unique(standard_params$Main_animals)
  
  store_main_param <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  store_main_param[, yrs] <- sapply(yrs, function(x) store_main_param[,x] <- 0)
  
  for (i in main_params) {
    
    animal_rows <- which(standard_params[, 'Main_animals'] == i)
    params <- standard_params[animal_rows, 'Animals']
    
    store_param <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
    store_param[, yrs] <- sapply(yrs, function(x) store_param[,x] <- 0)
    
    
    for (z in params) {
      print(z)
      
      for (man in man_type) {
        print(paste0('Calculating for ', man))
        # add each dataframe within each main_param in a list
        # calculate total sum for a given main_param
        man_flow <- get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = main_folder, subfolder = nutrient, subfolderX2 = pathway, subfolderX3 = man, subfolderX4 = i, pattern = z)
        store_param[, yrs] <- sapply(yrs, function(x) man_flow[, x] + store_param[,x])
      }
    }
    # export sum of param within main_param
    export_file(module = 'Nutrients', 
                file = store_param, 
                filename = i, 
                folder = main_folder, 
                subfolder = nutrient,
                subfolderX2 = pathway, 
                subfolderX3 = 'Total')
    
    # store sum of main_param in main_param_df
    store_main_param[, yrs] <- sapply(yrs, function(x) store_main_param[, x] + store_param[,x])
  }
  # export sum of main_param
  export_file(module = 'Nutrients', 
              file = store_main_param, 
              filename = 'Total_sum', 
              folder = main_folder, 
              subfolder = nutrient,
              subfolderX2 = pathway, 
              subfolderX3 = 'Total')
}


loop_total_manure_nutrient_flows <- function(nutrient) {
  
  pathways <- c('Gross_spreading','Housing','Net_spreading','River_discharge','Storage','Total_Nexcretion','Transported_IntraMunicipality')
  sapply(pathways, function(x) compute_total_nutrient_manure_flows(main_folder = 'Gross_manure', nutrient = nutrient, pathway = x))
}

