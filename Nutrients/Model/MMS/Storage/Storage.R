source('./Nutrients/Model/MMS/Housing/Housing.R')




## COMPUTE N/TAN ENTERING STORAGE (SOLID SYSTEMS) -------------------------------------------------------------------------------------------

compute_solid_TAN_entering_storage <- function(main_param, param, manure_type = 'Solid') {
  
  # exit housing TAN = [] ( Housing_TAN - ( Housing_NH3  + N_bedding *  f_imm) ]  * STORE_FYM
  # store_FYM is the fraction of manure stored, opposed to that used as feedstock for biogas
  # because the scarce data available for manure for biogas in Portugal points to its very small fraction (~0.01% mainland up to 1.72 % in one municipality)
  # STORE_FYM = 1 and STORE_FEED = 0
  
  f_imm <- 0.0067 #kg N kg Straw-1
  
  house_maN <-  get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gross_manure', subfolder = 'N', subfolderX2 = 'Housing', subfolderX3 = manure_type, subfolderX4 = main_param, pattern = param)
  house_TAN <- convert_N_to_TAN_dataframe(main_param = main_param, df = house_maN)
  
  house_NH3 <- get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gas_N_emissions', subfolder = 'NH3', subfolderX2 = 'Housing', subfolderX3 = manure_type, subfolderX4 = main_param, pattern = param)
  Straw_bedding <- general_func_animal_bedding(main_param, param, bedding_param = 'Straw')
  
  yrs <- paste0('X', seq(1987,2017))
  
  if (param == 'Non_dairy') {
    house_TAN[, yrs] <- 0
  }
  else {
    house_TAN[, yrs] <- sapply(yrs, function(x) round( house_TAN[, x] - ( house_NH3[,x]) + (Straw_bedding[,x] * f_imm), 1))
  }

  return(house_TAN)
  rm(list=c('f_imm','house_maN','house_NH3','yrs'))
}


compute_solid_N_entering_storage <- function(main_param, param, manure_type = 'Solid') {
  # exit housing N = Housing_N + N_bedding - Housing_NH3
  

  house_maN <-  get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gross_manure', subfolder = 'N', subfolderX2 = 'Housing', subfolderX3 = manure_type, subfolderX4 = main_param, pattern = param)
  house_NH3 <- get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gas_N_emissions', subfolder = 'NH3', subfolderX2 = 'Housing', subfolderX3 = manure_type, subfolderX4 = main_param, pattern = param)
  N_bedding <- general_func_animal_bedding(main_param, param, bedding_param = 'N_bedding')
  
  yrs <- paste0('X', seq(1987,2017))
  
  if (param == 'Non_dairy') {
    house_maN[, yrs] <- 0
  }
  else {
    house_maN[, yrs] <- sapply(yrs, function(x) round( house_maN[, x] +  N_bedding[,x] - house_NH3[,x], 1))
  }

  return(house_maN)
  rm(list=c('house_NH3','N_bedding','yrs'))
}



## COMPUTE N/TAN IN STORAGE (SOLID SYSTEMS) -------------------------------------------------------


set_manure_storage_fraction <- function(param, manure_type) {
  # note: these may have to be worked on
  
  
  X_store = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Animals', subfolderX2 = 'Store_MMS', pattern = manure_type)
  
  if (manure_type == 'Solid') {
    
    
    X_store <- subset(X_store, Animals == param)[, 'X_store']
    return(X_store)
  }
  else {
    
    X_store <- subset(X_store, Animals == param)[, 'X_store']
    return(X_store)
  }
}


compute_solid_TAN_storage <- function(main_param, param, manure_type = 'Solid', manure_spreading) {
  
  TAN_solid_entering  <- compute_solid_TAN_entering_storage(main_param, param, manure_type)
  
  # X_STORE_SOLID
  X_store_solid <- set_manure_storage_fraction(param = param, manure_type = manure_type)
  
  # calculation
  yrs <- paste0('X', seq(1987,2017))
  
  if (missing(manure_spreading) == TRUE) {
    
    TAN_solid_entering[, yrs] <- sapply(yrs, function(x) round(TAN_solid_entering[,x] * X_store_solid, 1))
  }
  else {
    
    TAN_solid_entering[, yrs] <- sapply(yrs, function(x) round(TAN_solid_entering[,x] * (1 - X_store_solid), 1))
  }

  return(TAN_solid_entering)
}


compute_solid_N_storage <- function(main_param, param, manure_type = 'Solid', manure_spreading) {
  
  N_solid_entering  <- compute_solid_N_entering_storage(main_param, param, manure_type)
  
  # X_STORE_SOLID
  X_store_solid <- set_manure_storage_fraction(param = param, manure_type = manure_type)
  
  
  # calculation
  yrs <- paste0('X', seq(1987,2017))
  
  if (missing(manure_spreading) == TRUE) {
    
    N_solid_entering[, yrs] <- sapply(yrs, function(x) round(N_solid_entering[,x] * X_store_solid, 1))
  }
  else {
    
    N_solid_entering[, yrs] <- sapply(yrs, function(x) round(N_solid_entering[,x] * (1 - X_store_solid), 1))
  }

  return(N_solid_entering)
}

  
  
## COMPUTE N/TAN IN STORAGE (LIQUID SYSTEMS) ------------------------------------------------------

# ASSUMPTION -----
# X_STORE_SLURRY AND X_STORE_FYM are the same as the previous Share_MMS

compute_slurry_TAN_storage <- function(main_param, param, manure_type = 'Slurry', manure_spreading) {
  # computes the total amounts of TAN in storage before being applied
  # storage_slurry_TAN = [ (house_slurry_TAN - house_slurry_NH3) + (yard_TAN - yard_NH3)] * X_store_slurry
  
  house_maN <-  get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gross_manure', subfolder = 'N', subfolderX2 = 'Housing', subfolderX3 = manure_type, subfolderX4 = main_param, pattern = param)
  house_TAN <- convert_N_to_TAN_dataframe(main_param = main_param, df = house_maN)
  
  yard_maN <-  get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gross_manure', subfolder = 'N', subfolderX2 = 'Yards', subfolderX3 = 'Total', subfolderX4 = main_param, pattern = param)
  yard_TAN <- convert_N_to_TAN_dataframe(main_param = main_param, df = yard_maN)
  
  # Nh3 emissions
  house_NH3 <- get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gas_N_emissions', subfolder = 'NH3', subfolderX2 = 'Housing', subfolderX3 = manure_type, subfolderX4 = main_param, pattern = param)
  yard_NH3 <- get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gas_N_emissions', subfolder = 'NH3', subfolderX2 = 'Yards', subfolderX3 = 'Total', subfolderX4 = main_param, pattern = param)
  
  # X_STORE_SLURRY
  X_store_slurry <- set_manure_storage_fraction(param = param, manure_type = manure_type)
  
  # calculation
  yrs <- paste0('X', seq(1987,2017))
  
  # manure storage == FRAC_MMS_slurry || manure_spreading == 1 - FRAC_MMS_slurry
  if (missing(manure_spreading) == TRUE) {
    
    house_TAN[, yrs] <- sapply(yrs, function(x) round(  
      ((house_TAN[,x] - house_NH3[,x]) + (yard_TAN[,x] - yard_NH3[,x])) * X_store_slurry, 1))
  }
  else {
    house_TAN[, yrs] <- sapply(yrs, function(x) round( ((house_TAN[, x] - house_NH3[,x]) + 
                                                           (yard_TAN[,x] - yard_NH3[,x])) *
                                                         (1 - X_store_slurry), 1))
  }

  return(house_TAN)
}




compute_slurry_N_storage <- function(main_param, param, manure_type = 'Slurry', manure_spreading) {
  # computes the total amounts of N in storage before being applied
  # storage_slurry_N = [ (house_slurry_N - house_slurry_NH3) + (yard_N - yard_NH3)] * X_store_slurry
  
  house_maN <-  get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gross_manure', subfolder = 'N', subfolderX2 = 'Housing', subfolderX3 = manure_type, subfolderX4 = main_param, pattern = param)
  yard_maN <-  get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gross_manure', subfolder = 'N', subfolderX2 = 'Yards', subfolderX3 = 'Total', subfolderX4 = main_param, pattern = param)

  # Nh3 emissions
  house_NH3 <- get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gas_N_emissions', subfolder = 'NH3', subfolderX2 = 'Housing', subfolderX3 = manure_type, subfolderX4 = main_param, pattern = param)
  yard_NH3 <- get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gas_N_emissions', subfolder = 'NH3', subfolderX2 = 'Yards', subfolderX3 = 'Total', subfolderX4 = main_param, pattern = param)
  
  # X_STORE_SLURRY
  X_store_slurry <- set_manure_storage_fraction(param = param, manure_type = manure_type)
  
  # calculation
  yrs <- paste0('X', seq(1987,2017))
  
  # manure storage == FRAC_MMS_slurry || manure_spreading == 1 - FRAC_MMS_slurry
  if (missing(manure_spreading) == TRUE) {
    
    house_maN[, yrs] <- sapply(yrs, function(x) round( ( (house_maN[, x] - house_NH3[,x]) + 
                                                           (yard_maN[,x] - yard_NH3[,x]) ) *
                                                         X_store_slurry, 1))
  }
  else {
    house_maN[, yrs] <- sapply(yrs, function(x) round( ( (house_maN[, x] - house_NH3[,x]) + 
                                                           (yard_maN[,x] - yard_NH3[,x]) ) *
                                                         (1 - X_store_slurry), 1))
  }
  
  return(house_maN)
  rm(list=c('yard_maN','house_NH3','yard_NH3','FRAC_MMS_slurry'))
}



correct_slurry_TAN_storage <- function(main_param, param, manure_type = 'Slurry') {
  # corrects the TAN amounts in slurry_storage for the mineralized organic N fraction to TAN
  # corrected_TAN_slurry_storage = TAN_slurry_storage + (N_slurry_storage - TAN_slurry_storage) * f_min
  
  f_min = 0.1 # Dammgen et al., 2007
  
  storage_TAN = compute_slurry_TAN_storage(main_param, param, manure_type)
  storage_N = compute_slurry_N_storage(main_param, param, manure_type)
  
  # calculation
  yrs <- paste0('X', seq(1987,2017))
  storage_TAN[, yrs] <- sapply(yrs, function(x) round(storage_TAN[,x] + (storage_N[,x] - storage_TAN[,x]) * f_min, 1))
  
  return(storage_TAN)
  rm(list=c('f_min','storage_N'))
}


loop_N_storage <- function() {
  
  standard_params <- get_standard_params_list(main_param = 'Animals')

  for (i in 1:nrow(standard_params)) {
    
    main_param <- standard_params[i, 'Main_animals']
    param <- standard_params[i, 'Animals']
    
    storageN_slurry <- compute_slurry_N_storage(main_param = main_param, param = param)
    storageN_slurry <- data_cleaning(storageN_slurry)
    export_file(module = 'Nutrients', 
                file = storageN_slurry, 
                filename = param, 
                folder = 'Gross_manure', 
                subfolder = 'N', 
                subfolderX2 = 'Storage',
                subfolderX3 = 'Slurry',
                subfolderX4 = main_param)
    
    storageN_FYM <- compute_solid_N_storage(main_param = main_param, param = param)
    storageN_FYM <- data_cleaning(storageN_FYM)
    export_file(module = 'Nutrients', 
                file = storageN_FYM, 
                filename = param, 
                folder = 'Gross_manure', 
                subfolder = 'N', 
                subfolderX2 = 'Storage',
                subfolderX3 = 'Solid',
                subfolderX4 = main_param)
  }
}


## COMPUTE GASEOUS N EMISSIONS IN STORAGE (LIQUID SYSTEMS) ------------------------------------------------------

general_func_compute_storage_Nemissions <- function(N_gas, main_param, param, manure_type) {
  
  select_EF <- select_animal_N_EFs(N_gas = N_gas,pathway = 'Storage',param = param,manure_type = manure_type)
  
  ifelse(manure_type == 'Slurry',
         storage_TAN <- correct_slurry_TAN_storage(main_param, param, manure_type),
         storage_TAN <- compute_solid_TAN_storage(main_param, param, manure_type))
  
  # calculation
  yrs <- paste0('X', seq(1987,2017))
  storage_TAN[, yrs] <- sapply(yrs, function(x) round(storage_TAN[,x] * select_EF, 1))
  
  return(storage_TAN)
}


loop_slurry_solid_storage_Nemissions <- function(N_gas) {
  
  standard_params <- get_standard_params_list(main_param = 'Animals')
  man_type <- c('Slurry','Solid')
  
  for (j in man_type) {
    
    for (i in 1:nrow(standard_params)) {
      
      main_param <- standard_params[i, 'Main_animals']
      param <- standard_params[i, 'Animals']

      storage_Nemissions <- general_func_compute_storage_Nemissions(N_gas = N_gas, main_param = main_param, param = param, manure_type = j)
      export_file(module = 'Nutrients', 
                  file = storage_Nemissions, 
                  filename = param, 
                  folder = 'Gas_N_emissions', 
                  subfolder = N_gas, 
                  subfolderX2 = 'Storage',
                  subfolderX3 = j,
                  subfolderX4 = main_param)
    }
  }
}


loop_slurry_solid_storage_all_N_emissions <- function() {
  
  N_gases <- c('NH3','N2O','NOx','NN2')
  sapply(N_gases, function(x) loop_slurry_solid_storage_Nemissions(N_gas = x))
}



## COMPUTE TOTAL STORAGE EMISSIONS (E_STORAGE_SLURRY/SOLID) -----------------

general_func_compute_total_storage_emissions <- function(main_param, param, manure_type) {
  # general function to compute the sum of N emissions from manure storage
  # unit: kg N yr-1
  
  yrs <- paste0('X', seq(1987,2017))
  storage_Nemissions_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  storage_Nemissions_df[, yrs] <- sapply(yrs, function(x) storage_Nemissions_df[,x] <- 0)
  
  
  N_gases <- c('NH3','N2O','NOx','NN2')
  
  
  for (i in N_gases) {
    
    #get N emissions for a given gas and manure_type 
    storage_Nemissions <- get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gas_N_emissions', subfolder = i, subfolderX2 = 'Storage', subfolderX3 = manure_type, subfolderX4 = main_param, pattern = param)
    # sum in storage_Nemissions_df
    storage_Nemissions_df[, yrs] <- sapply(yrs, function(x) round(storage_Nemissions_df[,x] + storage_Nemissions[,x], 1))
  }

  return(storage_Nemissions_df)
  rm(list='storage_Nemissions')
}


compute_all_total_storage_emissions <- function() {
  
  standard_params <- get_standard_params_list(main_param = 'Animals')
  man_type <- c('Slurry','Solid')
  
  for (j in man_type) {
    
    for (i in 1:nrow(standard_params)) {
      
      main_param <- standard_params[i, 'Main_animals']
      param <- standard_params[i, 'Animals']
       
      tot_animal_storage_Nemissions <- general_func_compute_total_storage_emissions(main_param, param, j)
    }
  }
}
