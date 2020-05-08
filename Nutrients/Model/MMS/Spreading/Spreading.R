source('./Nutrients/Model/MMS/Storage/Storage.R')


## COMPUTE N/TAN AVAILABLE FOR SPREADING (SOLID SYSTEMS) -------------------------------------------------------------------------------------------

compute_solid_TAN_spreading <- function(main_param, param, manure_type = 'Solid') {
  
  TAN_spreading <- compute_solid_TAN_storage(main_param, param, manure_type, manure_spreading = TRUE)
  return(TAN_spreading)
}

compute_solid_N_spreading <- function(main_param, param, manure_type = 'Solid') {
  
  N_spreading <- compute_solid_N_storage(main_param, param, manure_type, manure_spreading = TRUE)
  return(N_spreading)
}


## COMPUTE N/TAN AVAILABLE BEFORE APPLICATION (SLURRY SYSTEMS) -------------------------------------------------------------------------------------------

compute_slurry_TAN_spreading <- function(main_param, param, manure_type = 'Solid') {
  
  TAN_spreading <- compute_slurry_TAN_storage(main_param, param, manure_type, manure_spreading = TRUE)
  return(TAN_spreading)
}

compute_slurry_N_spreading <- function(main_param, param, manure_type = 'Solid') {
  
  N_spreading <- compute_slurry_N_storage(main_param, param, manure_type, manure_spreading = TRUE)
  return(N_spreading)
}



## ALLOCATE THE FRACTION OF MANURE SPREADING, AND LIKEWISE THE FRAC OF THE MANURE DISCARGED INTO STREAMS OR TRANSPORTED ELSEWHERE ------------------

manure_spreading_allocation_FRAC <- function(manure_allocation) {
  # manure_allocation == 'Fertiliser' | 'River_discharge' | 'Transport'
  
  FRAC_allocation <- get_activity_data(module = 'Nutrients', subfolder = 'General_params', subfolderX2 = 'Animals', subfolderX3 = 'Manure_allocation', pattern = manure_allocation)

}




allocate_slurry_spreading <- function(N_flow, main_param, param, manure_type = 'Slurry') {
  
  FRAC_spreading <- get_activity_data(module = 'Nutrients', subfolder = 'General_params', subfolderX2 = 'Animals', subfolderX3 = 'Manure_allocation', pattern = 'Fertiliser')
  
  ifelse(N_flow == 'TAN',
         slurry_spread <- compute_slurry_TAN_spreading(main_param, param, manure_type),
         slurry_spread <- compute_slurry_N_spreading(main_param, param, manure_type))
  
  # calculation
  yrs <- paste0('X', seq(1987,2017))
  slurry_spread[, yrs] <- sapply(yrs, function(x) round(slurry_spread[,x] * FRAC_spreading[, 'X2009'], 1))
  
  return(slurry_spread)
}


allocate_solid_manure_spreading <- function(N_flow, main_param, param, manure_type = 'Solid') {
  
  FRAC_spreading <- get_activity_data(module = 'Nutrients', subfolder = 'General_params', subfolderX2 = 'Animals', subfolderX3 = 'Manure_allocation', pattern = 'Fertiliser')
  
  ifelse(N_flow == 'TAN',
         solid_spread <- compute_solid_TAN_spreading(main_param, param, manure_type),
         solid_spread <- compute_solid_N_spreading(main_param, param, manure_type))
  
  # calculation
  yrs <- paste0('X', seq(1987,2017))
  solid_spread[, yrs] <- sapply(yrs, function(x) round(solid_spread[,x] * FRAC_spreading[, 'X2009'], 1))
  
  return(solid_spread)
}


## COMPUTE N/TAN AVAILABLE FOR SPREADING -----------------------------------------------------------------------------------
# EQ 31

compute_total_slurry_available_spreading <- function(N_flow, main_param, param, manure_type = 'Slurry') {
    
  
  ifelse(N_flow == 'TAN',
         slurry_spread <- compute_slurry_TAN_spreading(main_param, param, manure_type),
         slurry_spread <- compute_slurry_N_spreading(main_param, param, manure_type))
  
  ifelse(N_flow == 'TAN',
         slurry_storage <- correct_slurry_TAN_storage(main_param, param, manure_type),
         slurry_storage <- compute_slurry_N_storage(main_param, param, manure_type))
  
  tot_storage_Nemissions <- general_func_compute_total_storage_emissions(main_param, param, manure_type) # from Storage.R
  
  # calculation
  yrs <- paste0('X', seq(1987,2017))
  slurry_spread[, yrs] <- sapply(yrs, function(x) round(slurry_spread[,x] + slurry_storage[, x] - tot_storage_Nemissions[,x], 1))
  
  return(slurry_spread)
  rm(list=c('slurry_storage','tot_storage_Nemissions'))
}



compute_total_solid_available_spreading <- function(N_flow, main_param, param, manure_type = 'Solid') {
  
  
  ifelse(N_flow == 'TAN',
         solid_spread <- compute_solid_TAN_spreading(main_param, param, manure_type),
         solid_spread <- compute_solid_N_spreading(main_param, param, manure_type))
  
  ifelse(N_flow == 'TAN',
         solid_storage <- compute_solid_TAN_storage(main_param, param, manure_type),
         solid_storage <- compute_slurry_N_storage(main_param, param, manure_type))
  
  tot_storage_Nemissions <- general_func_compute_total_storage_emissions(main_param, param, manure_type) # from Storage.R
  
  # calculation
  yrs <- paste0('X', seq(1987,2017))
  solid_spread[, yrs] <- sapply(yrs, function(x) round(solid_spread[,x] + solid_storage[, x] - tot_storage_Nemissions[,x], 1))
  
  return(solid_spread)
  rm(list=c('slurry_storage','tot_storage_Nemissions'))
}



## COMPUTE NH3 EMISSIONS DURING AND FOLLOWING SPREADING  -----------------------------------------------------------------------------------

compute_manure_spreading_NH3_emissions <- function(main_param, param, manure_type) {
  
  EF <- select_animal_N_EFs(N_gas = 'NH3', pathway = 'Spreading', param = param, manure_type = manure_type)
  
  ifelse(manure_type == 'Solid',
         man_spreadN <- compute_total_solid_available_spreading('TAN',main_param, param),
         man_spreadN <- compute_total_slurry_available_spreading('TAN',main_param, param))
  
  # calculation
  yrs <- paste0('X', seq(1987,2017))
  man_spreadN[, yrs] <- sapply(yrs, function(x) round(man_spreadN[,x] * EF, 1))
  
  return(man_spreadN)
}


loop_manure_spreading_NH3_emissions <- function() {
  
  standard_params <- get_standard_params_list(main_param = 'Animals')
  man_type <- c('Slurry','Solid')
  
  for (j in man_type) {
    
    for (i in 1:nrow(standard_params)) {
      
      main_param <- standard_params[i, 'Main_animals']
      param <- standard_params[i, 'Animals']
      
      man_spread_NH3 <- compute_manure_spreading_NH3_emissions(main_param, param, j)
      export_file(module = 'Nutrients', 
                  file = man_spread_NH3, 
                  filename = param, 
                  folder = 'Gas_N_emissions', 
                  subfolder = 'NH3', 
                  subfolderX2 = 'Spreading',
                  subfolderX3 = j,
                  subfolderX4 = main_param)
    }
  }
  rm(list=c('man_type','man_spread_NH3'))
}


## COMPUTE NET N RETURNED TO SOIL AFTER SPREADING  -----------------------------------------------------------------------------------


compute_manure_spreading_net_N <- function(N_flow, main_param, param, manure_type) {
  
  
  
  if (manure_type == 'Solid') {
    
    ifelse(N_flow == 'TAN',
           man_spreadN <- compute_total_solid_available_spreading('TAN',main_param, param),
           man_spreadN <- compute_total_solid_available_spreading('N',main_param, param))
  }
  else {
    ifelse(N_flow == 'TAN',
           man_spreadN <- compute_total_slurry_available_spreading('TAN',main_param, param),
           man_spreadN <- compute_total_slurry_available_spreading('N',main_param, param))
  }
  
  man_spread_NH3 <- get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gas_N_emissions', subfolder = 'NH3', subfolderX2 = 'Spreading', subfolderX3 = manure_type, subfolderX4 = main_param, pattern = param)
  
  # calculation
  yrs <- paste0('X', seq(1987,2017))
  man_spreadN[, yrs] <- sapply(yrs, function(x) round(man_spreadN[,x] - man_spread_NH3[, x], 1))
  
  return(man_spreadN)
  rm(list='man_spread_NH3')
}



compute_all_manure_spreading_net_N <- function() {
  # computes total N returned to the soil following NH3 application
  # Activity data for N2O, NOx and runoff emissions ---------------------
  # unit: kg N yr-1
  
  standard_params <- get_standard_params_list(main_param = 'Animals')
  man_type <- c('Slurry','Solid')
  
  for (j in man_type) {
    
    for (i in 1:nrow(standard_params)) {
      
      main_param <- standard_params[i, 'Main_animals']
      param <- standard_params[i, 'Animals']
      
      man_net_N <- compute_manure_spreading_net_N(N_flow = 'N', main_param, param, j)
      export_file(module = 'Nutrients', 
                  file = man_net_N, 
                  filename = param, 
                  folder = 'Fertilisers', 
                  subfolder = 'N', 
                  subfolderX2 = 'Spreading',
                  subfolderX3 = j,
                  subfolderX4 = main_param)
    }
  }
  rm(list='man_net_N')
}

