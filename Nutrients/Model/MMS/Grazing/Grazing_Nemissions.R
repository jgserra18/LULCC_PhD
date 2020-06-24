source('./Main/Global_functions.R')
source('./Nutrients/Model/MMS/Support_functions/Convert_N_to_TAN_flows.R')
source('./Nutrients/Model/MMS/Support_functions/Find_EFs.R')



## GRAZING N-NH3 EMISSIONS ---------------------------------------------------------------------------------------
## EMEP 2016

compute_grazing_NH3_emissions <- function(main_param, param) {
  # computes grazing NH3 emissions
  # unit: kg N-NH3 yr-1
  
  graz_maN <-  get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gross_manure', subfolder = 'N', subfolderX2 = 'Grazing', subfolderX3 ='Total', subfolderX4 = main_param, pattern = param)
  graz_TAN <- convert_N_to_TAN_dataframe(main_param = main_param, df = graz_maN)
  
  EF <- select_animal_N_EFs(N_gas = 'NH3', pathway = 'Grazing', param = param, manure_type = 'Solid')
  
  yrs <- paste0('X', seq(1987,2017))
  graz_TAN[, yrs] <- sapply(yrs, function(x) round(graz_TAN[,x] * EF, 1))
  return(graz_TAN)
}



loop_grazing_NH3_emissions <- function() {
  
  standard_params <- get_standard_params_list('Animals')
  
  for (i in 1:nrow(standard_params)) {
    
    main_param <- standard_params[i, 'Main_animals']
    param <- standard_params[i, 'Animals']
    
    graz_NH3 <- compute_grazing_NH3_emissions(main_param = main_param, param = param)
    export_file(module = 'Nutrients', 
                file = graz_NH3, 
                filename = param, 
                folder = 'Gas_N_emissions', 
                subfolder = 'NH3', 
                subfolderX2 = 'Grazing',
                subfolderX3 = 'Total',
                subfolderX4 = main_param)
  }
}


## GRAZING N-N2O EMISSIONS ---------------------------------------------------------------------------------------
## IPCC 2019

compute_grazing_N2O_emissions <- function(main_param, param) {
  # computes grazing NH3 emissions
  # unit: kg N-N2o yr-1
  
  graz_maN <-  get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gross_manure', subfolder = 'N', subfolderX2 = 'Grazing', subfolderX3 ='Total', subfolderX4 = main_param, pattern = param)

  EF <- select_animal_N_EFs(N_gas = 'N2O', pathway = 'Grazing', param = param)
  
  yrs <- paste0('X', seq(1987,2017))
  graz_maN[, yrs] <- sapply(yrs, function(x) round(graz_TAN[,x] * EF, 1))
  return(graz_TAN)
}


loop_grazing_N2O_emissions <- function() {
  
  standard_params <- get_standard_params_list('Animals')
  
  for (i in 1:nrow(standard_params)) {
    
    main_param <- standard_params[i, 'Main_animals']
    param <- standard_params[i, 'Animals']
    
    graz_N2O <- compute_grazing_N2O_emissions(main_param = main_param, param = param)
    export_file(module = 'Nutrients', 
                file = graz_N2O, 
                filename = param, 
                folder = 'Gas_N_emissions', 
                subfolder = 'N2O', 
                subfolderX2 = 'Grazing',
                subfolderX3 = 'Total',
                subfolderX4 = main_param)
  }
}

