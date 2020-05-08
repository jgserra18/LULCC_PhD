source('./Main/Global_functions.R')


compute_manure_spreading_soil_NO_emissions <- function() {
  # using EMEP 2019 emission factor of 0.04 kg NO2 kg N-1 yr-1
  # computes soil NO2 emissions from manure spreading after NH3 emissions (NET N returned)
  # unit: kg N-NO2 yr-1
  
  # convert emission factor to N compound
  EF_NO2 <- 0.04 
  EF_NO2 <- EF_NO2 / 3.28443
  
  standard_params <- get_standard_params_list(main_param = 'Animals')
  man_type <- c('Slurry','Solid')
  
  for (j in man_type) {
    
    for (i in 1:nrow(standard_params)) {
      
      main_param <- standard_params[i, 'Main_animals']
      param <- standard_params[i, 'Animals']
      
      man_net_spreadN <- get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Fertilisers', subfolder = 'N', subfolderX2 = 'Spreading', subfolderX3 = j, subfolderX4 = main_param, pattern = param)
      
      # calculation
      yrs <- paste0('X', seq(1987,2017))
      man_net_spreadN[, yrs] <- sapply(yrs, function(x) round(man_net_spreadN[,x] * EF_NO2, 1))
      
      export_file(module = 'Nutrients', 
                  file = man_net_spreadN, 
                  filename = param, 
                  folder = 'Gas_N_emissions', 
                  subfolder = 'NOx', 
                  subfolderX2 = 'Spreading',
                  subfolderX3 = j,
                  subfolderX4 = main_param)
    }
  }
  rm(list='man_net_spreadN')
}
