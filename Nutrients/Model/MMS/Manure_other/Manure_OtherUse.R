source('./Nutrients/Model/MMS/Spreading/Spreading.R')


## MANURE DIRECT DISCARGE TO RIVER STREAMS -----------------------------------------------------------

compute_manure_discharge_rivers <- function(nutrient, main_param, param, manure_type, manure_use = 'River_discharge') {
  # unit: kg N yr-1
  
  if (nutrient == 'N') {
    
    ifelse(manure_type == 'Solid', man_discharge <- allocate_solid_manure_spreading('N',main_param, param, manure_type, manure_use), man_discharge <- allocate_slurry_spreading('N',main_param, param, manure_type, manure_use))
  }
  else if (nutrient == 'P') {
    print('IMPLEMENT THIS!')
  }
  
  return(man_discharge)
}


loop_manure_discharge_rivers <- function(nutrient) {
  
  standard_params <- get_standard_params_list(main_param = 'Animals')
  man_type <- c('Slurry','Solid')
  
  for (j in man_type) {
    
    for (i in 1:nrow(standard_params)) {
      
      main_param <- standard_params[i, 'Main_animals']
      param <- standard_params[i, 'Animals']
      
      man_discharge <- compute_manure_discharge_rivers(nutrient, main_param, param, manure_type = j)
      export_file(module = 'Nutrients', 
                  file = man_discharge, 
                  filename = param, 
                  folder = 'Gross_manure', 
                  subfolder = nutrient, 
                  subfolderX2 = 'River_discharge',
                  subfolderX3 = j,
                  subfolderX4 = main_param)
    }
  }
  rm(list=c('man_type','man_discharge'))
}



## MANURE INTER-MUNICIPALITY TRANSPORT  -----------------------------------------------------------

compute_manure_transported <- function(nutrient, main_param, param, manure_type, manure_use = 'Transport') {
  # unit: kg N yr-1
  
  if (nutrient == 'N') {
    
    ifelse(manure_type == 'Solid', man_transport <- allocate_solid_manure_spreading('N',main_param, param, manure_type, manure_use), man_transport <- allocate_slurry_spreading('N',main_param, param, manure_type, manure_use))
  }
  else if (nutrient == 'P') {
    print('IMPLEMENT THIS!')
  }
  
  return(man_transport)
}


loop_manure_transported <- function(nutrient) {
  
  standard_params <- get_standard_params_list(main_param = 'Animals')
  man_type <- c('Slurry','Solid')
  
  for (j in man_type) {
    
    for (i in 1:nrow(standard_params)) {
      
      main_param <- standard_params[i, 'Main_animals']
      param <- standard_params[i, 'Animals']
      
      man_transport <- compute_manure_transported(nutrient, main_param, param, manure_type = j)
      export_file(module = 'Nutrients', 
                  file = man_transport, 
                  filename = param, 
                  folder = 'Gross_manure', 
                  subfolder = nutrient, 
                  subfolderX2 = 'Transported_IntraMunicipality',
                  subfolderX3 = j,
                  subfolderX4 = main_param)
    }
  }
  rm(list=c('man_type','man_transport'))
}

