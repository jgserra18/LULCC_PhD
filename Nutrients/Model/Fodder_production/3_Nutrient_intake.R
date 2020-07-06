source('./Nutrients/Model/Fodder_production/2_Nutrient_retention.R')


## N EXCRETION INDUSTRIAL SYSTEMS (HOUSING + YARDS) ---------------------------------------

compute_nutrient_excretion_Industrial = function(main_param, param, management, nutrient = 'N') {
  # computes the N excreted in industry farming (i.e., non-grazing systems)
  #unit: kg nutrient yr-1
  
  if (management == 'Grazing') {
    
    Nut_excreted_system =  get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gross_manure', subfolder = nutrient, subfolderX2 = 'Grazing', subfolderX3 = 'Total', subfolderX4 = main_param, pattern = param)
  }
  else {
    
    Nut_excreted_grazing = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gross_manure', subfolder = nutrient, subfolderX2 = 'Grazing', subfolderX3 = 'Total', subfolderX4 = main_param, pattern = param)
    Tot_nut_excreted = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gross_manure', subfolder = nutrient, subfolderX2 = paste0('Total_',nutrient,'excretion'), subfolderX3 = main_param, pattern = param)
    
    Nut_excreted_system = Tot_nut_excreted
    
    yrs = paste0('X',seq(1987,2017))
    Nut_excreted_system[, yrs] = sapply(yrs, function(x) round(Tot_nut_excreted[, x] - Nut_excreted_grazing[,x] ,2))
    rm(list=c('Nut_excreted_grazing', 'Tot_nut_excreted'))
  }
  return(Nut_excreted_system)
}



# TOTAL N INTAKE --------------------------------------------------------------------------


compute_nutrient_intake_otherAnimals= function(main_param, param, management, nutrient = 'N') {
  # computes Nutrient intake for other animals (Horses and rabbits)
  # assumption: Nretention = 0
  
  if (main_param == 'Equides' | main_param == 'Rabbits') {
    
    Nut_excreted = compute_nutrient_excretion_Industrial(main_param, param, management, nutrient)
    Nut_intake = Nut_excreted
    
    return(Nut_intake)
  }
  else {
    stop('Only horses and rabbits')
  }
  rm(list='Nut_excreted')
}


compute_nutrient_intake_largeRuminants_Pigs = function(main_param, param, management, nutrient = 'N') {
  # computes the nutrient feed intake from the remaining animals
  # Nutrient retention can be set to 0 or not depending on GLEAMS assumptions (see 2_Nutrient_retention.R)
  # Feed intake = Retention + Excretion
  # unit: kg nutrient yr-1

  if (main_param == 'Equides' | main_param == 'Rabbits' | main_param == 'Sheep' | main_param == 'Goats') {
    stop('Not horses and rabbits nor small ruminants.')
  }
  else {
    
    if (management != 'Industrial' & management != 'Grazing') {
      stop('Only Industrial and Grazing systems when calculating feed flows.')
    }
    else {
      Nut_excreted = compute_nutrient_excretion_Industrial(main_param, param, management, nutrient)
      Nut_retained = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Fodder_production', subfolder = 'Nutrient_retention', subfolderX2 = nutrient, subfolderX3 = management, subfolderX4 = main_param, pattern = param)
      
      Nut_feed = Nut_retained
      yrs = paste0('X',seq(1987,2017))
      Nut_feed[, yrs] = sapply(yrs, function(x) round(Nut_retained[, x] + Nut_excreted[,x] ,2))
      
      return(Nut_feed)
      rm(list=c('Nut_excreted','Nut_retained'))
    }
  }
}



compute_nutrient_intake_smallRuminants = function(main_param, param, management, nutrient = 'N') {
  # note: only the total N intake is calculated for all population
  # this is not only in order to reduce complexity, but also because the feeding management shouldn't be that difference, e.g., dairy vs non-dairy goats
  # unit: kg nutrient yr-1
  
  if (main_param != 'Goats' & main_param != 'Sheep') {
    stop('Only goats and sheep.')
  }
  else {
    
    Nut_excreted = compute_nutrient_excretion_Industrial(main_param, param, management, nutrient)
    
    # compute total N retained
    yrs = paste0('X',seq(1987,2017))
    Nut_retained <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
    Nut_retained[, yrs] <- sapply(yrs, function(x) Nut_retained[,x] = 0)
    
    if (main_param == 'Goats') { params = c('Buck','Doeling','Goats') } else { params = c('Ewes_dairy','Ewes_other','Ram') }
    
    for (animal in params) {
      
      Nut_retained_animal = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Fodder_production', subfolder = 'Nutrient_retention', subfolderX2 = nutrient, subfolderX3 = management, subfolderX4 = main_param, pattern = animal)
      Nut_retained[, yrs] = sapply(yrs, function(x) round(Nut_retained[,x] + Nut_retained_animal[,x], 1))
    }

    # calculate nutrient in feeding 
    Nut_feed = Nut_retained
    Nut_feed[, yrs] = sapply(yrs, function(x) round(Nut_retained[, x] + Nut_excreted[,x] , 1))
    
    return(Nut_feed)
    rm(list=c('Nut_excreted','Nut_retained', 'Nut_retained_animal'))
  }
}


general_func_nutrient_intake = function(main_param, param, management, nutrient = 'N') {
  # specifically allocates the nutrient intake formula according to the main animal or animal subclasses
  # unit: kg nutrient yr-1
  
  if (main_param == 'Equides' | main_param == 'Rabbits') {
    
    N_intake = compute_nutrient_intake_otherAnimals(main_param, param, management, nutrient)
  }
  else if (main_param == 'Goats' | main_param == 'Sheep') {
    
    N_intake = compute_nutrient_intake_smallRuminants(main_param, param, management, nutrient)
  }
  else {
    N_intake = compute_nutrient_intake_largeRuminants_Pigs(main_param, param, management, nutrient)
  }
  
  return(N_intake)
}





compute_nutrient_feed_intake = function(nutrient = 'N') {
  
  standard_params <- get_standard_params_list(main_param = 'Animals')
  main_param <- unique(standard_params[, 'Main_animals'])
  management = c('Grazing','Industrial')
  
  for (i in main_param) {
    
    param = get_animal_subclass(i, 'totals')

    for (animal in param) {
      
      yrs = paste0('X',seq(1987,2017))
      total <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
      total[, yrs] <- sapply(yrs, function(x) total[,x] = 0)
      
      for (system in management) {
        
        if (system == 'Grazing' & i == 'Poultry') { next } else  {

          Nut_intake = general_func_nutrient_intake(main_param = i, param = animal, management = system, nutrient = nutrient) 
          total[, yrs] <- sapply(yrs, function(x) round(total[,x] + Nut_intake[, x], 1))
          
          export_file(module = 'Nutrients', 
                      file = Nut_intake, 
                      filename = animal, 
                      folder = 'Fodder_production', 
                      subfolder = 'Nutrient_intake', 
                      subfolderX2 = 'N',
                      subfolderX3 = system,
                      subfolderX4 = i)
        }
      }
      # export total
      export_file(module = 'Nutrients', 
                  file = Nut_intake, 
                  filename = animal, 
                  folder = 'Fodder_production', 
                  subfolder = 'Nutrient_intake', 
                  subfolderX2 = 'N',
                  subfolderX3 = 'Total',
                  subfolderX4 = i)
    }
  }
}


