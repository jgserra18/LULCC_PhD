source('./Main/Global_functions.R')
source('./Nutrients/Model/Fodder_production/4_DairyCows_Concentrate.R')
source('./Nutrients/Model/Fodder_production/3_Nutrient_intake.R')




# FEED ASSUMPTIONS ---------------------

#* PIGS: ration formulated for backyard pigs; for industrial pigs, 100% comes from concentrate or othern on-local feedstuff
#* Poultry: 100% comes from concentrate or other non-local feedstuff
#* Rabbits: 100% comes from concentrate or other non-local feedstuff

# RATIONS FORMULATED ------------------
#* Bovine: dairy cattle and other_cattle
#* Goats and Sheep
#* Backyard pigs
#* Horses

# DAIRY CATTLE PARTICULARITY ---------
#* Crop residues are fed as the difference of 1 - FRAC_other_roughage - FRAC_concentrates



# NUTRIENTS IN CONCENTRATE FEEDING --------------------------------------------------------


compute_nutrient_only_concentrates = function(main_param, param, management = 'Industrial', nutrient ='N') {
  # computes the nutrient content of animals where the feeding ratio is 100% concentrates or other non-locall feedstuff
  # unit: kg nutrient yr-1
  
  if (main_param == 'Rabbits' | main_param == 'Poultry' | (main_param == 'Pigs' & management == 'Industrial')) {
    
    Nut_intake = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Fodder_production',subfolder = 'Nutrient_intake', subfolderX2 = nutrient, subfolderX3 = 'Industrial', subfolderX4 = main_param, pattern = param)
    return(Nut_intake)
  }
  else {
    stop('Only animals fed 100% of concentrate/non-local feedstuff.')
  }
}


compute_nutrient_concentrates_dairyCow = function(main_param = 'Bovine', param = 'Dairy_cows', management, nutrient = 'N') {
  # unit: kg nutrient yr-1
  
  Nut_intake = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Fodder_production',subfolder = 'Nutrient_intake', subfolderX2 = nutrient, subfolderX3 = management, subfolderX4 = main_param, pattern = param)
  FRAC_concentrate = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Fodder_production',subfolder = 'FRAC_concentrate', subfolderX2 = main_param, pattern = param)
  
  yrs = paste0('X',seq(1987,2017))
  
  Nut_concentrate = FRAC_concentrate
  Nut_concentrate[, yrs] = sapply(yrs, function(x) round(Nut_intake[,x] * FRAC_concentrate[,x], 1))
  
  return(Nut_concentrate)
  rm(list=c('Nut_intake','FRAC_concentrate'))
}



compute_ration_concentrate_fraction = function(animal_ration = 'Equides') {
  # calculates the fraction of concentrates = 1 - sum(FRAC_roughages)
  # only for those with rations formulated
  # unit: %
  
  ration_composition = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Animals', subfolderX2 = 'Ration', subfolderX3 = 'Non_dairy', pattern = animal_ration)
  FRAC_concentrate = 1 - sum(ration_composition[, 'FRAC'])
  
  return(FRAC_concentrate)
}



compute_nutrient_concentrates_otherAnimals = function(main_param = 'Bovine', param, management, nutrient = 'N') {
  # only for pigs and Other_cattle (-dairy and non dairy cows)
  # unit. kg nutrient yr-1
  
  if (param == 'Dairy_cows' | param == 'Non_dairy') {
    stop('Other cattle only.')
  }
  else if (main_param == 'Pigs' & management == 'Industrial') {
    stop('Backyard pigs only.')
  }
  else {

    # define ration finder
    animal_ration  = ifelse(main_param == 'Bovine','Other_cattle', main_param)
    
    
    FRAC_concentrate = compute_ration_concentrate_fraction(animal_ration) # this is already included in the rations formulated
    Nut_intake = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Fodder_production',subfolder = 'Nutrient_intake', subfolderX2 = nutrient, subfolderX3 = management, subfolderX4 = main_param, pattern = param)
    
    yrs = paste0('X',seq(1987,2017))
    
    Nut_concentrate = Nut_intake
    Nut_concentrate[, yrs] = sapply(yrs, function(x) round(Nut_intake[,x] * FRAC_concentrate, 1))
    
    return(Nut_concentrate)
    rm(list=c('Nut_intake'))
  }
}




general_func_nutrient_concentrates = function(main_param, param, management, nutrient = 'N') {
  # unit: kg nutrient yr-1
  
  if (param == 'Dairy_cows') {
    
    Nut_concentrate = compute_nutrient_concentrates_dairyCow(main_param, param, management, nutrient)
  }
  else if ( (main_param == 'Bovine' & param != 'Dairy_cows' & param != 'Non_dairy') | (main_param == 'Pigs' & management == 'Grazing') ) {
    
    Nut_concentrate = compute_nutrient_concentrates_otherAnimals(main_param, param, management, nutrient)
  }
  else if (main_param == 'Rabbits' | main_param == 'Poultry' | (main_param == 'Pigs' & management == 'Industrial')) {
    
    Nut_concentrate = compute_nutrient_only_concentrates(main_param, param, management, nutrient)
  }
  return(Nut_concentrate)
}


loop_nutrient_concentrates_feeding = function(nutrient = 'N') {
  
  main_params = c('Poultry','Pigs','Bovine','Rabbits') 
  management = c('Grazing','Industrial')
  
  for (system in management) {
    
    for (main_param in main_params) {
      
      if (system != 'Industrial' & (main_param == 'Rabbits' | main_param == 'Poultry')) {
        next 
      }
      else {
        params = get_animal_subclass(main_param)
      }
      

      for (param in params) {
        print(paste0(system, main_param, param))
        
        if (param == 'Non_dairy') {
          next 
        }
        else {
          
          Nut_concentrate = general_func_nutrient_concentrates(main_param, param, system, nutrient)
          export_file(module = 'Nutrients', 
                      file = Nut_concentrate, 
                      filename = param, 
                      folder = 'Fodder_production', 
                      subfolder = 'Nutrient_ration', 
                      subfolderX2 = nutrient,
                      subfolderX3 = system,
                      subfolderX4 = 'Concentrates',
                      subfolderX5 = main_param)
          
        }
      }
    }
  }
}



# COMPUTE NUTRIENTS IN ROUGHAGE ration  DAIRY COWS ---------------------------------------------------------------- 


compute_nutrient_roughage_individualFeed_Dairy_cows = function(main_param='Bovine', param = 'Dairy_cows', roughage_feed, management, crop_product = 'Harvest', nutrient = 'N') {
  # computes the nutrient intake of a given roughage feed (e.g., Hay) for a given animal and management
  #  DAIRY COWS
  # unit: kg nutrient yr-1
  
  yrs = paste0('X',seq(1987,2017))
  
  # Total nutrient itnake of a given livestock subclass
  Nut_intake = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Fodder_production',subfolder = 'Nutrient_intake', subfolderX2 = nutrient, subfolderX3 = management, subfolderX4 = main_param, pattern = param)
  
  # get roughage feed params, subset based on roughage feed and crop_product
  roughage_ration = select_animal_roughage_feed_ration(main_param, param)
  roughage_ration = subset(roughage_ration, Crop_product == crop_product & Roughage == roughage_feed)
  
  # correct roughage feed based on the concentrate fraction
  FRAC_roughage_feed = roughage_ration[, 'FRAC']
  FRAC_concentrate = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Fodder_production',subfolder = 'FRAC_concentrate', subfolderX2 = main_param, pattern = param)

  updated_FRAC_roughage_feed = FRAC_concentrate
  sum_FRAC_roughage_feed = sum(roughage_ration[, 'FRAC'])
  updated_FRAC_roughage_feed[, yrs] = sapply(yrs, function(x) round(FRAC_roughage_feed / (sum_FRAC_roughage_feed + FRAC_concentrate[,x]), 1))
  
  # correct nutrient flow of roughage feed
  Nut_intake[, yrs] = sapply(yrs, function(x) round(Nut_intake[,x] * updated_FRAC_roughage_feed[,x] , 23))
  
  return(Nut_intake)
  rm(list=c('Nut_intake','animal_ration','roughage_ration','FRAC_roughage_feed'))
}




# COMPUTE NUTRIENTS IN ROUGHAGE ration NON DAIRY ANIMALS ---------------------------------------------------------------- 



select_animal_roughage_feed_ration = function(main_param, param) {
  # selects general animal rations
  
  if (param != 'Dairy_cows') {
    
    # find appropriate roughage ration 
    if (main_param == 'Bovine' & param != 'Non_dairy') { animal_ration = 'Other_cattle' } 
    else if (main_param == 'Bovine' & param == 'Non_dairy') { animal_ration = 'Non_dairy' }
    else if (main_param == 'Pigs') { animal_ration = 'Pigs_grazing' } 
    else { animal_ration = main_param }
    
    animal_div = 'Non_dairy'
  }
  
  else {
    
    animal_div = 'Dairy'
    animal_ration = 'Dairy_cows'
  }
  
  # get roughage feed params, 
  roughage_ration = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Animals', subfolderX2 = 'Ration', subfolderX3 = animal_div, pattern = animal_ration)
  
  return(roughage_ration)
}



compute_nutrient_roughage_individualFeed_nonDairy_animals = function(main_param, param, roughage_feed, management, crop_product, nutrient = 'N') {
  # computes the nutrient intake of a given roughage feed (e.g., Hay) for a given animal and management
  # ONLY NON-DAIRY ANIMALS
  # unit: kg nutrient yr-1
  
  # Total nutrient itnake of a given livestock subclass
  Nut_intake = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Fodder_production',subfolder = 'Nutrient_intake', subfolderX2 = nutrient, subfolderX3 = management, subfolderX4 = main_param, pattern = param)
  
  # get roughage feed params, subset 
  roughage_ration = select_animal_roughage_feed_ration(main_param, param)
  roughage_ration = subset(roughage_ration, Crop_product == crop_product & Roughage == roughage_feed)
  
  FRAC_roughage_feed = roughage_ration[, 'FRAC']

  yrs = paste0('X',seq(1987,2017))
  Nut_intake[, yrs] = sapply(yrs, function(x) round(Nut_intake[, x] * FRAC_roughage_feed, 1))
  
  return(Nut_intake)
  rm(list=c('Nut_intake','animal_ration','roughage_ration','FRAC_roughage_feed'))
}



compute_all_nutrient_roughage_feed_nonDairy_animals= function(main_param, param, roughage_feed, management, nutrient = 'N') {
  # if needed to loop each roughage feed in the ration of a non_dairy animal
  # unit. kg nutrient yr-1
  

  # get roughage feed params, 
  roughage_ration = select_animal_roughage_feed_ration(main_param, param)
  
  for (i in 1:nrow(roughage_ration)) {
    
    main_param = roughage_ration[i, 1]
    param = roughage_ration[i, 2]
    crop_product = roughage_ration[i, 3]
    if (crop_product == 'Harvest') { crop_products = 'Main_crop' } else { crop_products = 'Residues'}
    roughage_feed = roughage_ration[i, 4]
    
    Nutrient_roughage_feed = compute_nutrient_roughage_individualFeed_nonDairy_animals(main_param, param, roughage_feed, management, crop_product, nutrient)
    if (any(is.na(Nutrient_roughage_feed))==T) {
      print(paste0('NA here', param))
    }
    
    export_file(module = 'Nutrients', 
                file = Nutrient_roughage_feed, 
                filename = param, 
                folder = 'Fodder_production', 
                subfolder = 'Nutrient_ration', 
                subfolderX2 = nutrient,
                subfolderX3 = system,
                subfolderX4 = crop_products,
                subfolderX5 = management,
                subfolderX6 = main_param)
    
  }
}



loop_nutrient_roughage_feed_nonDairy_Animalsl = function(nutrient='N') {
  # just in case ithis is required
  # unit: kg nutrient yr-1
  
  main_params = c('Sheep','Pigs','Bovine','Goats','Equides')
  management = c('Grazing','Industrial')
  

  for (main_param in main_params) {
  
    params = get_animal_subclass(main_param, 'totals')

    for (param in params) {
      
        roughage_ration = select_animal_roughage_feed_ration(main_param, param)
      
        for (i in 1:nrow(roughage_ration)) {
          
          main_crop = roughage_ration[i, 1]
          crop = roughage_ration[i, 2]
          crop_product = roughage_ration[i, 3]
          
          if (crop_product == 'Harvest') { 
            
            crop_products = 'Main_crop' 
            
            roughage_feed = roughage_ration[i, 4]
            
            # prepare total df (industrial + grazing)
            yrs = paste0('X',seq(1987,2017))
            total <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
            total[, yrs] <- sapply(yrs, function(x) total[,x] = 0)
            
            for (system in management) {
              
              if (main_param == 'Pigs' & system == 'Industrial') {
                next 
              }
              else {
                
                if (param == 'Dairy_cows') {
                  Nutrient_roughage_feed = compute_nutrient_roughage_individualFeed_Dairy_cows(main_param, param, roughage_feed, management, crop_product, nutrient)
                } else {
                  Nutrient_roughage_feed = compute_nutrient_roughage_individualFeed_nonDairy_animals(main_param, param, roughage_feed, management,crop_product,  nutrient)
                }
                total[, yrs] <- sapply(yrs, function(x) total[,x] + Nutrient_roughage_feed[,x])
                
                export_file(module = 'Nutrients', 
                            file = Nutrient_roughage_feed, 
                            filename = roughage_feed, 
                            folder = 'Fodder_production', 
                            subfolder = 'Nutrient_ration', 
                            subfolderX2 = nutrient,
                            subfolderX3 = system,
                            subfolderX4 = crop_products,
                            subfolderX5 = main_param,
                            subfolderX6 = param)
              }
              # export total nutrient intake for a given roughage feed
              export_file(module = 'Nutrients', 
                          file = total, 
                          filename = roughage_feed, 
                          folder = 'Fodder_production', 
                          subfolder = 'Nutrient_ration', 
                          subfolderX2 = nutrient,
                          subfolderX3 = 'Total',
                          subfolderX4 = crop_products,
                          subfolderX5 = main_param,
                          subfolderX6 = param)
              
            }
            
          } 
          else { 
            
            crop_products = 'Residues'
            next 
          }
        }
    }
  }
}


compute_total_nutrient_roughage_feed_ForagePasturesFlows = function(crop_product = 'Main_crop', roughage_feed, nutrient = 'N') {
  # calculates the total nutrient flows of a given roughage feed (e.g., Fresh_hay)
  # needed to calculate fodder production of the main crop products and crop residues (e.g., Pastures, Fodder)
  # unit: kg nutrient yr-
  
  
  if (crop_product != 'Main_crop') {
    break('Only for main products!!')
  }
  else {
    
    main_params = c('Sheep','Pigs','Bovine','Goats','Equides')
    
    # prepare total nutrient flows for a given roughage feed
    yrs = paste0('X',seq(1987,2017))
    total <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
    total[, yrs] <- sapply(yrs, function(x) total[,x] = 0)
    
    for (main_param in main_params) {
      
      params = get_animal_subclass(main_param, 'totals')
      
      for (param in params) {
        
        roughage_ration = select_animal_roughage_feed_ration(main_param, param)
        
        if (length(which(roughage_ration[, 4] == roughage_feed)) == 0) {
          next 
        }
        else {
          
          file_exists = list_all_files_folder(module = 'Nutrients', mainfolder = 'Output', folder = 'Fodder_production', subfolder = 'Nutrient_ration', subfolderX2 = nutrient, subfolderX3 = 'Total', subfolderX4 = crop_product, subfolderX5 = main_param, subfolderX6 = param)
          file_exists = gsub('.csv','',file_exists)
          if (length(which(file_exists==roughage_feed)) == 0) {
            next 
          }
          else {
            
            Nut_roughage = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Fodder_production', subfolder = 'Nutrient_ration', subfolderX2 = nutrient, subfolderX3 = 'Total', subfolderX4 = crop_product, subfolderX5 = main_param, subfolderX6 = param, pattern = roughage_feed)
            total[, yrs] <- sapply(yrs, function(x) total[,x] + Nut_roughage[,x])
          }
        }
      }
    }
    
    return(total)
    rm(list=c('main_params','yrs','params','Nut_roughage'))
    
  }
}



compute_admin_nutrient_roughage_feed_ForagePasturesFlows = function(admin = 'PT', crop_product = 'Main_crop', roughage_feed, nutrient = 'N', convert_to_DM = FALSE) {
  # compute nutrient flows on a crop area basis for the given admin region
  # can be converted to DM as well
  # unit: kg nutrient ha-1 yr-1 or kg DM ha-1 yr-1
  
  yrs = paste0('X',seq(1987,2017))
  total_flows_muni = compute_total_nutrient_roughage_feed_ForagePasturesFlows(crop_product, roughage_feed, nutrient)
  
  if (roughage_feed == 'Fresh_grass') { main_param = 'Pastures'; param = 'Extensive_pasture' } else { main_param = 'Forage'; param = roughage_feed }
  crop_area <- get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = main_param, pattern = param)


  if (admin == 'PT') {
    # compute total nutrient flows at the mainland level (kg nutrient ha-1 yr-1)
    total_flows_muni = sapply(yrs, function(x) sum(total_flows_muni[, x]))
    crop_area = sapply(yrs, function(x) sum(crop_area[, x]))
    total_flows_muni= round(total_flows_muni / crop_area, 1)
  }
  else {
    
    total_flows_muni = compute_temporal_sumIF_admin_df(admin = admin,merge_df = total_flows_muni)
    crop_area = compute_temporal_sumIF_admin_df(admin = admin, merge_df = crop_area)
    total_flows_muni[,yrs] = sapply(yrs, function(x) round(total_flows_muni[,x] / crop_area[, x], 1))
  }
  
  
  if (convert_to_DM == TRUE) {
    # using a typical DM content of 0.8
    # kg GM  ha-1 yr-1
    total_flows_muni[, yrs]= sapply(yrs, function(x) round(total_flows_muni[,x]/(0.80 * (18 / 1000)), 1))
    
  }
  
  total_flows_muni = data_cleaning(total_flows_muni)
  
  return(total_flows_muni)
  rm(list=c('crop_area','yrs'))
}


