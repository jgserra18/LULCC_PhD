source('./Nutrients/Model/Soil_losses/Runoff/1_Nutrient_runoff_recentApplication.R')
source('./Nutrients/Model/MMS/Gross_manure/Compute_Nutrient_excretion.R')


// INCLUDE ALL ANIMALS
// REDUCE FRAC_GRAZING BY ORDER
// FIRST REDUCE DAIRY COWS, THEN PIGS/POULTRY, THEN BOVINE, THEN HORSES, THEN SHEEP/GOATS






require('doParallel')

#' @details this script adjust the FRAC_grazing of bovine, pigs and poultry, according to different agrarian regions,
#' according to a user-defined threshold (default is 1500 kg N ha-1 yr-1)
#' if the N excreted onto extensive pastures (i.e., Grassland) is higher than this, the FRAC_grazing is iteractively reduced 
#' if the threshold is still surpassed when FRAC_grazing is 0, this is allowed
#' horses, sheep and goats grazing fractions are not changed



set_yrs = paste0('X',seq(1987,2017))


total_default_nutrient_graz = function(nutrient, pathway='Grazing') {
  #' @param nutrient N
  #' @description  general function to ocmpute gross manure allocation
  #' @param pathway is set to grazing
  #' @return the total N excreted onto pastures (extensive and intensive) (kg N yr-1)

  
  yrs  = paste0('X',seq(1987,2017))
  store <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  store[, yrs] <- sapply(yrs, function(x) store[,x] = 0)
  
  standard_params <- get_standard_params_list(main_param = 'Animals')
  
  for (i in 1:nrow(standard_params)) {
    
    main_param <- standard_params[i, 'Main_animals']
    param <- standard_params[i, 'Animals']
    
    FRAC_pathway_animal = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Animals', subfolderX2 = 'Downscaled_distribution', subfolderX3 = pathway, subfolderX4 = main_param, pattern = param)
    
    # get gross man N
    man_nutrient <- compute_animal_nutrient_excretion(nutrient, main_param, param)
    pathway_N <- man_nutrient
    
    yrs <- paste0('X', seq(1987,2017))
    pathway_N[, yrs] <- sapply(yrs, function(x) round(pathway_N[, x] * FRAC_pathway_animal[, x], 1))
    store[, yrs] <- sapply(yrs, function(x) pathway_N[,x] + store[,x])
  }
  store = compute_FRAC_pastures_nutrient_flows(nutrient_flow_df = store, pasture_type = 'Extensive_pasture')
  
  return(store)
  rm(list=c('pathway_N'))
}


compute_nutrient_graz_per_area = function(nutrient = 'N', pathway = 'Grazing') {
  #' @param self explanatory
  #' @description calculate total N excreted onto grazing on a per extensive pasture hectare
  #' @return total N excreted on extensive pastures (kg N ha-1 yr-1)
  
  yrs = paste0('X',seq(1987,2017))
  default_grazN = total_default_nutrient_graz(nutrient, pathway) # total kg N in grazed pastures
  default_grazN_extensive = compute_FRAC_pastures_nutrient_flows(nutrient_flow_df = default_grazN, pasture_type = 'Extensive_pasture') # allocate flows for extensive pasture
  
  ext_pasture = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = 'Pastures', pattern = 'Extensive_pasture') # in ha
  default_grazN_extensive[, yrs] = sapply(yrs, function(x) round(default_grazN_extensive[,x] / ext_pasture[,x], 1))
  
  return(default_grazN_extensive)
  rm(list=c('yrs','default_grazN','ext_pasture'))
}


identify_default_surpass_threshold = function(threshold = 1500, calc_df, nutrient = 'N') {
  #' @param threshold threshold of maximum allowed limite on N excreted onto pastures (kg N ha-1 yr-1)
  #' @param calc_df dataframe to identify municipalities and years where thr threshold is surpassed
  #' @description Identifies municipalities/years where the threshold is surpassed
  #' @return returns the position of municiaplities where N in extensive pasture exceeds the threshold
  
  ids = as.data.frame(which(calc_df>threshold, arr.ind = T))
  ids = subset(ids, col>3) # first 4 cols dont count
  
  return(ids)
}


identify_agrarian_region = function(row) {
  #' @param row input row to identify the agrarian region
  #' @description finds the agrarian region of a municipality in a given row from calc_df
  #' @details agrarian regions - "Entre Douro e Minho" "Trás-os-Montes", "Beira Litoral", "Beira Interior", "Ribatejo e Oeste", "Alentejo", "Algarve" 
  #' @return the agrarian region of a given municipality 
  
  spatial_df = get_activity_data(module = 'Nutrients', subfolder = 'Raw_data_Municipality', pattern = 'Spatial_disaggregation')
  agrarian_region = spatial_df[row, 'agrarian_region' ]
  
  return(agrarian_region)
  rm(list='spatial_df')
}


set_conditions = function(agrarian_region) {
  #' @param agrarian_region is either "Entre Douro e Minho" "Trás-os-Montes", "Beira Litoral", "Beira Interior", "Ribatejo e Oeste", "Alentejo", "Algarve" 
  #' @description establish algorithm rules to reduce grazing fraction (reduce bovine, pigs, poultry or dairy)
  #' @return a vector, for each agrarian region, with the ordered animal cats to change

  
  if (agrarian_region == 'Entre Douro e Minho') { animal_cats = c('Poultry','Pigs','Bovine') }
  else if (agrarian_region == 'Trás-os-Montes') { animal_cats = c('Bovine','Poultry','Pigs') } 
  else if (agrarian_region == 'Beira Litoral') { animal_cats = c('Poultry','Pigs','Bovine','Dairy_cows') } 
  else if (agrarian_region == 'Beira Interior') { animal_cats = c('Poultry','Pigs','Bovine','Dairy_cows') } 
  else if (agrarian_region == 'Ribatejo e Oeste') { animal_cats = c('Pigs','Poultry','Bovine','Dairy_cows') } 
  else if (agrarian_region == 'Alentejo') { animal_cats = c('Poultry','Dairy_cows','Bovine','Pigs') } 
  else if (agrarian_region == 'Algarve') { animal_cats = c('Bovine','Pigs','Poultry','Dairy_cows') } 
  
  return(animal_cats)
}


get_animal_categories = function(animal_cats) {
  #' @param animal_cats the animal categories from set_conditions()
  #' @return returns the animal species from a given animal category
  
  
  if (animal_cats == 'Poultry') { animals = c('Broilers','Ducks','Geese','Rep_hens','Laying_hens','Turkeys') }
  else if (animal_cats == 'Pigs') { animals  = c('Non_pregnant_sows','Other_swine','Pigs_20','Pigs_50','Pregnant_sows') }
  else if (animal_cats == 'Bovine') { animals  = c('Male_calf_2','Male_calf_1-2','Non_dairy','Other_calf','Beef_calf','Female_calf_2','Female_calf_1-2','Female_calf-1') }
  else if (animal_cats == 'Dairy_cows') { animals = 'Dairy_cows' }
  
  return(animals)
}


modify_FRAC_grazing_category = function(FRAC_grazing_modifier, 
                                        row,
                                        col,
                                        animal_category, 
                                        animal_species, 
                                        pathway = 'Grazing') {
  #' @param FRAC_grazing_modifier positive integer (0-1) to be used to decrease FRAC_grazing
  #' @param row row of the municipality where N excreted onto grazing surpasses the threshold previously defined
  #' @param col col of the municipality where N excreted onto grazing surpasses the threshold previously defined
  #' @param animal_category animal category for the rule-based agrarian regions
  #' @param animal_species a specified species of a given animal_category, for which FRAC_grazing is being modified
  #' @description A modifier is applied to the default FRAC_grazing for a given position, and for a given animal specieis
  #' @return a modified FRAC_grazing for a given position (Municipality, year) and animal_species; returns 0 if negative
  
  if (animal_category == 'Dairy_cows') {
    FRAC_grazing = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Animals', subfolderX2 = 'Downscaled_distribution', subfolderX3 = pathway, subfolderX4 = 'Bovine', pattern = 'Dairy_cows')
    FRAC_grazing = FRAC_grazing[, c('Muni_ID','ID','Muni', paste0('X',seq(1987,2017)))]
  }
  else {
    FRAC_grazing = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Animals', subfolderX2 = 'Downscaled_distribution', subfolderX3 = pathway, subfolderX4 = animal_category, pattern = animal_species)
  }
  
  # specify grazing fraction for a given position (where the grazN is higher than the threshold)
  FRAC_grazing = FRAC_grazing[row, col]
  # apply modififier
  FRAC_grazing = FRAC_grazing - FRAC_grazing_modifier
  FRAC_grazing = ifelse(FRAC_grazing<0, 0, FRAC_grazing)
  
  return(FRAC_grazing)
}


compute_static_animal_Nexcretion = function( row, col, pathway = 'Grazing', nutrient ='N') {
  #' @param row row of the municipality where N excreted onto grazing surpasses the threshold previously defined
  #' @param col col of the municipality where N excreted onto grazing surpasses the threshold previously defined
  #' @description calculates the fixed N excreted by static animal categories (see below) ; this value must be added when calculating the N required to avoid exceeding the threshold
  #' @return returns the fixed N  excreted by static animal categories (kg N yr-1)

  main_params = c('Equides','Goats','Rabbits','Sheep')
  standard_params <- get_standard_params_list('Animals')
  standard_params = standard_params[which(standard_params$Main_animals %in% main_params), ]
  
  fixed_Nexcreted = 0
  
  for (i in 1:nrow(standard_params)) {
    
    main_param = standard_params[i,2]
    param = standard_params[i,1]
    FRAC_pathway_animal = set_FRAC_pathway(pathway, main_param, param)
    Nexct = compute_animal_nutrient_excretion(nutrient, main_param, param)
    
    # compute grazN and allocate N flows to extensive pastures
    Nexct[,set_yrs]=sapply(set_yrs, function(x) FRAC_pathway_animal[,x] * Nexct[,x])
    Nexct = compute_FRAC_pastures_nutrient_flows(Nexct, 'Extensive_pasture', row, col)
    
    fixed_Nexcreted = fixed_Nexcreted + Nexct
  }
  
  return(fixed_Nexcreted)
}




general_grazN_animal_category_position = function(FRAC_grazing_modifier = 0.01, #default to 1% 
                                         row,
                                         col,
                                         animal_category, 
                                         nutrient = 'N', 
                                         pathway = 'Grazing') {
  #' @param FRAC_grazing_modifier the modifier number to decrease FRAC_grazing default
  #' @param row row of the municipality where N excreted onto grazing surpasses the threshold previously defined
  #' @param col col of the municipality where N excreted onto grazing surpasses the threshold previously defined
  #' @param animal_category animal category to calculate its total N excreted onto pasture
  #' @description calculates a modified-reduced version of total N excreted onto pastures for a given position (row = Municipality, col = year)
  #' @description sums the modified-reduced version of ottal N excreted onto pastures for a given animal_category (ie, sum of its species)
  #' @return a positive integer (>=0) that is the accumulated, modified version of the total N excreted onto pastures for a given animal category and position
  
  animals = get_animal_categories(animal_category)
  fixed_grazN = compute_static_animal_Nexcretion(row, col)
  modified_grazN = 0
  
  for (animal in animals) {
    
    if (animal_category == 'Dairy_cows') { 
      tot_Nexct = compute_animal_nutrient_excretion(nutrient, 'Bovine', 'Dairy_cows') # total N excreted of a given animal species 
    }
    else {
      tot_Nexct = compute_animal_nutrient_excretion(nutrient, animal_category, animal) # total N excreted of a given animal species
    }
    tot_Nexct = tot_Nexct[row, col] # subset to the specified position
    FRAC_grazing = modify_FRAC_grazing_category(FRAC_grazing_modifier, row, col, animal_category, animal)
    
    modified_grazN = modified_grazN + (tot_Nexct * FRAC_grazing) # update sum of modified grazN for the specified animal_category
  }
  modified_grazN = modified_grazN * compute_pastures_FRAC(param = 'Extensive_pasture', row = row, col = col)
  modified_grazN = modified_grazN + fixed_grazN # add the fixed N excreted by static animal categories
  
  return(modified_grazN)
  rm(list=c('animals','tot_Nexct','FRAC_grazing'))
}



compute_total_updated_grazN = function(mod_FRAC_vector, 
                                       animal_cat_vector, 
                                       row, 
                                       col) {
  #' @param mod_FRAC_vector vector with FRAC_grazing modifier for animal categories X 
  #' @param animal_cat_vector vector with animal categories to be applied mod_FRACX for the Xth animal category
  #' @description Calculates updated total N excreted onto grazing using updated FRAC grazing
  #' @details this is necessary as when looping through the different animal categories, there may be useful to ahve 2 different modifiers
  #' @return returns the new total N excreted onto grazing; to be compared to the threshold
  #' @note N flows already allocated to extensive pasture
  
  ext_pasture = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = 'Pastures', pattern = 'Extensive_pasture') # in ha
  ext_pasture = ext_pasture[row, col]
  
  updated_grazN = 0
  for (i in 1:length(mod_FRAC_vector)) {
    
    updated_grazN = updated_grazN + general_grazN_animal_category_position(mod_FRAC_vector[[i]], row, col, animal_cat_vector[[i]])
  }
  
  updated_grazN = updated_grazN / ext_pasture
  return(updated_grazN)
}



tune_FRAC_grazing_modifier = function(threshold = 1500, nutrient = 'N', pathway = 'Grazing', write=FALSE, FRAC_modifier = 0.02, ...) {
  #' @param threshold threshold of maximum allowed limite on N excreted onto pastures (kg N ha-1 yr-1) ; from identify_default_surpass_threshold()
  #' @param FRAC_modifier fraction (0-1) to change the FRAC_grazing for the animals
  #' @param ... to add new arguments just in case
  #' @description calculates the FRAC_grazing modifier for all animal categories
  #' it is possible  that no animal other than horses, sheep and goats can graze; if so mod is set to 1 (ie, the threshold is always passed)
  #' @return returns a dataframe with the rows, cols and FRAC_modifier
  
  default_grazN = compute_nutrient_graz_per_area(nutrient, pathway) # calculate default N excreted onto pastures
  ids = identify_default_surpass_threshold(threshold = threshold, calc_df = default_grazN) # find position of municipalities higher than the threshold
  store = data.frame(rows='', cols='', NEW_FRAC_GRAZ='')

  for (i in 1:nrow(ids)) {
    print(paste0('Row ', i, '============'))
    row = ids[i, 1]
    col = ids[i, 2]
    
    agrarian_region = identify_agrarian_region(row = row)
    animal_alloc_order = set_conditions(agrarian_region = agrarian_region) # set animal categories ordered-conditions 
    
    mod = vector(mode = 'numeric', length = length(animal_alloc_order)) # FRAC_grazing modifier for 4 animal cats
    
    tot_mod_grazN = default_grazN[row, col] # initially set to the default position of default_grazN
    ctr = 0
    
    v_FRAC_modifier = c(0, 0.98, seq(0 + FRAC_modifier,0.98, FRAC_modifier)) # vector containing FRAC_modifiers
    while (tot_mod_grazN > threshold) {
      
      ctr = ctr + 1
      print(ctr)
      tot_mod_grazN = compute_total_updated_grazN(mod, animal_alloc_order, row, col)
      mod = mod + v_FRAC_modifier[ctr] # increased by 0.02 (ie, 2%)
      
      if (mod == 1 & (tot_mod_grazN > threshold)==TRUE)  { # then no grazing animal is allocated, ie mod = 1
        print(paste0('Total modelled grazN is ', tot_mod_grazN, ';; while threshold is ', threshold))
        mod = 1
        break 
      }
    }
    
    store[i, seq(1,3)] = c(row, col, mod) # row 
  }
  
  if (write==TRUE) {
    
    export_file(module = 'Nutrients', folder = 'Activity_data', subfolder = 'General_params', subfolderX2 = 'Animals', subfolderX3 = 'Grazing_algorithm', subfolderX4 = paste0('Threshold_',threshold), file = store, filename = 'FRAC_grazing_modifier')
  }
  
  return(store)
  rm(list=c('ids','mod','agrarian_region','animal_alloc_order','tot_mod_grazN'))
}

threshold=1000
nutrient='N'
pathway='Grazing'
FRAC_modifier = 0.5
tune_FRAC_grazing_modifier_parallel = function(threshold = 1500, nutrient = 'N', pathway = 'Grazing', write=FALSE, FRAC_modifier = 0.02,...) {
  #' @param threshold threshold of maximum allowed limite on N excreted onto pastures (kg N ha-1 yr-1) ; from identify_default_surpass_threshold()
  #' @param ... to add new arguments just in case
  #' @description calculates the FRAC_grazing modifier for all animal categories using parallel processing (3 cpus used by default)
  #' @return returns a dataframe with the rows, cols and FRAC_modifier
  #' @example tune_FRAC_grazing_modifier_parallel(write=T)
  
  
  default_grazN = compute_nutrient_graz_per_area(nutrient, pathway) # calculate default N excreted onto pastures
  ids = identify_default_surpass_threshold(threshold = threshold, calc_df = default_grazN) # find position of municipalities higher than the threshold
  store = data.frame(rows='', cols='', NEW_FRAC_GRAZ='')
  
  cl = makePSOCKcluster(3)
  registerDoParallel(cl)

  # start parallel
  store = foreach(i=1:nrow(ids), 
                  .export = c('store', 'default_grazN', 'ids', 'threshold', 'get_activity_data','get_mainfolder_sub',
                              'set_yrs', 'compute_pastures_FRAC','compute_static_animal_Nexcretion',
                              'data_cleaning',
                              'compute_static_animal_Nexcretion','get_standard_params_list',
                              'set_FRAC_pathway','compute_FRAC_pastures_nutrient_flows',
                              'identify_agrarian_region', 'set_conditions', 'compute_total_updated_grazN',
                              'compute_animal_nutrient_excretion','select_animal_Nutrient_excretion_coefficient',
                              'identify_read_fileclass','general_grazN_animal_category_position','get_animal_categories','modify_FRAC_grazing_category'),
                  .combine = rbind) %dopar% {
  
    
      row = ids[i, 1]
      col = ids[i, 2]
      
      agrarian_region = identify_agrarian_region(row = row)
      animal_alloc_order = set_conditions(agrarian_region = agrarian_region) # set animal categories ordered-conditions 
      
      mod = vector(mode = 'numeric', length = length(animal_alloc_order)) # FRAC_grazing modifier for 4 animal cats
      
      tot_mod_grazN = default_grazN[row, col] # initially set to the default position of default_grazN
      ctr = 0
      v_FRAC_modifier = c(0, 0.98, seq(0 + FRAC_modifier,0.98, FRAC_modifier)) # vector containing FRAC_modifiers
      
      while (tot_mod_grazN > threshold) {
        
        ctr = ctr + 1
        print(ctr)
        tot_mod_grazN = compute_total_updated_grazN(mod, animal_alloc_order, row, col)
        mod = mod + v_FRAC_modifier[ctr] # increased by 0.02 (ie, 2%)
        
        if (mod == 1 & (tot_mod_grazN > threshold)==TRUE)  { # then no grazing animal is allocated, ie mod = 1
          mod = 1
          break 
        }
      }
      data.frame(rows=row,
                 cols = col, 
                 FRAC_GRAZ_mod = mod[1])
  }
  # finish parallel
  stopCluster(cl) 
  
  if (write==TRUE) {
    
    export_file(module = 'Nutrients', folder = 'Activity_data', subfolder = 'General_params', subfolderX2 = 'Animals', subfolderX3 = 'Grazing_algorithm', subfolderX4 = paste0('Threshold_',threshold), file = store, filename = 'FRAC_grazing_modifier')
  }
  
  return(store)
  rm(list=c('ids','mod','agrarian_region','animal_alloc_order','tot_mod_grazN'))
}




apply_FRAC_grazing_modifier_individual = function(row, col, FRAC_grazing_modifier, animal_category, animal_species, pathway='Grazing') {
  #' @param row row of the municipality where N excreted onto grazing surpasses the threshold previously defined
  #' @param col col of the municipality where N excreted onto grazing surpasses the threshold previously defined
  #' @param FRAC_grazing_modifier final FRAC_grazing_modifier for the specified row (Muni) and col (year) for the animal categories
  #' @param animal_category equivalent to main_param  
  #' @param animal_category equivalent to param 
  #' @description applies the calculated FRAC_grazing_modifier for a row and col for a given animal category and animal species
  #' @return the adjusted/corrected FRAC_grazing after applying FRAC_grazing_modifier
  #' @example apply_FRAC_grazing_modifier_individual(row=6, col=4, FRAC_grazing_modifier=0.01, animal_category='Bovine',animal_species='Male_calf_2)
  
  if (animal_category == 'Dairy_cows') {
    FRAC_pathway_animal = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Animals', subfolderX2 = 'Downscaled_distribution', subfolderX3 = pathway, subfolderX4 = 'Bovine', pattern = 'Dairy_cows')
    FRAC_pathway_animal = FRAC_pathway_animal[, c('Muni_ID','ID','Muni',paste0('X',seq(1987,2017)))]
  }
  else {
    FRAC_pathway_animal = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Animals', subfolderX2 = 'Downscaled_distribution', subfolderX3 = pathway, subfolderX4 = animal_category, pattern = animal_species)
  }
  FRAC_pathway_animal = FRAC_pathway_animal[row, col]
  new_FRAC_pathway_animal = FRAC_pathway_animal-FRAC_grazing_modifier
  print(paste0('Replacing ', FRAC_pathway_animal, 'by ', new_FRAC_pathway_animal))
  new_FRAC_pathway_animal = ifelse(new_FRAC_pathway_animal<0,0,new_FRAC_pathway_animal)
  
  return(new_FRAC_pathway_animal)
}


apply_FRAC_grazing_modifier_all = function(threshold = 1500, pathway='Grazing', ...) {
  #' @param threshold threshold of maximum allowed limite on N excreted onto pastures (kg N ha-1 yr-1) ; from identify_default_surpass_threshold()
  #' @param ... to add new arguments just in case
  #' @description loops, for each animal category presented in the rules, the especific position (row, col) where grazing N exceeds the threshold defined
  #' @description it updates the old vlaue according to the FRAC_grazing_modifier so N excreted onto grazing is within the threshold
  #' @return exports the updates FRAC_grazing
  
  mod_file = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Animals', subfolderX2 = 'Grazing_algorithm', subfolderX3 = paste0('Threshold_', threshold), pattern = 'grazing_modifier')
  animal_cats = c('Bovine','Pigs','Poultry','Dairy_cows')
  
  for (main_param in animal_cats) {
    
    params = get_animal_categories(main_param)
    
    for (param in params) {
      print(paste0(param, '======================'))
      FRAC_grazing = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Animals', subfolderX2 = 'Downscaled_distribution', subfolderX3 = pathway, subfolderX4 = ifelse(param == 'Dairy_cows','Bovine',main_param), pattern = param)
      FRAC_grazing = FRAC_grazing[, c('Muni_ID','ID','Muni', paste0('X',seq(1987,2017)))]
      
      for (i in 1:nrow(mod_file)) {
        
        row = mod_file[i,1]
        col = mod_file[i,2]
        FRAC_grazing_modifier = mod_file[i,3]
        agrarian_region = identify_agrarian_region(row)
        animal_categories = set_conditions(agrarian_region)
        
        # if categories exists in that municipality (ie, included in the animal-based rules)
        # updated its FRAC_grazing for the position where it exists
        # otherwise pass to the next row of mod_file
        if ((main_param %in% animal_categories) == T) { 
          print(paste0('Updating for ', param, ';; position ', row, ', ', col))
          FRAC_grazing[row, col] = apply_FRAC_grazing_modifier_individual(row, col, FRAC_grazing_modifier, main_param, param)
          FRAC_grazing = data_cleaning(FRAC_grazing)
        }
      }
      export_file(module = 'Nutrients', folder = 'Activity_data', subfolder = 'General_params', subfolderX2 = 'Animals', subfolderX3 = 'Grazing_algorithm', subfolderX4 = paste0('Threshold_',threshold), subfolderX5 = 'Updated_FRAC_grazing', subfolderX6 = main_param, file = FRAC_grazing, filename = param)
    }
  }
}
