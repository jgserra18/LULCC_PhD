source('./Nutrients/Model/Soil_losses/Runoff/1_Nutrient_runoff_recentApplication.R')
source('./Nutrients/Model/MMS/Gross_manure/Compute_Nutrient_excretion.R')



require('doParallel')

#' @details this script adjust the FRAC_grazing of bovine, pigs and poultry, according to different agrarian regions,
#' according to a user-defined threshold (default is 800 kg N ha-1 yr-1)
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
    print(param)
    FRAC_pathway_animal = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Animals', subfolderX2 = 'Downscaled_distribution', subfolderX3 = pathway, subfolderX4 = main_param, pattern = param)
    
    # get gross man N
    man_nutrient <- compute_animal_nutrient_excretion(nutrient, main_param, param)
    pathway_N <- man_nutrient
    
    yrs <- paste0('X', seq(1987,2017))
    pathway_N[, yrs] <- sapply(yrs, function(x) round(pathway_N[, x] * FRAC_pathway_animal[, x], 1))
    store[, yrs] <- sapply(yrs, function(x) pathway_N[,x] + store[,x])
  }

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


identify_default_surpass_threshold = function(threshold = 800, calc_df, nutrient = 'N') {
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

  
  if (agrarian_region == 'Entre Douro e Minho') { animal_cats = c('Poultry','Pigs','Bovine', 'Equides','Non_dairy','Goats','Sheep') }
  else if (agrarian_region == 'Trás-os-Montes') { animal_cats = c('Bovine','Poultry','Pigs', 'Equides','Non_dairy','Goats','Sheep') } 
  else if (agrarian_region == 'Beira Litoral') { animal_cats = c('Poultry','Pigs','Bovine','Dairy_cows', 'Equides','Non_dairy','Goats','Sheep') } 
  else if (agrarian_region == 'Beira Interior') { animal_cats = c('Poultry','Pigs','Bovine','Dairy_cows', 'Equides','Non_dairy','Goats','Sheep') } 
  else if (agrarian_region == 'Ribatejo e Oeste') { animal_cats = c('Pigs','Poultry','Dairy_cows','Bovine', 'Equides','Non_dairy','Goats','Sheep') } 
  else if (agrarian_region == 'Alentejo') { animal_cats = c('Poultry','Dairy_cows','Pigs','Bovine', 'Equides','Non_dairy','Goats','Sheep') } 
  else if (agrarian_region == 'Algarve') { animal_cats = c('Pigs','Poultry','Dairy_cows', 'Bovine','Equides','Non_dairy','Goats','Sheep') } 
  
  return(animal_cats)
}


get_animal_categories = function(animal_cats) {
  #' @param animal_cats the animal categories from set_conditions()
  #' @return returns the animal species from a given animal category
  
  if (animal_cats == 'Dairy_cows' | animal_cats == 'Non_dairy') { animals = animal_cats }
  else {
    standard_params = get_standard_params_list('Animals')
    animals = subset(standard_params, Main_animals==animal_cats)[,1]
    
  }
  
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
  
  if (animal_species == 'Dairy_cows') {
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

general_grazN_animal_species_position = function(FRAC_grazing_modifier = 0.01, #default to 1% 
                                         row,
                                         col,
                                         animal_category, 
                                         animal_species,
                                         nutrient = 'N', 
                                         pathway = 'Grazing') {
  #' @param FRAC_grazing_modifier the modifier number to decrease FRAC_grazing default
  #' @param row row of the municipality where N excreted onto grazing surpasses the threshold previously defined
  #' @param col col of the municipality where N excreted onto grazing surpasses the threshold previously defined
  #' @param animal_category animal category to calculate its total N excreted onto pasture
  #' @description calculates a modified-reduced version of total N excreted onto pastures for a given position (row = Municipality, col = year)
  #' @description calculates this for the specified animal species
  #' @return a positive integer (>=0) that is the accumulated, modified version of the total N excreted onto pastures for a given animal species and position
  #' @note unit: kg N yr-1
  
  modified_grazN = 0
  # total N excreted per animal species
  if (animal_species == 'Dairy_cows' | animal_species == 'Non_dairy') {
    
    tot_Nexct = compute_animal_nutrient_excretion(nutrient, 'Bovine', animal_species) # total N excreted of a given animal species 
    tot_Nexct = tot_Nexct[row, col] # subset to the specified position
    FRAC_grazing = modify_FRAC_grazing_category(FRAC_grazing_modifier, row, col, 'Bovine', animal_species) # modifies FRAC_grazing
  }
  else {
    
    tot_Nexct = compute_animal_nutrient_excretion(nutrient, animal_category, animal_species) # total N excreted of a given animal species
    tot_Nexct = tot_Nexct[row, col] # subset to the specified position
    FRAC_grazing = modify_FRAC_grazing_category(FRAC_grazing_modifier, row, col, animal_category, animal_species) # modifies FRAC_grazing
  }

  modified_grazN = modified_grazN + (tot_Nexct * FRAC_grazing) # update sum of modified grazN for the specified animal_category
  modified_grazN = modified_grazN * compute_pastures_FRAC(param = 'Extensive_pasture', row = row, col = col) # allocate flows to extensive pastures

  return(modified_grazN)
  rm(list=c('animals','tot_Nexct','FRAC_grazing'))
}



compute_total_updated_grazN = function(mod_FRAC_vector, 
                                       animal_cat_vector, 
                                       animal_species_vector,
                                       row, 
                                       col,
                                       parallel=F) {
  #' @param mod_FRAC_vector vector with FRAC_grazing modifier for animal categories X 
  #' @param animal_cat_vector vector with animal categories to be applied mod_FRACX for the Xth animal category
  #' @param animal_species_vector vector with animal species to be applied mod_FRACX for the Xth animal category
  #' @description Calculates updated total N excreted onto grazing using updated FRAC grazing
  #' @details this is necessary as when looping through the different animal categories, there may be useful to ahve 2 different modifiers
  #' @return returns the new total N excreted onto grazing; to be compared to the threshold
  #' @note N flows already allocated to extensive pasture (kg N ha-1 yr-1)
  
  ext_pasture = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = 'Pastures', pattern = 'Extensive_pasture') # in ha
  ext_pasture = ext_pasture[row, col]
  
  updated_grazN = 0
  
  if (parallel==TRUE) {
    
    require('doParallel')
    cl = makePSOCKcluster(3)
    registerDoParallel(cl)

    updated_grazN = foreach(i=1:length(mod_FRAC_vector),
                            .export = c('get_activity_data','identify_read_fileclass',
                                        'get_mainfolder_sub','general_grazN_animal_species_position','compute_animal_nutrient_excretion',
                                        'set_yrs', 'compute_pastures_FRAC','select_animal_Nutrient_excretion_coefficient',
                                        'data_cleaning','get_standard_params_list','modify_FRAC_grazing_category',
                                        'set_FRAC_pathway','compute_FRAC_pastures_nutrient_flows',
                                        'general_grazN_animal_species_position'),
                            .combine='+') %dopar% {
                              
                              updated_grazN+ general_grazN_animal_species_position(mod_FRAC_vector[[i]], row, col, animal_cat_vector[[i]], animal_species_vector[[i]])      
                            }
    stopCluster(cl) 
  }
  else {
    for (i in 1:length(mod_FRAC_vector)) {
      
       updated_grazN = updated_grazN + general_grazN_animal_species_position(mod_FRAC_vector[[i]], row, col, animal_cat_vector[[i]], animal_species_vector[[i]])
    }
  }
  
  updated_grazN = updated_grazN / ext_pasture
  return(updated_grazN)
}



create_input_data_vectors = function(animal_alloc_order) {
  #' @param animal_alloc_order vector with the order of animals to be allocated in a given position (ie, agrarian region condition)
  #' @description creates an ordered dataframe according to the order of animal_alloc_order with Animals, Main_animals
  #' @return returns description
  
  
  store = data.frame(Animals = NULL, Main_animals = NULL)
  standard_params = get_standard_params_list('Animals')

  for (animal_cat in animal_alloc_order) {
    
    if (animal_cat == 'Dairy_cows' | animal_cat == 'Non_dairy') {
      store = rbind(store, c(animal_cat, animal_cat))
    }
    else {    
      animals = subset(standard_params, Main_animals == animal_cat)
      if (animal_cat == 'Bovine') { animals = animals[-which(animals$Animals %in% c('Dairy_cows', 'Non_dairy')), ] }
      store = rbind(store, animals)
    }
  }
  
  return(store)
}


find_threshold_FRAC_modifier = function(mod_FRAC_vector, 
                                        animal_cat_vector, 
                                        animal_species_vector,
                                        row, 
                                        col,
                                        FRAC_modifier,
                                        threshold = 800,
                                        parallel = F) {
  #TODO UPDATE DESCRIPTIONS
  #' @param mod_FRAC_vector vector with FRAC_grazing modifier for animal categories X 
  #' @param animal_cat_vector vector with animal categories to be applied mod_FRACX for the Xth animal category
  #' @param animal_species_vector vector with animal species to be applied mod_FRACX for the Xth animal category
  #' @description Calculates updated total N excreted onto grazing using updated FRAC grazing
  #' @details this is necessary as when looping through the different animal categories, there may be useful to ahve 2 different modifiers
  #' @return returns the new total N excreted onto grazing; to be compared to the threshold
  #' @note N flows already allocated to extensive pasture (kg N ha-1 yr-1)
  
  ext_pasture = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = 'Pastures', pattern = 'Extensive_pasture') # in ha
  ext_pasture = ext_pasture[row, col]
  
  updated_grazN = 0
  check_threshold = 8000 # set to initial value very high; this will be adjusted
  
  ctr = 1 # iteraction counter
  i = 0
  while (check_threshold > threshold) {
    
    i = i + 1
    if (i > length(mod_FRAC_vector)) { i = 1; updated_grazN = 0; ctr = ctr + 1 } # i can't be bigger than the number of animals being looped; reset updated grazN
    print(paste0('Iteraction: ', ctr, '; line being looped: ',i))
    updated_grazN = updated_grazN + general_grazN_animal_species_position(mod_FRAC_vector[i], row, col, animal_cat_vector[i], animal_species_vector[i])
    mod_FRAC_vector[i] = mod_FRAC_vector[i] + FRAC_modifier
    
    # check if this new frac grazing against the threshold
    check_threshold = compute_total_updated_grazN(mod_FRAC_vector, animal_cat_vector, animal_species_vector, row, col, parallel = parallel)
    print(paste0('Now  estimated: ', check_threshold, ' vs threshold: ', threshold))
    if (check_threshold <= threshold) {
      return(mod_FRAC_vector)
    }
  }
  rm(list=c('ext_pasture','updated_grazN','check_threshold','ctr','i'))
}



tune_FRAC_grazing_modifier = function(start_i = 1, end_i, threshold = 800, nutrient = 'N', pathway = 'Grazing', write=TRUE, FRAC_modifier = 0.05, parallel=F, ...) {
  #' @param start_i index to start the looping cycle 
  #' @param end_i index to finish the loop; if missing it is srt to the nrow of ids 
  #' @description 
  #' @return 
  #' @usage 

  default_grazN = compute_nutrient_graz_per_area(nutrient, pathway) # calculate default N excreted onto pastures (kg N ha-1)
  ids = identify_default_surpass_threshold(threshold = threshold, calc_df = default_grazN) # find position of municipalities higher than the threshold
  store = data.frame(rows=NULL, cols=NULL, FRAC_GRAZ_mod=NULL, param=NULL, main_param=NULL)

  end_i = ifelse(missing(end_i)==TRUE, nrow(ids), end_i) # set end_i
  print(end_i)
  for (i in start_i:end_i) {
    print(paste0('Row ', i, '============'))
    row = ids[i, 1]
    col = ids[i, 2]
    
    agrarian_region = identify_agrarian_region(row = row)
    animal_alloc_order = set_conditions(agrarian_region = agrarian_region) # set animal categories ordered-conditions 
    alloc_df = create_input_data_vectors(animal_alloc_order)
    mod = vector(mode = 'numeric', length = nrow(alloc_df)) # FRAC_grazing modifier for all animal species analysed
    find_best_mod = find_threshold_FRAC_modifier(mod_FRAC_vector = mod, animal_cat_vector = alloc_df$Main_animals, animal_species_vector = alloc_df$Animals, row, col, FRAC_modifier = FRAC_modifier, parallel = parallel)
    
    # store params to the dataframe "store"
    store[i,1] = row; store[i,2] = col; store[i,3] = paste(find_best_mod,collapse = ','); store[i,4]= paste(alloc_df$Animals, collapse = ','); store[i,5] = paste(alloc_df$Main_animals, collapse = ',')
  }
  
  if (write==TRUE) {
    
    export_file(module = 'Nutrients', folder = 'Activity_data', subfolder = 'General_params', subfolderX2 = 'Animals', subfolderX3 = 'Grazing_algorithm', subfolderX4 = paste0('Threshold_',threshold), file = store, filename = paste0('FRAC_grazing_modifier_',end_i))
  }
  return(store)
}



tune_FRAC_grazing_modifier_parallel = function(start_i = 1, end_i, threshold = 800, nutrient = 'N', pathway = 'Grazing', write=TRUE, FRAC_modifier = 0.05,  parallel=F, ...) {
  #' @param start_i index to start the looping cycle 
  #' @param end_i index to finish the loop; if missing it is srt to the nrow of ids 
  #' @description parallel function to modify the national FRAC_grazing according to the conditions set per region and the threshold define
  #' @return 
  #' @usage tune_FRAC_grazing_modifier_parallel(start_i=1, threshold=800, parallel=F, FRAC_modifier = 0.05)
  #' @usage tune_FRAC_grazing_modifier_parallel(start_i = 1110, end_i = 1101, threshold = 800, write=T, FRAC_modifier = 0.1)

  default_grazN = compute_nutrient_graz_per_area(nutrient, pathway) # calculate default N excreted onto pastures (kg N ha-1)
  ids = identify_default_surpass_threshold(threshold = threshold, calc_df = default_grazN) # find position of municipalities higher than the threshold
  store = data.frame(rows=vector(length=nrow(ids)), 
                     cols=vector(length=nrow(ids)), 
                     NEW_FRAC_GRAZ=vector(length=nrow(ids)), 
                     param=vector(length=nrow(ids)), 
                     main_param=vector(length=nrow(ids)))
  
  require('doParallel')
  dd = makePSOCKcluster(3)
  registerDoParallel(dd)
  
  end_i = ifelse(missing(end_i)==TRUE, nrow(ids), end_i) # set end_i
  end_i = ifelse(end_i>nrow(ids), nrow(ids), end_i)
  
  # start parallel
  store = foreach(i=start_i:end_i, 
                  .export = c('store', 'default_grazN', 'ids', 'threshold', 'get_activity_data','get_mainfolder_sub','general_grazN_animal_species_position',
                              'set_yrs', 'compute_pastures_FRAC','create_input_data_vectors',
                              'data_cleaning','get_standard_params_list',
                              'set_FRAC_pathway','compute_FRAC_pastures_nutrient_flows','find_threshold_FRAC_modifier','find_threshold_FRAC_modifier',
                              'identify_agrarian_region', 'set_conditions', 'compute_total_updated_grazN',
                              'compute_animal_nutrient_excretion','select_animal_Nutrient_excretion_coefficient',
                              'identify_read_fileclass','get_animal_categories','modify_FRAC_grazing_category'),
                  .combine = rbind) %dopar% {
                    
                    row = ids[i, 1]
                    col = ids[i, 2]
                    agrarian_region = identify_agrarian_region(row = row)
                    animal_alloc_order = set_conditions(agrarian_region = agrarian_region) # set animal categories ordered-conditions 
                    
                    alloc_df = create_input_data_vectors(animal_alloc_order)
                    mod = vector(mode = 'numeric', length = nrow(alloc_df)) # FRAC_grazing modifier for all animal species analysed
                    find_best_mod = find_threshold_FRAC_modifier(mod_FRAC_vector = mod, animal_cat_vector = alloc_df$Main_animals, animal_species_vector = alloc_df$Animals, row, col, FRAC_modifier = FRAC_modifier,  parallel=parallel)
                    
                    data.frame(rows=row,
                               cols = col, 
                               FRAC_GRAZ_mod = paste(find_best_mod,collapse = ','),
                               param  =paste(alloc_df$Animals, collapse = ','),
                               main_param = paste(alloc_df$Main_animals, collapse = ','))
                  }
  # finish parallel
  stopCluster(dd) 
  if (write==TRUE) {

    export_file(module = 'Nutrients', folder = 'Activity_data', subfolder = 'General_params', subfolderX2 = 'Animals', subfolderX3 = 'Grazing_algorithm', subfolderX4 = paste0('Threshold_',threshold), file = store, filename = paste0('FRAC_grazing_modifier_',start_i,'_',end_i))
  }
  return(store)
  rm(list=c('ids','mod','agrarian_region','animal_alloc_order','tot_mod_grazN'))
}


collate_all_FRAC_grazing_modifiers = function(threshold = 800, pathway = 'Grazing') {
  #' @description compiles all the individual FRAC_grazing_modifier into one dataset
  #' @return returns @description 

  FRAC_modifier_files = list.files(path = paste0('./Nutrients/Activity_data/General_params/Animals/Grazing_algorithm/Threshold_', threshold), pattern = 'FRAC_grazing_modifier_', full.names = T)
  FRAC_modifier_files = lapply(FRAC_modifier_files, read.csv)
  FRAC_modifier_files = data.table::rbindlist(FRAC_modifier_files)
  
  return(FRAC_modifier_files)
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

apply_FRAC_grazing_modifier_all()
apply_FRAC_grazing_modifier_all = function(threshold = 800, pathway='Grazing', ...) {
  #' @param threshold threshold of maximum allowed limite on N excreted onto pastures (kg N ha-1 yr-1) ; from identify_default_surpass_threshold()
  #' @param ... to add new arguments just in case
  #' @description loops, for each animal category presented in the rules, the especific position (row, col) where grazing N exceeds the threshold defined
  #' @description it updates the old vlaue according to the FRAC_grazing_modifier so N excreted onto grazing is within the threshold
  #' @return exports the updates FRAC_grazing
  
  mod_file = collate_all_FRAC_grazing_modifiers(threshold, pathway) # file with the FRAC_grazing modifiers for all positions
  standard_params <- get_standard_params_list(main_param = 'Animals')
  standard_params = subset(standard_params, Main_animals != 'Rabbits')

  for (i in 1:nrow(standard_params)) {
    
    main_param = standard_params[i,2]
    param = standard_params[i,1]
    
    # get old FRAC_grazing; this will be modified
    FRAC_grazing = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Animals', subfolderX2 = 'Downscaled_distribution', subfolderX3 = pathway, subfolderX4 = ifelse(param == 'Dairy_cows' | param == 'Non_dairy','Bovine',main_param), pattern = param)
    FRAC_grazing = FRAC_grazing[, c('Muni_ID','ID','Muni', paste0('X',seq(1987,2017)))]

    for (j in 1:nrow(mod_file)) {
      
      row = mod_file[j,1][[1]]
      col = mod_file[j,2][[1]]
      params = strsplit(x=mod_file[j,4][[1]], ',')[[1]] # all params
      param_id = which(params == param)
      if (length(param_id)==0) {
        next 
      }
      else {
        FRAC_grazing_modifier = as.numeric(strsplit(x=mod_file[j,3][[1]], ',')[[1]][param_id])  # get the modifier for this given param
        print(FRAC_grazing_modifier)
        print(paste0('Updating for ', param, ';; position ', row, ', ', col))
        FRAC_grazing[row, col] = apply_FRAC_grazing_modifier_individual(row, col, FRAC_grazing_modifier, ifelse( param == 'Dairy_cows','Dairy_cows',main_param), param)
        FRAC_grazing = data_cleaning(FRAC_grazing)
      }
    }
    # export for all params
    export_file(module = 'Nutrients', folder = 'Activity_data', subfolder = 'General_params', subfolderX2 = 'Animals', subfolderX3 = 'Grazing_algorithm', subfolderX4 = paste0('Threshold_',threshold), subfolderX5 = 'Updated_FRAC_grazing', subfolderX6 = ifelse(param == 'Dairy_cows' | param == 'Non_dairy','Bovine',main_param), file = FRAC_grazing, filename = param)
  }
  rm(list=c('row','mod_file','col','FRAC_grazing_modifiers','params','main_params','FRAC_grazing'))
}
