source('./Main/Global_functions.R')
source('./Nutrients/Model/INE_DB/Populate_INE_AgrarianRegion.R')
source('./Main/Data_operations.R')


####################################################################################-
#* SCRIPT DOWNLOADS DATA FROM STATISTICS PORTUGAL ----------------------------------
#* PRE-PROCESS IS DONE TO SUIT THE NEEDS TO CALCULATE ENERGY REQUIREMENT -----------
####################################################################################-





# GET SHEEP AND GOAT ANIMAL SUB-CATEGORIES ---------------------------------------------------------------------------------------------------

set_subcategories_var_id = function(param) {
  
  df = data.frame(main_param = c('Sheep','Sheep','Sheep','Goats','Goats','Goats'),
                  new_param = c('Ewes_dairy','Ewes_other','Ram','Goats','Doeling','Buck'),
                  var_id = c('11','12','2','11','12','2'))
  
  df = df[which(df[, 'new_param'] == param), 3]
  return(df)
}


get_AR_subcategories_FRAC_pop = function(main_param, param) {
  # get animal populations for other sheep and goat categories to calculate energy requirements
  # exports the population at the agrarian region level
  # calculates fraction of population at the agrarian region level
  # downscales to the municipality level
  # this is used to calculate animal populations at the municipality level for 1987-2017
  # unit: head yr-1
  
  INE_param_id = ifelse(main_param == 'Goats','0000546', '0000545')
  var_id = set_subcategories_var_id(param)
  animal_pop_AR = get_agrarian_region_INE(INE_param_id = INE_param_id, var_id = var_id, year = seq(1994, 2017))
  animal_pop_AR <- sapply(animal_pop_AR, as.numeric)
  animal_pop_AR = as.data.frame(animal_pop_AR)
  animal_pop_AR = data_cleaning(animal_pop_AR)
  
  if (param == 'Ewes_dairy') {
    animal_pop_AR[, 'X2016'] = c(0,28,33,188,22,40, 0)
  }
  else if (param == 'Ram') {
    animal_pop_AR[, 'X2016'] = c(11,36,13,73,33,370,12)
  }
  else if (param == 'Ewes_other') {
    animal_pop_AR[, 'X2016'] = c(51,183,46,114,86,863,34)
  }
  yrs = paste0('X', seq(1994,2017))
  store_df = get_activity_data(module = 'Nutrients', folder = 'Raw_data_Agrarian', subfolder = 'Animals', subfolderX2 = main_param, pattern = ifelse(main_param=='Goats','Goats','Ewes'))
  tot_main_param = store_df[, yrs]

  # calculate FRAC 
  FRAC_animal_pop_AR = animal_pop_AR
  FRAC_animal_pop_AR[, yrs] = sapply(yrs, function(x) round(animal_pop_AR[, x] * 1000 / tot_main_param[, x], 3))
  store_df[, yrs] = FRAC_animal_pop_AR[, yrs]
  store_df[, paste0('X', seq(1987,1993))] = store_df[, 'X1994']
  
  export_file(module = 'Nutrients', 
              file = store_df, 
              filename = paste0('FRAC_',param), 
              folder = 'Activity_data',
              subfolder = 'General_params', 
              subfolderX2 = 'Animals', 
              subfolderX3 = 'Diet',
              subfolderX4 = 'Ruminants',
              subfolderX5 = 'Population', 
              subfolderX6 = main_param)
}


downscale_AR_subcategories_FRAC_pop = function(main_param, param) {
  # downscales sheep and goat population from the AR to the municipality scale based on the total population@municipality
  # unit: head yr-1
  
  store_df = get_activity_data(module = 'Nutrients', mainfolder = 'Activity_data', subfolder = 'General_params', subfolderX2 = 'Animals', subfolderX3 = 'Diet', subfolderX4 = 'Population', subfolderX5 = main_param, pattern = paste0('FRAC_', param))
  names(store_df)[1] = 'agrarian_region_id'
  
  disagg_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Spatial_disaggregation')
  store_df <- plyr::join(x = disagg_df, y = store_df, by = 'agrarian_region_id')
  store_df = store_df[, c('Muni_ID','ID','Muni', paste0('X', seq(1987,2017)))]
  
  tot_pop_muni =get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Animals', subfolderX2 = main_param, pattern = ifelse(main_param=='Goats','Goats','Ewes'))
  tot_pop_muni[, yrs] = sapply(yrs, function(x) round(tot_pop_muni[,x] * store_df[, x], 0))
  
  export_file(module = 'Nutrients', 
              file = tot_pop_muni,
              filename =paste0('FRAC_',param), 
              folder = 'Activity_data',
              subfolder = 'General_params', 
              subfolderX2 = 'Animals', 
              subfolderX3 = 'Diet', 
              subfolderX4 = 'Ruminants',
              subfolderX5 = 'Population', 
              subfolderX6 = main_param)
}



loop_AR_subcategories_FRAC_pop = function() {
  
  df = data.frame(main_param = c('Sheep','Sheep','Sheep','Goats','Goats','Goats'),
                  new_param = c('Ewes_dairy','Ewes_other','Ram','Goats','Doeling','Buck'))
  
  for (i in 1:nrow(df)) {
    
    main_param = df[i,1]
    param = df[i,2]
    
    get_AR_subcategories_FRAC_pop(main_param, param)
    downscale_AR_subcategories_FRAC_pop(main_param, param)
  }
}



# GET SHEEP WOOL PRODUCTION ------------------------------------------------------------------------------------------------------------

get_wool_sheep = function() {
  # unit: tonnes wool yr-1
  
  wool_df = get_agrarian_region_INE(INE_param_id = '0008959', var_id = '1', year = seq(2003,2017), muni_id = c('11','16','17','18','15'), other_params = 'Wool')
  export_file(module = 'Nutrients', 
              file = wool_df,
              filename = 'Wool_Sheep', 
              folder = 'Activity_data',
              subfolder = 'General_params', 
              subfolderX2 = 'Animals', 
              subfolderX3 = 'Ruminants',
              subfolderX4 = 'Diet', 
              subfolderX5 = 'Fibre')
}


compute_wool_perSheep = function(main_param = 'Sheep') {
  # assumption: 5% of the hseep are younglings and do not produce wool yet
  # unit: kg wool head-1 yr-1
  
  wool = get_activity_data(module = 'Nutrients', mainfolder = 'Activity_data', subfolder = 'General_params', subfolderX2 = 'Animals', subfolderX3 = 'Diet', subfolderX4 = 'Fibre', pattern = 'Wool')
  tot_pop_muni =get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Animals', subfolderX2 = main_param, pattern = 'Ewes')
  tot_pop_muni = general_sumIF_NUTS2('NUTS2', tot_pop_muni)
  
  yrs = paste0('X', seq(2003,2017))
  
  wool_head = wool
  wool_head[, yrs] = sapply(yrs, function(x) round(wool[, x] * 1000 / (tot_pop_muni[, x] * (1-0.05)), 1))
  
  return(wool_head)
}


compute_linear_extrapolation_woolPerSheep = function(main_param = 'Sheep') {
  # estimates wool produced per sheep for 1987-2017
  # unit: kg wool head-1 yr-1
  
  woold_head = compute_wool_perSheep()
  names(woold_head) <- gsub('X','', names(woold_head))
  
  # create template df with all historical years
  # and populate milk_cow years (2003-2017)
  
  df <- data.frame(nuts2_ID = seq(1,5))
  yrs <- as.character(seq(1987,2017))
  df[, yrs] <- sapply(yrs, function(x) df[,x] <- NA)
  df[, as.character(seq(2003,2017))] <- woold_head[,  as.character(seq(2003,2017))]
  
  for (i in 1:nrow(woold_head)) {
    
    # make LM prediction for the remaining years (1987,2002)  
    calc_df <- data.frame(y =as.numeric(woold_head[i,-1]), x = seq(2003,2017))
    lm_model <- lm(y ~ x, data = calc_df)
    lm_prediction <- predict(lm_model, newdata =  data.frame(x =  seq(1987,2002)))
    
    df[i, as.character(seq(1987,2002))] <- round(lm_prediction, 1)
  }
  # downscale to municipality
  df = convert_dairy_Nex_NUTS2_municipality(df)
  names(df)[-1] = paste0('X', names(df)[-1])
  
  return(df)
  rm(list=c('wool_head','yrs','calc_df','lm_model','lm_prediction'))
}



# GET SHEEP AND GOAT MILK PRODUCTION ---------------------------------------------------------------------------------------------------


source('./Nutrients/Model/MMS/Gross_manure/Dairy_cows_Nexcretion.R')

get_sheep_goat_dairy_production = function() {
  # exports these data
  
  main_param = c('Sheep','Goats')
  sapply(main_param, function(x) get_historical_dairy_milk_production(x))
}


compute_milk_perSheepGoat = function(main_param, param) {
  # assumption: only 10% of goats are lactating
  # unit: milk per head-1 yyr-1
  
  if (param == 'Ewes_dairy' | param == 'Goats') {
    
    milk_prod = get_activity_data(module = 'Nutrients', pattern = main_param, folder = 'General_params', subfolder = 'Animals', subfolderX2 = 'Milk_production')
    animal_pop = get_activity_data(module = 'Nutrients', mainfolder = 'Activity_data', subfolder = 'General_params', subfolderX2 = 'Animals', subfolderX3 = 'Diet', subfolderX4 = 'Ruminants', subfolderX5 = 'Population', subfolderX6 = main_param, pattern = paste0('FRAC_', param))
    animal_pop = general_sumIF_NUTS2('NUTS2', animal_pop)

    # subset based on milk data availability
    yrs = paste0('X', seq(2003, 2017))
    
    pop_modifier = ifelse(main_param == 'Goats', 0.1, 1)
    
    milk_hd = milk_prod
    milk_hd[,yrs] = sapply(yrs, function(x) round(milk_prod[, x] * 1000 / (animal_pop[,x] *pop_modifier), 1))
    milk_hd = data_cleaning(milk_hd)
    
    return(milk_hd)
    rm(list=c('milk_prod','yrs','pop_modifier','animal_pop'))
  }
  else {
    stop('Check param!')
  }
}



compute_linearl_extrapolation_milk_perSheepGoat = function(main_param, param) {
  # rule: if negative, assume min value
  # unit: kg milk head-1 yr-1
  # for 1987-2017
  
  milk_per_hd = compute_milk_perSheepGoat(main_param, param)
  names(milk_per_hd) <- gsub('X','', names(milk_per_hd))
  
  # create template df with all historical years
  # and populate milk_cow years (2003-2017)
  
  df <- data.frame(nuts2_ID = seq(1,5))
  yrs <- as.character(seq(1987,2017))
  df[, yrs] <- sapply(yrs, function(x) df[,x] <- NA)
  df[, as.character(seq(2003,2017))] <- milk_per_hd[,  as.character(seq(2003,2017))]
  
  for (i in 1:nrow(milk_per_hd)) {
    
    # make LM prediction for the remaining years (1987,2002)  
    calc_df <- data.frame(y =as.numeric(milk_per_hd[i,-1]), x = seq(2003,2017))
    lm_model <- lm(y ~ x, data = calc_df)
    lm_prediction <- predict(lm_model, newdata =  data.frame(x =  seq(1987,2002)))
    
    df[i, as.character(seq(1987,2002))] <- round(lm_prediction, 1)
  }
  
  
  threshold_param = ifelse(main_param == 'Goats', 500, 50)
  negative_ids = which(df[, -1]<threshold_param, arr.ind = TRUE)
  
  for (j in 1:nrow(negative_ids)) {
    
    r = negative_ids[j,1]
    c = negative_ids[j,2]
    
    df[r,c] = threshold_param
  }
  
  # downscale to municipality
  df = convert_dairy_Nex_NUTS2_municipality(df)
  names(df)[-1] = paste0('X', names(df)[-1])
  
  return(df)
  rm(list=c('yrs','calc_df','lm_model','lm_prediction'))
}





# GET MILK FAT AND PROTEIN CONTENTCONTENT STATISTICAL DATA ------------------------------------------------------------------------------------------------------

modify_get_INE_data = function(INE_param_id, year, muni_id, var_id, other_params) {
  
  print('Exporting INE DB ...... ')
  # convert to json file
  url <- paste0('https:/www.ine.pt/ine/json_indicador/pindica.jsp?op=2&varcd=', INE_param_id, 
                '&Dim1=', year, '&Dim2=', 
                muni_id, '&Dim3=', 
                var_id, '&lang=PT')
  url <- gsub('/', '//', url)
  json_df <- jsonlite::fromJSON(url)
  json_df = json_df[[7]][[1]][[1]][3]
  
  return(json_df)
}



get_milk_fat_content = function(INE_param_id, var_id = '1', year = seq(2003,2017), muni_id = c('11','16','17','18','15')) {
  
  
  df <- data.frame()
  
  for (i in seq_along(muni_id)) {
    
    print(paste0('Agrarian region: ', i))
    df[i, 'nuts2_ID'] <- muni_id[i]
    
    for (j in year) {
      
      print(paste0('Year: ', j))
      js_year <- paste0('S7A', j)
      df[i, as.character(j)] <- modify_get_INE_data(INE_param_id, js_year, muni_id[i], var_id, other_params)
    }
  }
  
  # fill the datafram assumint that for 1987-2002 the value of 2003
  yrs = as.character(seq(1987,2002))
  df[, yrs] = sapply(yrs, function(x) df[,x] = df[, '2003'])
  
  # correct positions with "-" to the minimum of that row
  pos_idx = which(df =='-', arr.ind = T)
  
  for (j in 1:nrow(pos_idx)) {
    
    r = pos_idx[j,1]
    c = pos_idx[j,2]
    
    df[r,c] = min(as.integer(gsub('-',NA, df[r,-1])), na.rm=T)
  }
  names(df)[-1] = paste0('X', names(df)[-1])
  
  # disaggregate to the municipality level
  disagg_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Spatial_disaggregation')
  df <- plyr::join(x = disagg_df, y = df, by = 'nuts2_ID')
  df = df[, c('Muni_ID','ID','Muni', paste0('X', seq(1987,2017)))]
  
  return(df)
}


get_milk_protein_content = function() {
  # assplies the same procedure that of milk fat content for milk protein content
  # unit: %protein in milk
  milk_prot = get_milk_fat_content(INE_param_id = '0008605')
  return(milk_prot)
}

loop_milk_fat_protein_dairy_ruminants = function() {
  
  # fat
  df = data.frame(animals = c('Dairy_cows','Sheep','Goats'),
                  INE_param_id = c('0008604', '0008606','0008607'))
  
  for (i in 1:nrow(df)) {
    
    fat_content = get_milk_fat_content(df[i, 2])
    export_file(module = 'Nutrients', 
                file = fat_content, 
                filename = paste0('Fat_', df[i,1]), 
                folder = 'Activity_data',
                subfolder = 'General_params', 
                subfolderX2 = 'Animals', 
                subfolderX3 = 'Diet',
                subfolderX4 = 'Ruminants', 
                subfolderX5 = 'Milk')
    
    if (df[i,1] == 'Dairy_cows') {
      
      prot_content  = get_milk_protein_content()
      export_file(module = 'Nutrients', 
                  file = prot_content, 
                  filename = 'Prot_Dairy_cows', 
                  folder = 'Activity_data',
                  subfolder = 'General_params', 
                  subfolderX2 = 'Animals',
                  subfolderX3 = 'Diet',
                  subfolderX4 = 'Ruminants', 
                  subfolderX5 = 'Nutrient_retention')
    }
  }
}


# GET FRACTION OF chicken EGGS HATCHING -----------------------------------------------------

compute_egg_HATCH_frac = function() {
  
  egg_consumption = as.data.frame(get_agrarian_region_INE(INE_param_id = '0006997', var_id = '11', year = seq(2003,2017), muni_id = 'PT'))
  egg_hatch = as.data.frame(get_agrarian_region_INE(INE_param_id = '0006997', var_id = '21', year = seq(2003,2017), muni_id = 'PT'))
  egg_hatch = sapply(egg_hatch, as.numeric)
  egg_consumption = sapply(egg_consumption, as.numeric)
  
  FRAC_hatch = egg_hatch
  FRAC_hatch= round(egg_hatch/(egg_hatch + egg_consumption), 3)
  FRAC_hatch[1] = 'PT'
  FRAC_hatch = as.data.frame(FRAC_hatch)
  FRAC_hatch = data.table::transpose(FRAC_hatch)
  names(FRAC_hatch) = c('id', paste0('X',seq(2003,2017)))
  
  export_file(module = 'Nutrients', 
              file = FRAC_hatch,
              filename = 'FRAC_Hatch', 
              folder = 'Activity_data',
              subfolder = 'General_params', 
              subfolderX2 = 'Animals', 
              subfolderX3 = 'Diet',
              subfolderX4 = 'Chicken', 
              subfolderX5 = 'Eggs')
}




# GET EGG PRODUCTION OF CHICKEN ----------------------------------------------------




get_chicken_egg_production = function(INE_param_id = '0008954') {
  # unit: tonnes egg yr-1
  
  egg_prod = get_agrarian_region_INE(INE_param_id = '0008954', var_id = '1', year = seq(2003,2017), muni_id = c('11','16','17','18','15'))
  egg_prod = sapply(egg_prod, as.numeric)
  
  condition <- which(is.na(egg_prod)==TRUE,arr.ind = TRUE)
    
  for (j in 1:nrow(condition)) {
    
    id_1 = condition[j,1]
    id_2 = condition[j,2]
    
    egg_prod[id_1,id_2] = 0
  }
  egg_prod = as.data.frame(egg_prod)
  export_file(module = 'Nutrients', 
              file = egg_prod,
              filename = 'Egg_production_tonnes', 
              folder = 'Activity_data',
              subfolder = 'General_params', 
              subfolderX2 = 'Animals', 
              subfolderX3 = 'Chicken',
              subfolderX4 = 'Diet', 
              subfolderX5 = 'Chicken_eggs')
}


compute_eggs_perLayingHens = function(main_param, param) {
  # unit: g eggs head-1 yr-1
  
  eggs = get_activity_data(module = 'Nutrients', mainfolder = 'Activity_data', subfolder = 'General_params', subfolderX2 = 'Animals', subfolderX3 = 'Diet', subfolderX4 = 'Chicken', subfolderX5 = 'Eggs', pattern = 'Egg')
  tot_pop_muni =get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Animals', subfolderX2 = 'Poultry', pattern = 'Laying_hens')
  tot_pop_muni = general_sumIF_NUTS2('NUTS2', tot_pop_muni)
  
  yrs = paste0('X', seq(2003,2017))
  
  egg_head = eggs
  egg_head[, yrs] = sapply(yrs, function(x) round(eggs[, x] * 1e6 / tot_pop_muni[, x], 1))
  
  return(egg_head)
  rm(list=c('eggs','tot_pop_muni'))
}


compute_linear_extrapolation_eggs_perLayingHens = function(main_param, param) {
  # unit: g eggs head-1 yr-1
  
  eggs_per_hd = compute_eggs_perLayingHens(main_param, param)
  names(eggs_per_hd) <- gsub('X','', names(eggs_per_hd))
  
  # create template df with all historical years
  # and populate milk_cow years (2003-2017)
  
  df <- data.frame(nuts2_ID = seq(1,5))
  yrs <- as.character(seq(1987,2017))
  df[, yrs] <- sapply(yrs, function(x) df[,x] <- NA)
  df[, as.character(seq(2003,2017))] <- eggs_per_hd[,  as.character(seq(2003,2017))]
  
  for (i in 1:nrow(eggs_per_hd)) {
    
    # make LM prediction for the remaining years (1987,2002)  
    calc_df <- data.frame(y =as.numeric(eggs_per_hd[i,-1]), x = seq(2003,2017))
    lm_model <- lm(y ~ x, data = calc_df)
    lm_prediction <- predict(lm_model, newdata =  data.frame(x =  seq(1987,2002)))
    
    df[i, as.character(seq(1987,2002))] <- round(lm_prediction, 1)
  }
  
  df[df<0] = 0
  # downscale to municipality
  df = convert_dairy_Nex_NUTS2_municipality(df)
  names(df)[-1] = paste0('X', names(df)[-1])
  
  return(df)
  rm(list=c('yrs','calc_df','lm_model','lm_prediction'))
}





# get poultry slaughter weight -------------------------------------------------------------


get_broilers_slaughter_weight = function(INE_param_id = '0001338') {
  # unit: kg head-1 yr-1
  
  broilers_weight = get_agrarian_region_INE(INE_param_id = '0001338', var_id = '111', year = seq(2001,2017), muni_id = as.character(seq(1,7)))
  broilers_weight = sapply(broilers_weight, as.numeric)
  broilers_weight = as.data.frame(broilers_weight)
  export_file(module = 'Nutrients', 
              file = broilers_weight,
              filename = 'Broilers_slaughter_weight', 
              folder = 'Activity_data',
              subfolder = 'General_params', 
              subfolderX2 = 'Animals', 
              subfolderX3 = 'Diet',
              subfolderX4 = 'Chicken', 
              subfolderX5 = 'Weights')
}


compute_linear_extrapolation_broilers_slaughter_weight = function(main_param='Poultry', param='Broilers') {
  # unit: kg head-1 yr-1
  
  weight_per_hd = get_activity_data(module = 'Nutrients', mainfolder = 'Activity_data', folder = 'General_params', subfolder = 'Animals', subfolderX2 = 'Diet', subfolderX3 = 'Chicken', subfolderX4 = 'Weights', pattern = 'Broilers_slaughter')
  condition <- which(is.na(weight_per_hd)==TRUE,arr.ind = TRUE)
  
  for (j in 1:nrow(condition)) {
    
    id_1 = condition[j,1]
    id_2 = condition[j,2]
    
    weight_per_hd[id_1,id_2] = min(d[,-1])
  }
  names(weight_per_hd) <- gsub('X','', names(weight_per_hd))
  
  # create template df with all historical years
  # and populate milk_cow years (2003-2017)
  
  df <- data.frame(agrarian_region_id = seq(1,7))
  yrs <- as.character(seq(1987,2017))
  df[, yrs] <- sapply(yrs, function(x) df[,x] <- NA)
  df[, as.character(seq(2001,2017))] <- weight_per_hd[,  as.character(seq(2001,2017))]
  
  for (i in 1:nrow(weight_per_hd)) {
    
    # make LM prediction for the remaining years (1987,2002)  
    calc_df <- data.frame(y =as.numeric(weight_per_hd[i,-1]), x = seq(2001,2017))
    lm_model <- lm(y ~ x, data = calc_df)
    lm_prediction <- predict(lm_model, newdata =  data.frame(x =  seq(1987,2000)))
    
    df[i, as.character(seq(1987,2000))] <- round(lm_prediction, 1)
  }
  
  df[df<0] = 1.10
  names(df)[-1] = paste0('X', names(df)[-1])
  disagg_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Spatial_disaggregation')
  df <- plyr::join(disagg_df, df, 'agrarian_region_id')
  df = df[, c('Muni_ID','ID','Muni', paste0('X', seq(1987,2017)))]

  return(df)
}

