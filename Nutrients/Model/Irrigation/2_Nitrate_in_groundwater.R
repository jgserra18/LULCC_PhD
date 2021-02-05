source('./Main/Global_functions.R')
source('./Main/General_GIS.R')
source('./LULCC/Model/PreProcessing/Organize_ExpVarRasterList.R')
source('./LULCC/Model/PreProcessing/Organize_ObsLulccRasterStack.R')

# default resolution: 500m

spatial_res = '500'



# construct exploratory variables -------------------------


get_statistical_nutrient_flows_data = function(year) {
  
  params = c('N_excretion','N_fertiliser','N_offtake')
  store = list()
  
  for (param in params) {
    print(paste0('Preparing exp parameters for ', param))
    file_names = list_all_files_folder(module = 'Nutrients', folder = 'General_params', subfolder = 'Irrigation', subfolderX2 = 'Other_exploratory_parameters', subfolderX3 = paste0('X', year), subfolderX4 = param, full_names = F)
    files = list_all_files_folder(module = 'Nutrients', folder = 'General_params', subfolder = 'Irrigation', subfolderX2 = 'Other_exploratory_parameters', subfolderX3 = paste0('X', year), subfolderX4 = param, full_names = T)
    files = stack(lapply(files, raster))
    names(files) = gsub('.tif', '', file_names)
    
    store = append(store, files)
  }
  store = stack(store)
  
  return(store)
  rm(list=c('file_names','files'))
}




get_climatic_vars = function(year, write) {
  #  precipitation and et0; annual
  print('Preparing climatic.')
  
  clc_500 = raster('./LULCC/Activity_data/CLC/500m/CLC_PT1990.tif')
  prec <- get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Environmental_params', subfolder = 'Climatic', subfolderX2 = 'Prec', subfolderX3 ='1x1', pattern = year)
  et0 <- get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Environmental_params', subfolder = 'Climatic', subfolderX2 = 'ET0', subfolderX3 ='1x1', pattern = year)
  climatic_vars = stack(prec, et0)
  names(climatic_vars) = c('prec','et0')
  climatic_vars = resample(climatic_vars, clc_500)
  
  return(climatic_vars)
  rm(list=c('prec','et0'))
}


get_MITERRA_fractions = function(year) {
  print('Preparing miterra.')
  
  FRAC_rf = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'MITERRA_fractions', subfolder = 'FRAC_runoff', subfolderX2 = 'Runoff_fraction', pattern = year)
  FRAC_leaching = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'MITERRA_fractions', subfolder = 'FRAC_leaching', subfolderX2 = 'Leaching_fraction', pattern = gsub('X','',year))
  
  miterra_vars = stack(FRAC_rf, FRAC_leaching)
  names(miterra_vars) = c('FRAC_runoff','FRAC_leaching')
  miterra_vars = aggregate(miterra_vars, 5)
  
  return(miterra_vars)
  rm(list=c('FRAC_rf','FRAC_leaching'))
}


get_total_leaching = function(year) {
  print('Preparing leaching.')
  
  leaching = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Leaching', subfolder = 'Total_leaching', subfolderX2 = 'N', subfolderX3 = 'UAA', subfolderX4 = 'Spatially_explicit', pattern = paste0('X',year))
  leaching = aggregate(leaching, 5)
  
  return(leaching)
}

get_ssnb = function(year) {
  print('Preparing ssnb.')
  
  crop_ssnb = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Nutrient_balances', subfolder ='Soil_balance', subfolderX2 = 'N', subfolderX3 = 'Cropland', subfolderX4 = 'Spatially_explicit', pattern = year)
  grass_ssnb = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Nutrient_balances', subfolder ='Soil_balance', subfolderX2 = 'N', subfolderX3 = 'Grassland', subfolderX4 = 'Spatially_explicit', pattern = year)
  ssnb = stack(crop_ssnb, grass_ssnb)
  names(ssnb) = c('crop_ssnb','grass_ssnb')
  ssnb = aggregate(ssnb, 5)
  
  return(ssnb)
  rm(list=c('crop_ssnb','grass_ssnb'))
}



get_lulc = function(year) {
  print('Preparing lulc.')
  
  clc_df = CLC_df()
  lulc = get_activity_data(module = 'LULCC', mainfolder = 'Output', folder = 'LULC', subfolder = 'PT', subfolderX2 = spatial_res, pattern = year)
  store = list()
  ids = unique(lulc[[1]])
  
  ctr = 0
  for (id in ids) {
    
    if (id == 0) {
      next 
    }
    else {
          ctr = ctr + 1
    new_lulc = lulc==id
    store = append(store, new_lulc)
    }
  }
  store = stack(store)
  names(store) = clc_df[,2]
  
  return(store)
  rm(list=c('lulc','ids','new_lulc'))
}

get_annual_LULC = function(year) {
  
  yr = as.numeric(year)
  print('Preparing annual  lulc.')
  if (yr<1995) {
    year = '1990'
  }
  else if (yr>1995 & yr<2003) {
    year = '2000'
  }
  else if (yr>2003 & yr<2009) {
    year = '2006'
  }
  else if (yr>2009 & yr<2015) {
    year = '2012'
  }
  else {
    year = '2018'
  }
  clc = get_activity_data(module = 'LULCC', subfolder = 'CLC', subfolderX2 = '500m', pattern = year)
  return(clc)
}

get_agri_industrial_activities = function() {
  # preprocessed using  QGIS
  # data from SNIamb
  
  clc_500 = raster('./LULCC/Activity_data/CLC/500m/CLC_PT1990.tif')
  
  filepath  = list.files(path = './Nutrients/Output/Irrigation/Nitrate_modelling/GW/Exploratory_params/Point_activities/', pattern = '.tif', full.names = TRUE)
  r_stck = list()
  for (file in filepath) {
    
    r_file  = raster(file)
    r_file[is.na(r_file[])] = 0 
    r_file = mask(crop(r_file, extent(clc_500)), clc_500)
    r_stck = append(r_stck, r_file)
  }
  r_stck[is.na(r_stck[])] == 0
  r_stck = stack(r_stck)
  return(r_stck)
  rm(list=c('clc_500','file'))
}


other_exploratory_params = function() {
  #static
  print('Other exploratory params.')
  
  clc_500 = raster('./LULCC/Activity_data/CLC/500m/CLC_PT1990.tif')
  
  filepath  = list.files(path = './Nutrients/Activity_data/General_params/Irrigation/Other_exploratory_parameters/Common/', pattern = '.tif', full.names = TRUE)
  r_stck = list()
  for (file in filepath) {
    
    r_file  = raster(file)
    r_file  = resample(r_file, clc_500)
    r_stck = append(r_stck, r_file)
  }
  r_stck = stack(r_stck)

  for (i in 1:nlayers(r_stck)) {
    
    r_file = r_stck[[i]]
    export_file(module = 'Nutrients', folder = 'Irrigation', 
                subfolder = 'Nitrate_modelling', subfolderX2 = 'GW',
                subfolderX3 = 'Exploratory_params', subfolderX4 = 'Common',
                file = r_file, filename = names(r_stck)[i])
    
  }

  rm(list=c('clc_500','filepath'))
}



export_annual_ExpVars = function(year) {
  # export all the exp parameters so they take less time when running the RF model
  
  lulc = get_annual_LULC(year)
  ssnb = get_ssnb(year)
  leaching = get_total_leaching(year)
  miterra = get_MITERRA_fractions(year)
  climatic = get_climatic_vars(year)

  expvars = stack(lulc,ssnb,leaching,miterra,climatic)

  for (i in 1:nlayers(expvars)) {
    
    r_file = expvars[[i]]
    export_file(module = 'Nutrients', folder = 'Irrigation', 
                subfolder = 'Nitrate_modelling', subfolderX2 = 'GW',
                subfolderX3 = 'Exploratory_params', subfolderX4 = year,
                file = r_file, filename = names(expvars)[i])
    
  }
  rm(list=c('lulc','ssnb','leaching','miterra','climatic'))
}

loop_annual_ExpVars = function() {
  
  yrs = paste0(seq(1991, 2017))
  for (yr in yrs) {
    export_annual_ExpVars(yr)
  }
}





# prepare exploratory variables for the RF model -------------------------------------------------

generate_annual_ExpVars = function(year) {
  
  r_names =  list_all_files_folder(module = 'Nutrients', mainfolder = 'Output', folder = 'Irrigation', subfolder = 'Nitrate_modelling', subfolderX2 = 'GW', subfolderX3 = 'Exploratory_params', subfolderX4 = year)
  file_list = list_all_files_folder(module = 'Nutrients', mainfolder = 'Output', folder = 'Irrigation', subfolder = 'Nitrate_modelling', subfolderX2 = 'GW', subfolderX3 = 'Exploratory_params', subfolderX4 = year, full_names = T)
  file_list = lapply(file_list, raster)
  
  rstack = stack(file_list)
  names(rstack) = r_names
  
  return(rstack)
  rm(list=c('r_names','file_list'))
}



generate_other_common_ExpVars = function() {
  
  r_names =  list_all_files_folder(module = 'Nutrients', mainfolder = 'Output', folder = 'Irrigation', subfolder = 'Nitrate_modelling', subfolderX2 = 'GW', subfolderX3 = 'Exploratory_params', subfolderX4 = 'Common')
  file_list = list_all_files_folder(module = 'Nutrients', mainfolder = 'Output', folder = 'Irrigation', subfolder = 'Nitrate_modelling', subfolderX2 = 'GW', subfolderX3 = 'Exploratory_params', subfolderX4 = 'Common', full_names = T)
  file_list = lapply(file_list, raster)
  
  rstack = stack(file_list)
  names(rstack) = r_names
  
  return(rstack)
  rm(list=c('r_names','file_list'))
}


stack_2nd_period_NO3_predictions = function() {
  
  yrs = seq(2004,2017)
  store = list()
  for (yr in yrs) {
    
    file_pattern  = paste0('RF_GW_', yr) 
    file = raster(
      list.files(path = './Nutrients/Output/Irrigation/Nitrate_modelling/GW/Spatial_prediction/', pattern =file_pattern, full.names = T))
    store  = append(store, file)
  }
  store = stack(store)
  return(store)
}


generate_ExpVars_stack = function(year) {
  
  environ = stack_environmental_data(spatial_res)
  mrb = stack_MRB_data(spatial_res)
  annual_stck = generate_annual_ExpVars(year)
  other = generate_other_common_ExpVars()
 # annual_lulc = get_annual_LULC(year)
  point_activities = get_agri_industrial_activities()
 # nutrient_params = get_statistical_nutrient_flows_data(year)
  expvars = stack(environ,mrb,annual_stck, other, point_activities)#, nutrient_params)

  # due to limited monitoring stations, the RF model for years below 2004 should account for the spatial prediction of nitrate for t + 1
  if (as.numeric(year)<2004) {
    
   # t_1 = as.numeric(year)+1
   # pat_t_1 = paste0('RF_GW_', t_1)
    #RF_no3_t_1 = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Irrigation', subfolder = 'Nitrate_modelling', subfolderX2 = 'GW', subfolderX3 = 'Spatial_prediction', pattern = pat_t_1)
    past_no3 = stack_2nd_period_NO3_predictions()
    old_names = names(expvars)
    expvars = stack(expvars, past_no3)
    names(expvars) = c(old_names, names(past_no3))
  }
  return(expvars)
  rm(list=c('environ','mrb','annual_stck','other'))
}


# RANDOM FOREST MODEL ---------------------------------------------------------------------------------------

source('./Nutrients/Model/Irrigation/Preprocessing/1_Preprocessing_2017.R')  
source('./Nutrients/Model/Irrigation/Preprocessing/2_Preprocessing_1987_2016.R')  

library(caTools)
library(ranger)
library(ggplot2)

all_stations  = get_activity_data(module = 'Nutrients', folder = 'Nutrient_params', subfolder = 'N', subfolderX2 = 'Irrigation', subfolderX3 = 'GW_monitoring_station',  subfolderX4 = '1987-2016', pattern = 'GW')

aggregate_all_dataset = function(year, expvars) {
  # call exploratory datasets and normalize each exploratory parameter (0-1)
  # calls NO3 monitoring stations and extracts data from each exploratory parameter
  # converts to a dataframe and masks NAs
  
  
  # get monitoring stations for a given year
  if (year == '2017') {
    stations_shp = spatialize_stations_X2017(stations_source = 'GW')[[2]]
  }
  else {
    all_stations  = get_activity_data(module = 'Nutrients', folder = 'Nutrient_params', subfolder = 'N', subfolderX2 = 'Irrigation', subfolderX3 = 'GW_monitoring_station',  subfolderX4 = '1987-2016', pattern = 'GW')
    stations_shp = spatialize_annual_mean_NO3(year, all_stations)[[2]]
  }

  expvars = generate_ExpVars_stack(year)
  
  # aggregate all into one dataset
  for (i in 1:nlayers(expvars)) {
    
    shp_name <- as.character(names(expvars)[i])
    stations_shp[, shp_name] <- raster::extract(expvars[[i]], stations_shp)
  }
  df <- na.omit(stations_shp)
  df = df[, -which(names(df)=='geometry')]
  
  return(df)
  rm(list=c('stations_shp','shp_name','expvars'))
}


create_partition_datasets = function(df_all, year, write) {
  # creates partition for further prediction
  # output: list where index 1 is the trainset and 2 is the testset
  
  set.seed(1000)
  
  train <- sample.split(df_all$x, SplitRatio = 0.8)
  train_set <- df_all[train, ]
  test_set <- df_all[-train, ]
  
  if (write == TRUE) {
    export_file(module = 'Nutrients', folder = 'Irrigation', subfolder = 'Nitrate_modelling', subfolderX2 = 'GW', subfolderX3 = 'Partitions', subfolderX4 = 'Training', file = as.data.frame(train_set), filename = paste0('Trainset_',year))
    export_file(module = 'Nutrients', folder = 'Irrigation', subfolder = 'Nitrate_modelling', subfolderX2 = 'GW', subfolderX3 = 'Partitions', subfolderX4 = 'Testing', file = as.data.frame(test_set), filename = paste0('Testset_',year))
  }
  
  return(list(train_set, test_set))
}


hypertune_parameters = function(formula, training_data) {
  #' @source https://bradleyboehmke.github.io/HOML/random-forest.html#ref-probst2019hyperparameters
  #' @param formula formula
  #' @param training_data training data (eg partitin[[2]])
  #' @description hypertunes using a grid search the optimal parameters as defined by the RMSE
  #' @usage  hypertune_parameters(formula = fm, data = partition[[1]])
  
  hyper_grid_2 <- expand.grid(
    mtry       = c(25,27,30,32,35),
    ntrees     = seq(500,1500,500),
    replace    = c(T,F),
    node_size  = seq(2, 10, by = 2),
    sample.fraction = c(.5, .63, .8),                       
    OOB_RMSE  = 0
  )
  
  # perform grid search
  for(i in 1:nrow(hyper_grid_2)) {
    print(i)
    # train model
    model <- ranger(
      formula         = formula, 
      data            = training_data, 
      num.trees       = hyper_grid_2$ntrees[i],
      mtry            = hyper_grid_2$mtry[i],
      min.node.size   = hyper_grid_2$node_size[i],
      sample.fraction = hyper_grid_2$sample.fraction[i],
      replace         = hyper_grid_2$replace[i],
      seed            = 123
    )
    
    # add OOB error to grid
    hyper_grid_2$OOB_RMSE[i] <- sqrt(model$prediction.error)
  }
  
  hyper_grid_2 %>% 
    dplyr::arrange(OOB_RMSE) %>%
    head(10)
  
  return(hyper_grid_2)
}



#year = '2002'
spatial_prediction_annual_mean_NO3_RF = function(year) {
  #' @param year model year
  #' @description spatially predicts nitrate concentration in groundwater using the randomforest algorithm 
  #  unit: mg NO3 L-1
  
  rstack <- generate_ExpVars_stack(year)
  df_no3 <- aggregate_all_dataset(year, rstack)
  partition = create_partition_datasets(df_all = df_no3, year = year, write = TRUE)
    
  fm <- as.formula(paste('Avg_NO3~', paste(names(rstack), collapse = '+')))
  new_mtry = (ncol(df_no3)-4)/3
  partition[[1]]$geometry = NULL
  
  # hyper tune parameters
  tune = hypertune_parameters(formula = fm, training_data = partition[[1]])
  tune = dplyr::slice_max(.data = tune, n=1, order_by = 'imp') # best hyperparameters 
  
  # ranger model
  rf_model = ranger(formula = fm, data = partition[[1]], 
                    num.trees = tune$ntrees, 
                    mtry = tune$mtry, 
                    min.node.size=tune$node_size, 
                    sample.fraction=tune$sample.fraction,
                    replace = tune$replace, num.threads = 4, 
                    local.importance = T, 
                    importance = 'impurity')
  
  # export top 25 exploratory vars
  # importance
  var_imp = as.data.frame(rf_model$variable.importance, row.names = F)
  var_imp = data.frame(imp = var_imp$`rf_model$variable.importance`, 
                       var = row.names(var_imp))
  var_imp$var = gsub('.tif','',var_imp$var)
  var_imp$var = gsub('X500m.','',var_imp$var)
  var_imp = var_imp%>%
    dplyr::arrange(desc(var_imp$imp)) %>%
    dplyr::slice_max(order_by = imp, n=25)
  View(var_imp)
  export_file(module = 'Nutrients', file = var_imp, 
              filename = paste0('Top25_ExpVars_', year), 
              folder = 'Irrigation', 
              subfolder = 'Nitrate_modelling', 
              subfolderX2 = 'GW',
              subfolderX3 = 'Spatial_prediction')
  
  # predict new raster
  predict_rf = predict(rstack, rf_model, type='response', progress='window', fun = function(model, ...) predict(model, ...)$predictions)
  predict_rf <- mask(crop(predict_rf, extent(rstack[[1]])), rstack[[1]])
 
  return(list(predict_rf = predict_rf, test_set = partition[[2]]))
  rm(list=c('rstack','df_no3','partition','fm','rf_model'))
}


test_RF_accuracy  = function(predict_rf, test_set) {
  
  test_set$test_No3 <- raster::extract(predict_rf, test_set)
  dff <- as.data.frame(test_set[, c('test_No3', 'Avg_NO3')])
  p1 <- ggplot(dff, aes(test_No3, Avg_NO3)) + geom_point() + 
    geom_smooth(method='lm') + 
    theme_test() 
  print(p1)
  print(summary(lm(test_No3~Avg_NO3, dff))$r.squared)
}

compute_ROC = function(predict_rf, test_set) {
  
  library(pROC)
  
  test_set$test_No3 <- raster::extract(predict_rf, test_set)
  result.roc = roc(test_set$Avg_NO3, test_set$test_No3, auc = T, plot = T)
  result.roc$auc
  plot(result.roc, print.thres="best")
  
}



loop_first_timestep_GW_RF = function() {
  # loops from 2017 to 2004
  # this is the first timestep as it is considerably more data-rich
  # unit: mg NO3 L-1 
  
  yrs = as.character(seq(2017,2004,-1))
  for (yr in yrs) {
    
    dataset = spatial_prediction_annual_mean_NO3_RF(year = yr)
    test_set = dataset[[2]]
    RF_NO3 = dataset[[1]]
    
    r2 = round(test_RF_accuracy(predict_rf = RF_NO3, test_set = test_set), 2)
    compute_ROC(RF_NO3, test_set)
    export_file(module = 'Nutrients',
                folder = 'Irrigation', 
                subfolder = 'Nitrate_modelling', 
                subfolderX2 = 'GW', 
                subfolderX3 = 'Spatial_prediction', 
                file =RF_NO3,
                filename = paste0('RF_GW_',yr,'_',r2))
  }
}

loop_second_timestep_GW_rf = function() {

  yrs = as.character(seq(2003,1995,-1))
  for (yr in yrs) {
    
    dataset = spatial_prediction_annual_mean_NO3_RF(year = yr)
    test_set = dataset[[2]]
    RF_NO3 = dataset[[1]]
    
    r2 = round(test_RF_accuracy(predict_rf = RF_NO3, test_set = test_set), 3)
    export_file(module = 'Nutrients',
                folder = 'Irrigation', 
                subfolder = 'Nitrate_modelling', 
                subfolderX2 = 'GW', 
                subfolderX3 = 'Spatial_prediction', 
                file =RF_NO3,
                filename = paste0('RF_GW_',yr,'_',r2))
  }
}




