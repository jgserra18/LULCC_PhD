source('./Main/Global_functions.R')
source('./Main/General_GIS.R')
source('./LULCC/Model/PreProcessing/Organize_ExpVarRasterList.R')
source('./LULCC/Model/PreProcessing/Organize_ObsLulccRasterStack.R')

# default resolution: 500m

spatial_res = '500'



# construct exploratory variables -------------------------


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


other_exploratory_params = function() {
  #static
  print('Other exploratory params.')
  
  clc_500 = raster('./LULCC/Activity_data/CLC/500m/CLC_PT1990.tif')

  filepath  = list.files(path = './Nutrients/Activity_data/General_params/Irrigation/Other_exploratory_parameters/', full.names = TRUE)
  r_stck = lapply(filepath, raster)
  r_stck = stack(r_stck)
  r_stck = resample(r_stck, clc_500)
  
  
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
  
  lulc = get_lulc(year)
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



generate_ExpVars_stack = function(year) {
  
  environ = stack_environmental_data(spatial_res)
  mrb = stack_MRB_data(spatial_res)
  annual_stck = generate_annual_ExpVars(year)
  other = generate_other_common_ExpVars()
  expvars = stack(environ,mrb,annual_stck, other)
  
  # due to limited monitoring stations, the RF model for years below 2004 should account for the spatial prediction of nitrate for t + 1
  if (as.numeric(year)<2004) {
    
    t_1 = as.numeric(year)+1
    pat_t_1 = paste0('RF_GW_', t_1)
    RF_no3_t_1 = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Irrigation', subfolder = 'Nitrate_modelling', subfolderX2 = 'GW', subfolderX3 = 'Spatial_prediction', pattern = pat_t_1)
    
    old_names = names(expvars)
    expvars = stack(expvars, RF_no3_t_1)
    names(expvars) = c(old_names, 'RF_NO3_t+1')
  }
  return(expvars)
  rm(list=c('environ','mrb','annual_stck','other'))
}

  
# RANDOM FOREST MODEL ---------------------------------------------------------------------------------------

source('./Nutrients/Model/Irrigation/Preprocessing/1_Preprocessing_2017.R')  
source('./Nutrients/Model/Irrigation/Preprocessing/2_Preprocessing_1987_2016.R')  

library(caTools)
library(randomForest)
library(ggplot2)

set.seed(1000)


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
  
 # expvars = generate_ExpVars_stack(year)
  
  # aggregate all into one dataset
  for (i in 1:nlayers(expvars)) {
    
    shp_name <- as.character(names(expvars)[i])
    stations_shp[, shp_name] <- raster::extract(expvars[[i]], stations_shp)
  }
  df <- na.omit(stations_shp)
  
  return(df)
  rm(list=c('stations_shp','shp_name','expvars'))
}


create_partition_datasets = function(df_all, write) {
  # creates partition for further prediction
  # output: list where index 1 is the trainset and 2 is the testset
  
  train <- sample.split(df_all$x, SplitRatio = 0.8)
  train_set <- df_all[train, ]
  test_set <- df_all[-train, ]
  
  if (write == TRUE) {
    export_file(module = 'Nutrients', folder = 'Irrigation', subfolder = 'Nitrate_modelling', subfolderX2 = 'GW', subfolderX3 = 'Partitions', subfolderX4 = 'Training', file = train_set, filename = paste0('Trainset_',year))
    export_file(module = 'Nutrients', folder = 'Irrigation', subfolder = 'Nitrate_modelling', subfolderX2 = 'GW', subfolderX3 = 'Partitions', subfolderX4 = 'Testing', file =test_set, filename = paste0('Testset_',year))
  }
  
  return(list(train_set, test_set))
}


spatial_prediction_annual_mean_NO3_RF = function(year) {
  # spatially predicts nitrate concentration in groundwater using the randomforest algorithm 
  # unit: mg NO3 L-1
  
  
  rstack <- generate_ExpVars_stack(year)
  df_no3 <- aggregate_all_dataset(year, rstack)
  partition = create_partition_datasets(df_no3, FALSE)
    
  fm <- as.formula(paste('Avg_NO3~', paste(names(rstack), collapse = '+')))
  
  # rf model
  new_mtry = (ncol(df_no3)-4)/3
  rf_model <- randomForest(formula=fm,
                           data=partition[[1]], ntree=500, mtry=new_mtry, importance=T)
  # predict new raster
  predict_rf <- raster::predict(rstack, rf_model)
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




loop_first_timestep_GW_RF = function() {
  # loops from 2017 to 2004
  # this is the first timestep as it is considerably more data-rich
  # unit: mg NO3 L-1 
  
  yrs = as.character(seq(2017,2004,-1))
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

loop_second_timestep_GW_rf = function() {
  
  yrs = as.character(seq(2003,1993,-1))
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

library(doParallel)
require(caret)
require(gbm)

cl <- makePSOCKcluster(3)
registerDoParallel(cl)

set.seed(123)
rstack <- generate_ExpVars_stack('2017')
df_no3 <- aggregate_all_dataset('2017', rstack)
partition = create_partition_datasets(df_no3, FALSE)


fm <- as.formula(paste('Avg_NO3~', paste(names(rstack), collapse = '+')))
mtry <- sqrt(ncol(df_no3)-4)
tunegrid <- expand.grid(.mtry=seq(10, 40, 5))

# random search
metric <- "RMSE"
control <- trainControl(method="repeatedcv", number=10, repeats=5, search="grid")
rf_random <- train(fm, data=df_no3, method="rf", metric=metric,tunegrid = tunegrid, ntree = 550)
print(rf_random)
plot(rf_random)

stopCluster(cl)
