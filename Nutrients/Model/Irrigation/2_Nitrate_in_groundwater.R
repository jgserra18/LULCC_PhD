source('./Main/Global_functions.R')
source('./Main/General_GIS.R')
source('./LULCC/Model/PreProcessing/Organize_ExpVarRasterList.R')

# default resolution: 500m

spatial_res = '500'



# construct exploratory variables -------------------------



get_climatic_vars = function(year) {
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
  
  lulc = get_activity_data(module = 'LULCC', mainfolder = 'Output', folder = 'LULC', subfolder = 'PT', subfolderX2 = spatial_res, pattern = year)
  store = list()
  ids = unique(lulc[[1]])
  
  for (id in ids) {
    
    new_lulc = lulc==id
    store = append(store, new_lulc)
  }
  store = stack(store)
  
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
  
  return(r_stck)
  rm(list=c('clc_500','filepath'))
}




generate_ExpVars_stack = function(year) {
  
  environ = stack_environmental_data(spatial_res)
  mrb = stack_MRB_data(spatial_res)
  lulc = get_lulc(year)
  ssnb = get_ssnb(year)
  leaching = get_total_leaching(year)
  miterra = get_MITERRA_fractions(year)
  climatic = get_climatic_vars(year)
  
  expvars = stack(environ,mrb,lulc,ssnb,leaching,miterra,climatic)
  
  return(expvars)
  rm(list=c('environ','mrb','lulc','ssnb','leaching','miterra','climatic'))
}

// falta tempo de residencia, etc

year = '2016'
environ = stack_environmental_data(spatial_res)
mrb = stack_MRB_data(spatial_res)
lulc = get_lulc(year)
ssnb = get_ssnb(year)
leaching = get_total_leaching(year)
miterra = get_MITERRA_fractions(year)
climatic = get_climatic_vars(year)
other = other_exploratory_params()

expvars = stack(environ,mrb,lulc,ssnb,leaching,miterra,climatic, other)
rm(list=c('environ','mrb','lulc','ssnb','leaching','miterra','climatic', 'other'))


# call exploratory datasets and normalize each exploratory parameter (0-1)
# calls NO3 monitoring stations and extracts data from each exploratory parameter
# converts to a dataframe and masks NAs

gw_sp  = no3_2000[[2]]

for (i in 1:nlayers(expvars)) {
  
  shp_name <- as.character(names(expvars)[i])
  gw_sp[, shp_name] <- raster::extract(expvars[[i]], gw_sp)
}

df <- na.omit(gw_sp)

# creates partition for further prediction
# output: list where index 1 is the trainset and 2 is the testset

set.seed(99)
require(caTools)

train <- sample.split(df$x, SplitRatio = 0.75)
train_set <- df[train, ]
test_set <- df[-train, ]

partition = list(train_set, test_set)

# spatially predicts nitrate concentration in groundwater using the randomforest algorithm 
# unit: mg NO3 L-1

library(randomForest)


fm <- as.formula(paste('Avg_NO3~', paste(names(expvars), collapse = '+')))

rf_model <- randomForest(formula=fm,
                         data=partition[[1]], ntree=500, mtry=73/3, importance=T)

predict_rf <- raster::predict(expvars, rf_model)
predict_rf <- mask(crop(predict_rf, extent(expvars[[1]])), expvars[[1]])

require(tmap)
tm_shape(predict_rf)+ tm_raster(breaks=c(0,10,25,50,+Inf), palette = c('blue1','green1','yellow1','red1')) + 
  tm_shape(gw_sp) + tm_dots(col='Avg_NO3', breaks=c(0,10,25,50,+Inf),  palette = c('blue1','green1','yellow1','red1'), size = 0.1)


# test accuracy
test_set$test_No3 <- raster::extract(predict_rf, test_set)
dff <- as.data.frame(test_set[, c('test_No3', 'Avg_NO3')])

library(ggplot2)
p1 <- ggplot(dff, aes(test_No3, Avg_NO3)) + geom_point() + 
  geom_smooth(method='lm') + 
  theme_test() 
p1
summary(lm(test_No3~Avg_NO3, dff))$r.squared
