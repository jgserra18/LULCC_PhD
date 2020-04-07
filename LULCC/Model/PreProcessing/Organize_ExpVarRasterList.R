source('./Main/Global_functions.R')
source('./Main/General_GIS.R')

## ----------------------- LIBRARIES --------------------- ##
## --------------------------------------------------------##

library(lulcc)
library(dplyr)



## ----------------------- Exploratory Variables Organization --------------------- ##
## ---------------------------------------------------------------------------------##


general_rstack_func <- function(folder_path) {
  # general function to stack rasters from a given folder path
  
  l_files <- list.files(folder_path, full.names = TRUE)
  
  require(doParallel)
  require(doRNG)
  cl <- registerDoParallel(cores=2)
  getDoParWorkers()
  
  
  rs_func <- function(x) {
    r_file <- raster(x)
    if (res(r_file)[1] != 100) {
      r_file <- resample_to_CLC(module = 'LULCC',r_file, TRUE, spatial_res)
    }
    return(r_file)
  }
  
  store <- mclapply(l_files, FUN = rs_func)
  store <- stack(store)
  return(store)
  doParallel::stopImplicitCluster()
  rm(list=c('l_files', 'r_file'))
}



## ----------------------- Exploratory Variables Organization --------------------- ##
## ---------------------------------------------------------------------------------##

get_output_statistical_data <- function(spatial_res) {
  # creates a RasterStack from all statistical data from Statistics Portugal at subregional level
  
  out_stat <- get_folderpath(module = 'LULCC', 'Output', 'Exploratory_variables', 'Statistical_data')
  stat_files <- list.files(path = out_stat, pattern = as.character(spatial_res), full.names = TRUE)
  stat_files <- list.files(path = stat_files, full.names = TRUE)
  store_list <- vector('list', length = length(stat_files))
  
  ctr <- 0
  for (i in stat_files) {
    
    ctr <- ctr + 1
    r_file <- raster(i)
    
    store_list[[ctr]] <- r_file
  }
  
  store_list <- stack(store_list)
  
  return(store_list)
  rm(list=c('out_stat', 'stat_files', 'r_file'))
}



correct_Params_Resolution <- function(param, spatial_res) {
  
  spatial_res <-  ifelse(spatial_res=='Native', 'Native', paste0(spatial_res, 'm'))
  
  climatic_native <- get_dir_files(module = 'LULCC',folder = 'Activity_data', param_pattern = param, subfolder = spatial_res)
  
  for (i in climatic_native) {
    
    r_file <- raster(i)
    clc <- get_activity_data(module = 'LULCC',folder = 'CLC', pattern = '1990', subfolder =spatial_res)
    r_file <- resample(r_file, clc)
  # r_file <- resample_to_CLC(raster_file = r_file, spatial_res ='1000m')
    tifoptions=c('COMPRESS=DEFLATE', 'PREDICTOR=2', 'ZLEVEL=6')
    
    if (compareRaster(r_file, clc)==TRUE) {
      writeRaster(r_file, i, options=tifoptions, overwrite = TRUE)
    } else {
      print('Wrong spatial resolution.')
    }
  }
}


stack_environmental_data <- function(spatial_res) {
  # creates a rasterstack with climatic and environmental parameters
  # resamples those data to a 100x100m grid using the ngb algorithm
  
  
  store <- list()
  folders <- c('Environmental_params', 'Climatic_params')
  ctr <- 0
  
  for (i in folders) {
    
    d <- get_dir_files(module = 'LULCC', folder = 'Activity_data', subfolder = i, mainfolder = as.character(paste0(spatial_res,'m')))

    for (j in l_files) {
      
      print(paste0('Working with ', j))

      r_file <- raster(j)
      # check spatial resolution
    #  if (res(r_file)[1] != 100) {
    #    r_file <- resample_to_CLC(r_file, TRUE)
    #  }
      
      ctr <- ctr + 1
      store[[ctr]] <- r_file
      
      filename <- gsub(paste0(folder_path, '/'), '', j)
      names(store)[[ctr]] <- filename
    }
  }
  
  store <- stack(store)
  return(store)
  rm(list=c('store', 'folders', 'folder_path', 'l_files', 'r_file'))
}


stack_MRB_data <- function(spatial_res) {
  # stacks MRB data (e.g., distance to roads, distance to rivers)
  # resamples to CLC spatial res and extent
  # more importantly, it reclassifies and masks the MRB raster to the CLC data
  
  store <- list()
  ctr_store <- 0
  
  folder_path <- file.path('./Output/Exploratory_variables//MRB/')
  folder_path <- list.files(path = folder_path, pattern = as.character(spatial_res), full.names = TRUE)
  l_files <- list.files(folder_path,  full.names=TRUE)
  filename <- list.files(folder_path)
    
    ctr <- 0
    
    for (j in l_files) {
      
      print(j)
      ctr <- ctr + 1
      r_file <- raster(l_files[[ctr]])
      store[[ctr]] <- r_file
      names(store)[[ctr]] <- filename[[ctr]]
    }
  
  store <- stack(store)
  return(store)
  rm(list=c('folders', 'folder_path', 'l_files', 'shp_file', 'r_file'))
}


export_MRB_data <- function(spatial_res) {
  
  r_stack <- stack_MRB_data(spatial_res)
  names(r_stack) <- gsub('.tif', '', names(r_stack))
  
  for (i in 1:nlayers(r_stack)) {
    r_file <- r_stack[[i]]
    export_file(module = 'LULCC',file = r_file, folder = 'Exploratory_variables', filename = names(r_stack)[[i]], 
                subfolder = 'MRB', subfolderX2 = spatial_res)
    
  }
}


aggregate_stack_ExpVarRaster <- function(spatial_res) {
  
  mrb <- stack_MRB_data(spatial_res)
  environ <- stack_environmental_data(spatial_res)
  stat <- get_output_statistical_data(spatial_res)
  
  st <- stack(mrb, environ, stat)
  return(st)
  rm(list=c('mrb', 'environ', 'stat'))
}


feed_ExpVarRasterList <- function(admin, admin_id, spatial_res) {
  
  st_expVar <- aggregate_stack_ExpVarRaster(spatial_res)
  names(st_expVar) <- paste0('ef', seq(1, nlayers(st_expVar)))
  
  if (missing(admin)==TRUE | missing(admin_id)==TRUE) {
      st_expVar <- ExpVarRasterList(x = st_expVar, pattern = 'ef')
  }
  else {
    st_expVar <- general_RasterCrop_admin(module = 'LULCC',r_file = st_expVar, admin = admin, admin_id = admin_id)
    st_expVar <- ExpVarRasterList(x = st_expVar, pattern = 'ef')
  
  }
  return(st_expVar)
}

