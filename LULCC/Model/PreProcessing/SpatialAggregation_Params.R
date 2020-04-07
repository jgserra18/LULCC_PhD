source('./Model/Main/Main_functions.R')
source('./Model/GIS/General_GIS.R')
source('./Model/PreProcessing/Organize_ExpVarRasterList.R')


## ----------------------- ACTIVITY DATA PARAMS ----------------------- ##
## ---------------------------------------------------------------------##


export_aggregatedParams <- function(folder, spatial_res, names, r_file) {
  
  if (folder=='Climatic_params') {
    
    res_df <- data.frame(spatial_res = c(2,5,10),
                         convert = c('500m', '200m', 'Native'))
    id <- which(res_df$spatial_res==spatial_res)
    
    export_file(file = r_file, 
                folder = 'Activity_data', 
                filename = names, 
                subfolder = folder, 
                subfolderX2 =  res_df[id, 2])
  } 
  
  else {
    export_file(file = r_file, 
                folder = 'Activity_data', 
                filename = names, 
                subfolder = folder, 
                subfolderX2 =  paste0(spatial_res, '00m'))
  }
}

aggregate_ActivityData_params <- function(folder, spatial_res) {
    
  if (folder == 'Climatic_params') {
    files <- get_dir_files(folder = 'Activity_data', param_pattern = folder, subfolder = '1000m')
    names <- get_dir_files(folder = 'Activity_data', param_pattern = folder, subfolder = '1000m', file_names = TRUE)
  } else {
    files <- get_dir_files(folder = 'Activity_data', param_pattern = folder, subfolder = 'Native')
    names <- get_dir_files(folder = 'Activity_data', param_pattern = folder, subfolder = 'Native', file_names = TRUE)  }

  for (i in seq_along(files)) {
    
    r_file <- raster(files[i])
    
    if (folder == 'Climatic_params') {
      
      res_df <- data.frame(spatial_res = c(2,5,10), convert = c('500m', '200m', 'Native'))
      r_file <- aggregate(r_file, spatial_res)
      id <- which(res_df$spatial_res==spatial_res)
      clc <- raster(paste0('./Activity_data/CLC/', res_df[id,2], '/CLC_PT1990.tif'))
      r_file <- resample(r_file, clc)
     # r_file <- resample_to_CLC(raster_file = r_file, spatial_res =as.character(res_df[id,2]))
    } 
    
    else {
      
      if (folder=='CLC') {
        r_file <- aggregate(r_file, spatial_res, modal)
      } else {
        r_file <- aggregate(r_file, spatial_res)
      }
    }
    export_aggregatedParams(folder = folder, spatial_res = spatial_res, names = names[i], r_file = r_file)
  }
  
  rm(list=c('files', 'names', 'r_file'))
}


loop_aggregate_ActivityData_params <- function() {
  # climatic params native spatial resolution of 1 x 1 km
  
  #folders <- c('CLC','Climatic_params', 'Environmental_params')
  folders <- c('CLC')
  res <- c(5,10)
  
  for (i in folders) {
    print(paste0('Working with ', i))
    for (j in res) {
      print(paste0('====== SPATIAL RESOLUTION OF ', j))
      aggregate_ActivityData_params(folder = i, spatial_res = j)
    }
  }
}

## ----------------------- OUTPUT PARAMS ----------------------- ##
## --------------------------------------------------------------##

general_aggregate_output_params <- function(param_name, spatial_res) {
  
  files <- get_dir_files('Output', 'Exploratory_variables', param_name, 'Native')
  names <- get_dir_files('Output', 'Exploratory_variables', param_name, 'Native', file_names = TRUE)
  
  for (i in seq_along(files)) {
    
    r_file <- raster(files[i])
    r_file <- aggregate(r_file, spatial_res)
    export_file(file = r_file, folder = 'Exploratory_variables', filename = names[i], 
                subfolder = param_name, subfolderX2 =  paste0(spatial_res, '00m'))
  }
  rm(list=c('files', 'names', 'r_file'))
}

# repeated CBA
aggregate_statistical_params <- function(spatial_res) {
  
  general_aggregate_output_params(param_name = 'Statistical_data', spatial_res = spatial_res)
}

aggregate_MRB_params <- function(spatial_res) {
  
  general_aggregate_output_params(param_name = 'MRB', spatial_res = spatial_res)
}

loop_aggregate_output_params <- function() {
  
  res <- c(2,5,10)
  for (i in res) {
    aggregate_statistical_params(i)
    aggregate_MRB_params(i)
  }
}
