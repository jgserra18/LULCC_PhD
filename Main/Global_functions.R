source('./Main/Set_ProjectDirectory.R')

## LIBRARIES --------------------------------------------------------------------------------


library(sf)
library(raster)


## GENERAL PURPOSE ---------------------------------------------------------------------------

identify_read_fileclass <- function(file_path) {
  # read file path and reads the file accordingly
  
  patterns <- c('.tif', '.csv', '.shp', '.grd')
  id <- which(grepl(paste(patterns, collapse = '|'), file_path)==TRUE)
  file_path <- file_path[id]

  if (grepl('.tif', file_path)==TRUE) {
    r_file <- raster(file_path)
  }
  else if (grepl('.csv', file_path)==TRUE) {
    r_file <- read.csv(file_path, stringsAsFactors = F)
  }
  else if (grepl('.shp', file_path)==TRUE) {
    r_file <- sf::read_sf(file_path)
  }
  else if (grepl('.grd', file_path)==TRUE) {
    r_file <- stack(file_path)
  }
  return(r_file)
}


create_new_directory <- function(path, new_dir) {
  # creates new directory by pasting the path and the new directory folder
  
  new_path <- file.path(path, new_dir)
  dir.create(path = new_path, showWarnings = FALSE)
  return(new_path)
}


correct_filename_iteraction <- function(path, filename) {
  # function used to add the number of interactive cycles within a given admin_reg and admin_id
  # TO BE USED: in fwrite or write.csv
  
  filepath <- file.path(path, paste0(filename, '_1.csv'))
  
  if (file.exists(filepath)==TRUE) {
    iter_number <- as.integer(stringr::str_match(filepath,  paste0(filename, "_(.*?).csv"))[, 2]) + 1
    new_filepath <- file.path(path, paste0(filename, '_', iter_number, '.csv'))
  } else {
    new_filepath <- file.path(path, paste0(filename, '_1.csv'))
  }
  return(new_filepath)
  rm(list=c('filepath', 'iter_number'))
}


correct_filename_iter <- function(path, filename) {
  
  ctr <- 0
  filepath <- file.path(path, paste0(filename, '_', ctr, '.csv'))
  
  while (file.exists(filepath)==TRUE) {
    
    ctr <- ctr + 1
    filepath <- file.path(path, paste0(filename, '_', ctr,'.csv'))
  }
  new_filepath <- file.path(path, paste0(filename, '_', ctr, '.csv'))
  return(new_filepath)
  rm(list=c('filepath'))
}


## GETTERS --------------------------------------------------------------------------------------------

get_mainfolder_sub <- function(module, main_folder, pattern) {
  # gets the mainfolder for the model (e.g., Activity-data)
  # pattern can be specified for any subfolder
  
  sel_folder <- list.files(path = paste0('./', module), pattern = main_folder, full.names = TRUE)
  
  if (missing(pattern)==TRUE) {
    return(sel_folder)
  } 
  else {
    pattern_path <- list.files(path = sel_folder, pattern = pattern, full.names = TRUE)
    
    if (length(pattern_path)==0) {
      create_new_directory(path = sel_folder, new_dir = pattern)
    }
    
    return(pattern_path)
  }
}

get_activity_data <- function(module, folder, pattern, subfolder, mainfolder, subfolderX2, subfolderX3, subfolderX4, subfolderX5, subfolderX6) {
  # gets the path of a given file within folders/subfolders of activity data,
  # this can also be used to get output data
  
  # get activity data path and read and select a file based on the specified pattern
  if (missing(mainfolder)==TRUE) {
    act_data <- get_mainfolder_sub(module, main_folder = 'Activity_data', pattern = folder)
  }
  else {
    act_data <- get_mainfolder_sub(module, main_folder = mainfolder, pattern = folder)
  }
  if (missing(subfolder)==TRUE & missing(subfolderX2)==TRUE & missing(subfolderX3)==TRUE & missing(subfolderX4)==TRUE & missing(subfolderX5)==T & missing(subfolderX6)==T) {
    
    sel_file <- list.files(path = act_data, pattern = pattern, full.names = TRUE)
    r_file <- identify_read_fileclass(sel_file)
  }
  else if (missing(subfolderX2)==TRUE & missing(subfolderX3)==TRUE & missing(subfolderX4)==TRUE & missing(subfolderX5)==TRUE & missing(subfolderX6)==TRUE) {
    sel_subfolder <- list.files(path = act_data, pattern = subfolder, full.names = TRUE)
    sel_file <- list.files(path = sel_subfolder, pattern = pattern, full.names = T)
    r_file <- identify_read_fileclass(sel_file)
  } 
  else if (missing(subfolderX3)==TRUE & missing(subfolderX4)==TRUE & missing(subfolderX5)==TRUE & missing(subfolderX6)==TRUE){
    sel_subfolder <- list.files(path = act_data, pattern = subfolder, full.names = TRUE)
    sel_subfolder <- list.files(path = sel_subfolder, pattern = subfolderX2, full.names = TRUE)
    sel_file <- list.files(path = sel_subfolder, pattern = pattern, full.names = T)
    r_file <- identify_read_fileclass(sel_file)
  }
  else if (missing(subfolderX4)==TRUE & missing(subfolderX5)==TRUE & missing(subfolderX6)==TRUE) {

    sel_subfolder <- list.files(path = act_data, pattern = subfolder, full.names = TRUE)
    sel_subfolder <- list.files(path = sel_subfolder, pattern = subfolderX2, full.names = TRUE)
    sel_subfolder <- list.files(path = sel_subfolder, pattern = subfolderX3, full.names = TRUE)
    sel_file <- list.files(path = sel_subfolder, pattern = pattern, full.names = T)
    r_file <- identify_read_fileclass(sel_file)
  } 
  else if (missing(subfolderX5)==TRUE & missing(subfolderX6)==TRUE) {

    sel_subfolder <- list.files(path = act_data, pattern = subfolder, full.names = TRUE)
    sel_subfolder <- list.files(path = sel_subfolder, pattern = subfolderX2, full.names = TRUE)
    sel_subfolder <- list.files(path = sel_subfolder, pattern = subfolderX3, full.names = TRUE)
    sel_subfolder <- list.files(path = sel_subfolder, pattern = subfolderX4, full.names = TRUE)
    sel_file <- list.files(path = sel_subfolder, pattern = pattern, full.names = T)
    r_file <- identify_read_fileclass(sel_file)
  }
  else if (missing(subfolderX6)==TRUE) {

        sel_subfolder <- list.files(path = act_data, pattern = subfolder, full.names = TRUE)
    sel_subfolder <- list.files(path = sel_subfolder, pattern = subfolderX2, full.names = TRUE)
    sel_subfolder <- list.files(path = sel_subfolder, pattern = subfolderX3, full.names = TRUE)
    sel_subfolder <- list.files(path = sel_subfolder, pattern = subfolderX4, full.names = TRUE)
    sel_subfolder <- list.files(path = sel_subfolder, pattern = subfolderX5, full.names = TRUE)
    sel_file <- list.files(path = sel_subfolder, pattern = pattern, full.names = T)
    r_file <- identify_read_fileclass(sel_file)
  }
  else {
    sel_subfolder <- list.files(path = act_data, pattern = subfolder, full.names = TRUE)
    sel_subfolder <- list.files(path = sel_subfolder, pattern = subfolderX2, full.names = TRUE)
    sel_subfolder <- list.files(path = sel_subfolder, pattern = subfolderX3, full.names = TRUE)
    sel_subfolder <- list.files(path = sel_subfolder, pattern = subfolderX4, full.names = TRUE)
    sel_subfolder <- list.files(path = sel_subfolder, pattern = subfolderX5, full.names = TRUE)
    sel_subfolder <- list.files(path = sel_subfolder, pattern = subfolderX6, full.names = TRUE)
    sel_file <- list.files(path = sel_subfolder, pattern = pattern, full.names = T)
    r_file <- identify_read_fileclass(sel_file)
  }
  
  return(r_file)
  rm(list=c('act_data', 'sel_file'))
}



get_dir_files <- function(module, folder, param_pattern, subfolder, subfolderX2, mainfolder, file_pattern, file_names) {
  # list the files fullpath
  
  main <- get_mainfolder_sub(module, folder, param_pattern)
  folder <- file.path(main, subfolder)

  if (missing(subfolderX2)==TRUE) {
    folder <- folder
  }
  else {
    folder <- file.path(folder, subfolderX2)
  }
  
  if (missing(mainfolder)==FALSE) {
    folder <- file.path(folder, mainfolder)
  }
  
  if(missing(file_names)==TRUE) {
    ifelse(missing(file_pattern)==FALSE,
           files <- list.files(folder, pattern = file_pattern, full.names = TRUE),
           files <- list.files(folder, full.names = TRUE))
  }
  else {
    ifelse(missing(file_pattern)==FALSE,
           files <- list.files(folder, pattern = file_pattern),
           files <- list.files(folder))
    files <- gsub('.tif', '', files)
  }
  
  return(files)
  rm(list=c('main', 'folder'))
}



get_folderpath <- function(module, main_folder, folder, subfolder) {
  # used to return the path of a given folder/subfolder
  # NOTE: this is useful when exporting data as it gives the directory
  
  main_path <- get_mainfolder_sub(module, main_folder, folder)
  
  if (missing(subfolder)==TRUE) {
    return(main_path)
  }
  else {
    sub_path <- list.files(path = main_path, pattern = subfolder, full.names = TRUE)
    return(sub_path)
  }
}


list_dir_files <- function(module, main_folder, folder, subfolder, subfolderX2, pattern) {
  
  path <- get_folderpath(module, main_folder, folder, subfolder)
  
  if (missing(subfolderX2)==TRUE) {
    files <- list.files(path = path, pattern = pattern)
  }
  else if (missing(subfolderX2)==TRUE & missing(pattern)==TRUE) {
    files <- list.files(path = path)
  }
  else {
    if (missing(pattern)==TRUE) {
      files <- list.files(path = path, pattern = subfolderX2, full.names = TRUE)
      files <- list.files(path = files)
    }
    else {
      files <- list.files(path = path, pattern = pattern)
      files <- list.files(path = files)
    }
  }
  files <- gsub('.csv','',files)
  return(files)
}


list_all_files_folder <- function(module, folder, subfolder, mainfolder, subfolderX2, subfolderX3, subfolderX4, subfolderX5, subfolderX6, full_names = FALSE) {
  # gets the path of a given file within folders/subfolders of activity data,
  # this can also be used to get output data
  
  # get activity data path and read and select a file based on the specified pattern
  if (missing(mainfolder)==TRUE) {
    act_data <- get_mainfolder_sub(module, main_folder = 'Activity_data', pattern = folder)
  }
  else {
    act_data <- get_mainfolder_sub(module, main_folder = mainfolder, pattern = folder)
  }

  if (missing(subfolder)==TRUE & missing(subfolderX2)==TRUE & missing(subfolderX3)==TRUE & missing(subfolderX4)==TRUE & missing(subfolderX5)==T & missing(subfolderX6)==T) {
    
    sel_file <- list.files(path = act_data)
  }
  else if (missing(subfolderX2)==TRUE & missing(subfolderX3)==TRUE & missing(subfolderX4)==TRUE & missing(subfolderX5)==TRUE & missing(subfolderX6)==TRUE) {
    sel_subfolder <- list.files(path = act_data, pattern = subfolder, full.names = TRUE)
    sel_file <- list.files(path = sel_subfolder, full.names = full_names)
  } 
  else if (missing(subfolderX3)==TRUE & missing(subfolderX4)==TRUE & missing(subfolderX5)==TRUE & missing(subfolderX6)==TRUE){
    sel_subfolder <- list.files(path = act_data, pattern = subfolder, full.names = TRUE)
    sel_subfolder <- list.files(path = sel_subfolder, pattern = subfolderX2, full.names = TRUE)
    sel_file <- list.files(path = sel_subfolder, full.names = full_names)
  }
  else if (missing(subfolderX4)==TRUE & missing(subfolderX5)==TRUE & missing(subfolderX6)==TRUE) {

    sel_subfolder <- list.files(path = act_data, pattern = subfolder, full.names = TRUE)
    sel_subfolder <- list.files(path = sel_subfolder, pattern = subfolderX2, full.names = TRUE)
    sel_subfolder <- list.files(path = sel_subfolder, pattern = subfolderX3, full.names = TRUE)
    sel_file <- list.files(path = sel_subfolder, full.names = full_names)
  } 
  else if (missing(subfolderX5)==TRUE & missing(subfolderX6)==TRUE) {

    sel_subfolder <- list.files(path = act_data, pattern = subfolder, full.names = TRUE)
    sel_subfolder <- list.files(path = sel_subfolder, pattern = subfolderX2, full.names = TRUE)
    sel_subfolder <- list.files(path = sel_subfolder, pattern = subfolderX3, full.names = TRUE)
    sel_subfolder <- list.files(path = sel_subfolder, pattern = subfolderX4, full.names = TRUE)
    sel_file <- list.files(path = sel_subfolder, full.names = full_names)
  }
  else if (missing(subfolderX6)==TRUE) {

        sel_subfolder <- list.files(path = act_data, pattern = subfolder, full.names = TRUE)
    sel_subfolder <- list.files(path = sel_subfolder, pattern = subfolderX2, full.names = TRUE)
    sel_subfolder <- list.files(path = sel_subfolder, pattern = subfolderX3, full.names = TRUE)
    sel_subfolder <- list.files(path = sel_subfolder, pattern = subfolderX4, full.names = TRUE)
    sel_subfolder <- list.files(path = sel_subfolder, pattern = subfolderX5, full.names = TRUE)
    sel_file <- list.files(path = sel_subfolder, full.names = full_names)
  }
  else {
    sel_subfolder <- list.files(path = act_data, pattern = subfolder, full.names = TRUE)
    sel_subfolder <- list.files(path = sel_subfolder, pattern = subfolderX2, full.names = TRUE)
    sel_subfolder <- list.files(path = sel_subfolder, pattern = subfolderX3, full.names = TRUE)
    sel_subfolder <- list.files(path = sel_subfolder, pattern = subfolderX4, full.names = TRUE)
    sel_subfolder <- list.files(path = sel_subfolder, pattern = subfolderX5, full.names = TRUE)
    sel_subfolder <- list.files(path = sel_subfolder, pattern = subfolderX6, full.names = TRUE)
    sel_file <- list.files(path = sel_subfolder, full.names = full_names)
  }
  
  return(sel_file)
  rm(list=c('act_data'))
}












list_main_params <- function(folder, INE_param, main_param) {
  # folder is e.g., Raw_data_Agrarian
  # INE_param
  data <- get_folderpath(module = 'Nutrients',main_folder = 'Activity_data',folder = folder,subfolder = INE_param)
  
  if (missing(main_param)==TRUE) {
    data <- list.files(path = data)
  }
  else {
    data <- list.files(path = data, pattern = main_param, full.names = T)
    data <- list.files(path = data)
    data <- gsub('.csv','',data)
  }
  return(data)
}


##  EXPORTERS ---------------------------------------------------------------------------


create_output_folders <- function(module, folder, subfolder, subfolderX2, subfolderX3) {
  
  out <-  get_mainfolder_sub(module, 'Output')
  
  if (missing(subfolder)==TRUE && missing(subfolderX2)==TRUE && missing(subfolderX3)==TRUE) {
    folderpath <- file.path(out, folder)
  }
  else if (missing(subfolderX2)==TRUE && missing(subfolderX3)==TRUE && missing(subfolder)==FALSE) {
    folderpath <- file.path(out, folder, subfolder)
  }
  else if (missing(subfolderX3)==TRUE) {
    folderpath <- file.path(out, folder, subfolder, subfolderX2)
  }
  else {
    folderpath <- file.path(out, folder, subfolder, subfolderX2, subfolderX3)
  }
  dir.create(path = folderpath, showWarnings = FALSE, recursive = TRUE)
  
  return(folderpath)
}


create_activityData_folders <- function(module, folder, subfolder, subfolderX2, subfolderX3) {
  
  file_path <- get_mainfolder_sub(module, main_folder = folder, pattern = subfolder)
  
  if (missing(subfolderX3)==TRUE) {
    file_path <- file.path(file_path, subfolderX2)
    dir.create(path = file_path, showWarnings = FALSE, recursive = TRUE)
  } 
  else {
    file_path <- file.path(file_path, subfolderX2, subfolderX3)
    dir.create(path = file_path, showWarnings = FALSE, recursive = TRUE)
  }

  return(file_path)
}




export_file <- function(module, file, folder, filename, subfolder, subfolderX2, subfolderX3, subfolderX4, subfolderX5, subfolderX6) {
  ## NOTE: CURRENTLY ONLY APPLIED TO RASTERS, STACKS/BRICKS AND DATAFRAMES
  
  print(paste0('Preparing to export ', filename))
  ifelse(folder=='Activity_data',
    file_path <- create_activityData_folders(module, folder, subfolder, subfolderX2), 
    file_path <- create_output_folders(module, folder, subfolder, subfolderX2) )

    if (missing(subfolderX3)==FALSE & missing(subfolderX4)==TRUE & missing(subfolderX5)==TRUE & missing(subfolderX6)==TRUE) {
      
      file_path <- file.path(file_path, subfolderX3)
      dir.create(file_path)
    }
    else if (missing(subfolderX3)==TRUE & missing(subfolderX4)==TRUE & missing(subfolderX5)==TRUE & missing(subfolderX6)==TRUE) {

      
    }
    else if (missing(subfolderX3)==FALSE & missing(subfolderX4)==FALSE & missing(subfolderX5)==TRUE & missing(subfolderX6)==TRUE) {

      file_path <- file.path(file_path, subfolderX3)
      dir.create(file_path)
      
      file_path <- file.path(file_path, subfolderX4)
      dir.create(file_path)
    }
    else if (missing(subfolderX3)==FALSE & missing(subfolderX4)==FALSE & missing(subfolderX5)==FALSE &  missing(subfolderX6)==TRUE) {

      file_path <- file.path(file_path, subfolderX3)
      dir.create(file_path)
      
      file_path <- file.path(file_path, subfolderX4)
      dir.create(file_path)

      file_path <- file.path(file_path, subfolderX5)
      dir.create(file_path)
    }
    else {
      
      file_path <- file.path(file_path, subfolderX3)
      dir.create(file_path)
      
      file_path <- file.path(file_path, subfolderX4)
      dir.create(file_path)

      file_path <- file.path(file_path, subfolderX5)
      dir.create(file_path)

      file_path <- file.path(file_path, subfolderX6)
      dir.create(file_path)
    }

  file_path <- file.path(file_path, filename)

  print(paste0('======= Exporting ', filename))
  
  # write depending on file format
  if (class(file)=='RasterLayer') {
    
    file_path <- paste0(file_path, '.tif')
    tifoptions=c('COMPRESS=DEFLATE', 'PREDICTOR=2', 'ZLEVEL=6')
    writeRaster(file, file_path, options=tifoptions, overwrite = TRUE)
  }
  else if (class(file)=='data.frame') {
    file_path <- paste0(file_path, '.csv')
    write.csv(x = file, file = file_path, row.names = FALSE)
  }
  else if (class(file)=='RasterStack') {

    file_path = ifelse(grepl('.tif',filename)==TRUE,
      paste0(file_path,'.tif'),
      paste0(file_path,'.gri'))
    tifoptions=c('COMPRESS=DEFLATE', 'PREDICTOR=2', 'ZLEVEL=6')
    writeRaster(file, file_path, options=tifoptions, overwrite = TRUE)
  }
  else if (class(file)=='SpatialPoints') {
    
    file_path <- paste0(file_path, '.shp')
    write_sf(file, file_path)
  }
}





## DATA CLEANING ----------------------------------------------------------------------------------------

data_cleaning <- function(dataset) {
  #this cleans up the data
  #Inf, NA or NaN
  
  options(warn=-1)
  dataset <- do.call(data.frame, 
                     lapply(dataset, function(x) {
                       replace(x, is.infinite(x) | is.na(x), 0)
                       }
                       )
                     )
  options(warn=0)
  # colnames(dataset) <- gsub('X', '', names(dataset))
  return(dataset)
}



## PARAM LIST -----------------------------------------------------------------------------------------------

get_standard_params_list <- function(main_param) {
  
  standard_params <- get_activity_data(module = 'Nutrients', folder = 'General_params', pattern = 'Params_list')
  
  if (main_param == 'Animals') {
    
    standard_params <-  standard_params[, c('Animals','Main_animals')]
    standard_params <- standard_params[-which(is.na(standard_params)==T), ]  
  } 
  else {
    
    standard_params <- standard_params[, c('Main_crop','Crop')]
  }

  return(standard_params)
}
