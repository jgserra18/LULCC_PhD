source('./Nutrients/Model/INE_DB/Populate_INE_AgrarianRegion.R')
  
library(doParallel)


##  WEB SCRAPING FUNCTIONS -------------------------------------------------------------------------------------


update_get_INE_data <- function(INE_param_id, year, muni_id, var_id, other_params) {
  # permanent grasslands have a different dimension within the JSON file
  
  if (INE_param_id == '0004331' && missing(var_id)==TRUE) {
    
    url <- paste0('https:/www.ine.pt/ine/json_indicador/pindica.jsp?op=2&varcd=', '0004331', 
                  '&Dim1=',year , '&Dim2=', muni_id,
                   '&lang=PT')
    url <- gsub('/', '//', url)
    json_df <- jsonlite::fromJSON(url)
    json_df <- json_df[[7]][[1]][[1]][[3]]
  }
  else {
    json_df <- get_INE_data(INE_param_id, year, muni_id, var_id, other_params)
  }
  return(json_df)
}


get_municipality_INE <- function(INE_param_id, var_id, 
                                 year = c('1989','1999','2009'),
                                 other_params) {
  # general function to scrape municipality data from Statistics Portugal
  
  cl <- registerDoParallel(cores=4)
  getDoParWorkers()
  
  df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  
  for (j in year) {
    
    js_year <- paste0('S7A', j)
    store <- foreach(i=1:nrow(df), 
                     .export = c('update_get_INE_data', 'get_INE_data', 'get_activity_data'),
                     .combine = 'rbind',
                     .packages = 'jsonlite') %dopar% {
                      
                       INE_value <- update_get_INE_data(INE_param_id = INE_param_id, 
                                                        year = js_year, 
                                                        muni_id = as.character(df[i, 'ID']), 
                                                        var_id = var_id, 
                                                        other_params = other_params)
                       data.frame(val = INE_value,
                                  i = i)
                     }
    df[, j] <- store[, 'val']
  }
  return(df)
}




populate_func_municipality_INE <- function(ids_file) {
  # ids_file is either "Crop_ids" or "Animals_id"
  # reads each row of ids_file
  # gets main_param (e.g., Bovine or Cereals) and respective params (e.g., Dairy_coows)
  
  df_id <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = ids_file) 
  
  ifelse(ids_file == 'Crop_ids', subX2 <- 'Areas', subX2 <- 'Animals')
  path <- create_activityData_folders(module = 'Nutrients', folder = 'Activity_data', subfolder = 'Raw_data_Municipality', subfolderX2 = subX2)
  
  
  for (i in 1:nrow(df_id)) {
    
    INE_param_id <- substr(x = df_id[i,1], start = 2, stop = 8)
    
    main_param <- df_id[i, 2]
    param <- df_id[i, 3]
    print(paste0('Working with ', param))
    var_id <- df_id[i,4]
    print(paste0(param, '\n', var_id, '\n'))
    other_param <- df_id[i, 5]
    
    calc_df <- get_municipality_INE(INE_param_id = INE_param_id, var_id = var_id, other_params = other_param)
    export_file(module = 'Nutrients',
                file = calc_df, folder = 'Activity_data', 
                filename = param, subfolder = 'Raw_data_Municipality', subfolderX2 = subX2, subfolderX3 = main_param)
  }
  rm(list=c('df_id', 'path', 'INE_param_id', 'main_param','param','var_id','calc_df'))
}

loop_populate_municipality_INE <- function() {
  
  sapply(c('Crop_ids', 'Animals_id'), populate_func_municipality_INE)
}


