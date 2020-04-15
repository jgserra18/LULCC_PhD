source('./Nutrients/Model/INE_DB/Populate_INE_AgrarianRegion.R')


## ----------------------- WEB SCRAPING FUNCTIONS --------------------- ##
## ---------------------------------------------------------------------##

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
  
  require(doParallel)
  cl <- registerDoParallel(cores=3)
  getDoParWorkers()
  
  df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  store <- foreach(i=1:nrow(df), 
                   .export = c('update_get_INE_data', 'get_INE_data'),
                   .combine = rbind,
                   .packages = c('jsonlite')) %dopar% {
                     
                     for (j in year) {
                       
                       js_year <- paste0('S7A', j)
                       print(js_year)
                       df[i, year] <- update_get_INE_data(INE_param_id = INE_param_id, year = js_year, muni_id = df[i, 'ID'], var_id = var_id)
                     }
                   }
  return(store)
}
  


  for (i in 1:nrow(df)) {
    
    print(paste0('Muni ID is ', df[i, 'ID']))
    for (j in year) {
      
      js_year <- paste0('S7A', j)
      print(js_year)
      df[i, year] <- update_get_INE_data(INE_param_id = INE_param_id, year = js_year, muni_id = df[i, 'ID'], var_id = var_id, other_params = other_params)
    }
  }
  return(df)
}

df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Crop_ids') 
path <- create_activityData_folders(module = 'Nutrients', folder = 'Activity_data', subfolder = 'Raw_data_Municipality', subfolderX2 = 'Areas')

for (i in 1:1) {
  
  INE_param_id <- substr(x = df[i,1], start = 2, stop = 8)
  
  main_crop <- df[i, 'Main_crop']
  crop <- df[i, 'Crop']
  var_id <- df[i, 'Crop_id']
  
  crop_df <- get_municipality_INE(INE_param_id = INE_param_id, var_id = var_id, year = '1999')
  export_file(module = 'Nutrients',
              file = crop_df, folder = 'Activity_data', 
              filename = crop, subfolder = 'Raw_data_Municipality', subfolderX2 = 'Areas', subfolderX3 = main_crop)
}


