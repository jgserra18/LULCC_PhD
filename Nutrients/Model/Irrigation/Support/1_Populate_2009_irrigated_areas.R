source('./Nutrients/Model/INE_DB/Population_INE_Municipality.R')
source('./Nutrients/Model/INE_DB/Populate_INE_AgrarianRegion.R')
source('./Nutrients/Model/INE_DB/Standardize_INE_params.R')
source('./Main/Data_operations.R')



library(doParallel)



# get irrigated areas per crop and irrigation method for 2009 -----------------------------------------------------------------------------------------

set_irrigation_methods_INE_codes = function() {
  
  irrig_methods = data.frame(methods=c('furrows','other_gravity','sprinkler','gun','pivot','drip','microsprink'),
                             code = c('11','12','211','212','213','221','222'))
  return(irrig_methods)
}



get_INE_irrigated_areas = function(INE_param_id, muni_id, irrigation_method_id, crop_id) {
  # main function to web scrap irrigated areas per crop and irrigation system from Statistics Portugal
  
  url <- paste0('https:/www.ine.pt/ine/json_indicador/pindica.jsp?op=2&varcd=', INE_param_id, 
                '&Dim1=', 'S7A2009', '&Dim2=', 
                 muni_id, '&Dim3=', 
                 irrigation_method_id, '&Dim4=', crop_id, '&lang=PT')
  url <- gsub('/', '//', url)
  json_df <- jsonlite::fromJSON(url)
  json_df <- json_df[[7]][[1]][[1]][[7]]
  print(json_df)
  return(json_df)
}


get_municipality_irrigated_areas_method <- function(INE_param_id, 
                                                    irrigation_method_id, 
                                                    crop_id) {
  # general function to scrape municipality data from Statistics Portugal
  # scrapes data for a given irrigated crop and irrigation method for all municipalities
  
  
  df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  df[1:nrow(df), 'X2009'] = sapply(1:nrow(df), function(x) {
    INE_value <- get_INE_irrigated_areas(INE_param_id = INE_param_id,
                                         muni_id = as.character(df[x, 'ID']), 
                                         irrigation_method_id = irrigation_method_id, 
                                         crop_id = crop_id)
  })

  corr_vals = which(df=='-',arr.ind = T)
  
  for (i in 1:nrow(corr_vals)) {
    
    r = corr_vals[i,1]
    c = corr_vals[i, 2]
    df[r,c] = 0
  }
  
  return(df)
}



get_irrigation_method_all_crops = function(irrigation_method_id, irrigation_method_name) {
  
  irrig_id = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Irrigation', subfolderX2 = 'Portugal_statistics', pattern = 'Crop_ids')
  sapply(1:nrow(irrig_id), function(i) {
    
    INE_param_id = substr(x = irrig_id[i,1], start = 2, stop = 8)
    main_param = irrig_id[i,2]
    param = irrig_id[i,3]
    crop_id = as.character(irrig_id[i, 4])
    
    print(paste0('Starting to export crop data: ', crop_id))
    irrig_area_method_muni = get_municipality_irrigated_areas_method(INE_param_id = INE_param_id, 
                                                                     irrigation_method_id = irrigation_method_id, 
                                                                     crop_id = crop_id)
    export_file(module = 'Nutrients', 
                folder = 'Activity_data', 
                subfolder = 'Correct_data_Municipality', 
                subfolderX2 = 'Irrigation', 
                subfolderX3 = 'Preprocessing_X2009',
                subfolderX4 = 'Irrigated_areas',
                subfolderX5 = irrigation_method_name, 
                subfolderX6 = main_param, 
                filename = ifelse(param=='Other_industry','Tomato',param), 
                file = irrig_area_method_muni)
  })
}


get_all_irrigation_method_all_crops = function() {
  
  irrig_methods = set_irrigation_methods_INE_codes()
  
  for (i in 1:nrow(irrig_methods)) {
    
    irrigation_method_name = irrig_methods[i,1]
    irrigation_method_id = irrig_methods[i, 2]
    
    get_irrigation_method_all_crops(irrigation_method_id, irrigation_method_name)
  }
}



