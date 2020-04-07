source('./Model/Main/Main_functions.R')


library(rjson)


## ----------------------- WEB SCRAPING FUNCTIONS --------------------- ##
## ---------------------------------------------------------------------##


get_INE_data <- function(INE_param_id, year, muni_id, var_id, other_params) {
  
  print('Exporting INE DB ...... ')
  # convert to json file
  url <- paste0('https:/www.ine.pt/ine/json_indicador/pindica.jsp?op=2&varcd=', INE_param_id, 
                 '&Dim1=', year, '&Dim2=', 
                 muni_id, '&Dim3=', 
                 var_id, '&lang=PT')
  url <- gsub('/', '//', url)
  # test <- getURL(URLencode(test))
  json_df <- jsonlite::fromJSON(url)

  if (missing(other_params)==TRUE) {
    
    json_df <- json_df[[7]][[1]][[1]][5]
    return(json_df)
  } 
  else {
    
    json_df <- json_df[[7]][[1]][[1]]
    json_df <- subset(json_df, dim_4=='T' & dim_5 =='T')[ncol(json_df)]
    return(json_df)
  }
}


get_agrarian_region_INE <- function(INE_param_id, var_id, 
                                    year = seq(1987,2017), 
                                    muni_id = as.character(seq(1,7)),
                                    other_params) {
  # EXCEPTIONS: FORAGE, HORTICULTURE, POULTRY, RABBITS
  
  # param_modifier regards information that shifts the IDs of the different agrarian regions
  # this is either because there are no regional data (horticulture) 
  # or because data was collated from the census
  

  df <- data.frame()
  print(muni_id)
  for (i in seq_along(muni_id)) {
    
    print(paste0('Agrarian region: ', i))
    df[i, 'id'] <- i
    
    for (j in year) {
      
      print(paste0('Year: ', j))
      js_year <- paste0('S7A', j)
      df[i, as.character(j)] <- get_INE_data(INE_param_id, js_year, muni_id[i], var_id, other_params)
    }
  }
  return(df)
}


## ----------------------- SCRAP AND POPULATE CROP DATA --------------------- ##
## ---------------------------------------------------------------------------##

forage_horticulture_populate_modifier <- function(var_id,
                                                  muni_id = as.character(seq(11,17))) {
  # in work
  
  # 0005640 is 2009,2013,2016
  # 0002749 1989,1993,1995,1997,1999,2003,2005,2007
  
  crop_dfA <- get_agrarian_region_INE(INE_param_id = '0002749', var_id = var_id, year = c(1989,1993,1995,1997,1999,2003,2005,2007), muni_id = muni_id, other_params = 'Yes')
  crop_dfB <- get_agrarian_region_INE(INE_param_id = '0005637', var_id = var_id, year = c(2009,2013,2016), muni_id = muni_id, other_params = 'Yes')
  
  crop_df <- merge(crop_dfA, crop_dfB,'id')
  return(crop_df)
}



populate_perma_pastures_DB <- function(muni_id = as.character(seq(11,17))) {
  
    crop_df <- get_agrarian_region_INE(INE_param_id = '0003485', var_id = 'T', year = as.character(c(1989,1993,1995,1997,1999,2003,2005,2007,2009,2013)), muni_id = muni_id)
    export_file(file = crop_df, folder = 'Activity_data', filename = 'extensive_pasture', subfolder = 'Raw_data_Agrarian', 
                subfolderX2 = 'Areas', subfolderX3 = 'Pastures')
}


main_populate_crop_param_DB <- function() {
  # EXCEPTIONS: FORAGE, HORTICULTURE, POULTRY, RABBITS
  
  # this populates the activity data for crop parameters (yields, acreage) based on the "Crop_ids" file
  # parses Statistics Portugal
  # units: YIELDS (kg FM yr-1)
  # units: AREAS (ha yr-1)
  
  params_df <-  get_activity_data(folder = 'Raw_data_Agrarian', pattern = 'Crop_ids')
  params <- data.frame(Param_name = c('Areas','Yields'),
                       Param_id = c('0000019', '0000023'))
  
  for (i in 1:nrow(params)) {
    
    INE_param_id <- params[i,2] 

    for (j in 1:nrow(params_df)) {
      
      print(params_df[j, 'Crop_id'])
      
      var_id <- params_df[j, 'Crop_id']
      main_crop <- params_df[j,'Main_crop']
      
      crop_df <- get_agrarian_region_INE(INE_param_id = INE_param_id, var_id = var_id)
      
      # create directory for params
      path <- create_activityData_folders(folder = 'Activity_data', subfolder = 'Raw_data_Agrarian', subfolderX2 = params[i,1])
      crop_name <- params_df[j, 'Crop']
      
      print('========== Exporting ....')
      export_file(file = crop_df, folder = 'Activity_data', filename = crop_name, subfolder = 'Raw_data_Agrarian', subfolderX2 = params[i,1], subfolderX3 = main_crop)
    }
  }
  # clean the work environment
  rm(list=c('params_df', 'params', 'var_id', 'crop_df', 'path', 'crop_name'))
}


populate_other_crops_param_DB <- function() {
  # populate INE data for forage and horticultural crops as well as intensive pastures
  
  params_df <-  get_activity_data(folder = 'Raw_data_Agrarian', pattern = 'Other_crops_ids')
  
  for (i in 1:nrow(params_df)) {
    
    print(params_df[i, 'Crop_id'])
    var_id <- params_df[i, 'Crop_id']
    crop_name <- params_df[i, 'Crop']
    crop_df <- forage_horticulture_populate_modifier(var_id = var_id)
    
    print('========== Exporting ....')
    export_file(file = crop_df, folder = 'Activity_data', filename = crop_name, subfolder = 'Raw_data_Agrarian', subfolderX2 = 'Areas', subfolderX3 = params_df[i,2])
  }
}



## ----------------------- CORRECT VALUES --------------------- ##
## -------------------------------------------------------------##


correct_missing_values_vars <- function(param, main_var) {
  # SELF-EXPLANATORY, CORRECTS "-"
  # param is Areas, Yields
  # main_var is e.g., Cereals
  
  var_path <- get_mainfolder_sub(main_folder = 'Activity_data', pattern = 'Raw_data_Agrarian')
  var_path <- file.path(var_path,param,main_var)
  
  files <- list.files(path = var_path, full.names = TRUE)
  
  for (i in files) {
    
    print(i)
    # correct NAs
    r_file <- read.csv(i, stringsAsFactors = F)
    r_file <- sapply(r_file, as.numeric)
    
    # check if NAs exist
    condition <- which(is.na(r_file)==TRUE,arr.ind = TRUE)
    
    if (dim(condition)[1]>0) {
      print('Correcting "-".')
      
      crr_NA <- which(is.na(r_file)==TRUE, arr.ind = T)
      for (j in 1:nrow(crr_NA)) {
        
        id_1 <- crr_NA[j,1]
        id_2 <- crr_NA[j,2]
        
        r_file[id_1,id_2] <- 0
        write.csv(x = r_file, file = i, row.names = F)
      }
    }
    else {
      print('No NAs')
    }
  }
}


correct_all_missing_values <- function() {
  
  params <- c('Areas','Yields')
  
  for (i in params) {
    
    # find the main vars
    var_path <- get_mainfolder_sub(main_folder = 'Activity_data', pattern = 'Raw_data_Agrarian')
    var_path <- file.path(var_path,i)
    main_vars <- list.files(path = var_path)
    
    # now correct every main_var
    for (j in main_vars) {
      
      correct_missing_values_vars(param = i, main_var = j)
    }
  }
}


## ----------------------- TIMSERIES LINEAR EXTRAPOLATION --------------------- ##
## -----------------------------------------------------------------------------##

interpolate_other_crops_timeseries <- function(param, main_crop, crop) {
  # param is either Areas or Yields
  # RULE: if estimated value is lower than min or max, the estimated value is set to either the min or max
  # linear extrapolation
  
  print('========= Reading')
  crop_df <- get_activity_data(folder = 'Raw_data_Agrarian', pattern = crop, subfolder = param, subfolderX2 = main_crop)
  names(crop_df) <- gsub('X','',names(crop_df))
  
  print('==== Creating template df.')
  # create template df
  df <- data.frame(id = seq(1,7))
  yrs <- seq(1987,2017)
  
  for (i in yrs) {
    df[, as.character(i)] <- NA
  }

  print('==== Populating template df with crop df.')
  # populate template df with crop_df
  # missing years are populated with NA
  new_yrs <- names(crop_df)[-1]
  
  for (i in new_yrs) {
    df[, as.character(i)] <- crop_df[, as.character(i)]
  }
  df <- df[, -1]
  
  print('==== Interpolating timeseries.')
  #linearly interpolate years without any value
  for (i in 1:nrow(df)) {
    
    # select x_out for different crops
    if (crop=='extensive_pasture') {
      xout <- c(1987,1988,1990,1991,1992,1994,1996,1998,2000,2001,2002,2004,2006,2008,2010,2011,2012,2014,2015,2016,2017)
    } else if (crop=='intensive_pasture') {
      xout <- c(1987,1988,1990,1991,1992,1994,1996,1998,2000,2001,2002,2004,2006,2008,2010,2011,2012,2014,2015,2017)
    } else {
      xout <- c(1987,1988,1990,1991,1992,1994,1996,1998,2000,2001,2002,2004,2006,2008,2010,2011,2012,2014,2015,2017)
    }
    
    new_df <- approx(x= as.numeric(yrs), y = df[i,], 
                     xout = xout, rule = 2)
    
    inter_years <- new_df[[1]]
    inter_values <- round(new_df[[2]], 0)
    
    ctr <- 0
    for (j in inter_years) {
      ctr <- ctr + 1
      df[i, as.character(j)] <- inter_values[ctr]
    }
  }
  df[, 'id'] <- seq(1,7)
  df <- df[, c('id', as.character(seq(1987,2017)))]
  
  return(df)
  rm(list=c('crop_df','yrs','new_yrs','new_df','inter_years','inter_values','ctr'))
}


loop_interpolate_other_crops_timeseries <- function() {
  
  params_df <-  get_activity_data(folder = 'Raw_data_Agrarian', pattern = 'Other_crops_ids')
  
  for (i in 1:nrow(params_df)) {
    
    main <- as.character(params_df[i, 'Main_crop'])
    crops <- as.character(params_df[i, 'Crop'])
    print('======== Interpolating ')
    crop_df <- interpolate_other_crops_timeseries(param = 'Areas', main_crop = main, crop =crops)
    
    print('========== Exporting ....')
    export_file(file = crop_df, folder = 'Activity_data', filename = crops, subfolder = 'Raw_data_Agrarian', subfolderX2 = 'Areas', subfolderX3 = main)
  }
}


## ----------------------- SCRAP AND POPULATE ?IMAL DATA ------------------- ##
## ---------------------------------------------------------------------------##


convert_forage_crop_categories <- function() {
  # forage - forage maize, forage oat, annual mixtures, other_forage
  
  
   
}

## ----------------------- SCRAP AND POPULATE ANIIMAL DATA ------------------- ##
## ---------------------------------------------------------------------------##


## ----------------------- RUN ME ------------------- ##
## ---------------------------------------------------##

# scrape data related to the acreage of permanent/intensive pastures, common crops and forage/horticultural crops
# data for pastures, forage and horticulture are only available for a few years, so these were linearly interpolated to years with missing data
# no yield data are available for pastures and horticulture; for forage crops, there are only 5 years

populate_perma_pastures_DB()
main_populate_crop_param_DB()
populate_other_crops_param_DB()

# correct missing values from INE database
correct_all_missing_values()

#linearly interpolate the acreage of horticulture, forage and pastures
loop_interpolate_other_crops_timeseries()