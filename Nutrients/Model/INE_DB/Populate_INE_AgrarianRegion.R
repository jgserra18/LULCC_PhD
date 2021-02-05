source('./Main/Global_functions.R')


library(rjson)


## WEB SCRAPING FUNCTIONS ------------------------------------------------------------------------------------------


get_INE_data <- function(INE_param_id, year, muni_id, var_id, other_params) {
  #' @param INE_param_id ID from Statistics Portugal regarding each individual datatable;specifies to the database this (e.g., "0002749")
  #' @param year year using special signature ("S7A2009")
  #' @param muni_id Municipality id
  #' @param var_id Within the datatable, specifies a given crop/animal
  #' @param other_params other_params specifies whether it is agrarian region data (if missing), Muni data from the Census or muni data from "outside" the census
  #' @description gets INE data
  #' @return a dataframe scraped and parsed from INE database

  print('Exporting INE DB ...... ')
  # convert to json file
  url <- paste0('https:/www.ine.pt/ine/json_indicador/pindica.jsp?op=2&varcd=', INE_param_id, 
                 '&Dim1=', year, '&Dim2=', 
                 muni_id, '&Dim3=', 
                 var_id, '&lang=PT')
  url <- gsub('/', '//', url)
  json_df <- jsonlite::fromJSON(url)

  if (missing(other_params)==TRUE ) {
    
    json_df <- json_df[[7]][[1]][[1]][5]
  } 
  
  else if (other_params == 'Def') {
    
    json_df <- json_df[[7]][[1]][[1]]
    
    if ("valor" %in% colnames(json_df)) {
      
      json_df <- json_df[which(json_df$dim_3==var_id), "valor"]
    }
    else {
      print('Value here is set to 0.')
      json_df <- "0"
    }
  }
  
  else if (other_params == 'Muni') {
    
      json_df <- json_df[[7]][[1]][[1]]
      
      if ("valor" %in% colnames(json_df)) {
        
        json_df <- json_df[which(json_df$dim_4=='T'), "valor"]
      }
      else {
        print('Value here is set to 0.')
        json_df <- "0"
      }
  }
  
  else if (other_params == 'Wool') {
    
    json_df <- json_df[[7]][[1]][[1]][[3]][[1]]
  }
  else {
    json_df <- json_df[[7]][[1]][[1]]
    json_df <- subset(json_df, dim_4=='T' & dim_5 =='T')[ncol(json_df)]
  }
  
  return(json_df)
}


get_agrarian_region_INE <- function(INE_param_id, var_id, 
                                    year = seq(1987,2017), 
                                    muni_id = as.character(seq(1,7)),
                                    other_params) {
  #' @note see get_INE_data for param description
  #' @details EXCEPTIONS: FORAGE, HORTICULTURE, POULTRY, RABBITS
  #' @description param_modifier regards information that shifts the IDs of the different agrarian regions
  #' this is either because there are no regional data (horticulture) 
  #' or because data was collated from the census
  

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


##  SCRAP AND POPULATE CROP DATA --------------------------------------------------------------------------


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
    #export_file(module = 'Nutrients',file = crop_df, folder = 'Activity_data', filename = 'Extensive_pasture', subfolder = 'Raw_data_Agrarian', 
        #        subfolderX2 = 'Areas', subfolderX3 = 'Pastures')
      return(crop_df)
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
      path <- create_activityData_folders(module = 'LULCC', folder = 'Activity_data', subfolder = 'Raw_data_Agrarian', subfolderX2 = params[i,1])
      crop_name <- params_df[j, 'Crop']
      
      print('========== Exporting ....')
      export_file(module = 'Nutrients',
                   file = crop_df, folder = 'Activity_data', 
                   filename = crop_name, subfolder = 'Raw_data_Agrarian', subfolderX2 = params[i,1], subfolderX3 = main_crop)
    }
  }
  # clean the work environment
  rm(list=c('params_df', 'params', 'var_id', 'crop_df', 'path', 'crop_name'))
}


# This can be incorporated into Crop_ids or other_crop_ids -----------------

get_incomplete_crop_yields <- function(main_param) {
  # gets forage yields from Statistics Portugal
  # Assumptions ------
  # 1 - Total forage crop yields --> other_forage, forage_sorghum, forage_roots, annual_mixtures
  # 2 - yields before 2011 are assumed that of 2011
  
  if (main_param == 'Forage') {
    params <- data.frame(crops = c('forage_maize','forage_oat','Annual_mixtures','forage_sorghum','other_forage','forage_roots'),
                         var_id = c('10601','10602','106','106','106','106'))
    for (i in 1:nrow(params)) {
      
      param <- params[i,1]
      var_id <- params[i,2]
      crop_df <- get_agrarian_region_INE(INE_param_id = '0000023', var_id = var_id, year = seq(2011,2017))
      
      AR <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Agrarian', subfolder = 'Yields', subfolderX2 = 'Cereals' , pattern = 'Wheat') #template
      
      data_yrs <- paste0('X',seq(2011,2017))
      AR[, data_yrs] <- sapply(data_yrs, function(x) crop_df[, gsub('X','',x)])
      
      fill_yrs <- paste0('X',seq(1987,2010))
      AR[, fill_yrs] <- sapply(fill_yrs, function(x) AR[, x] <- AR[, 'X2011'])
      
      export_file(module = 'Nutrients', file = AR, folder = 'Activity_data', filename = param, subfolder = 'Raw_data_Agrarian', subfolderX2 = 'Yields', subfolderX3 = main_param)
    }
  } 
  else {
    params <- data.frame(crops = c('Horticulture_intensive', 'Horticulture_extensive'),
                         var_id = c('105','105'))
    
    for (i in 1:nrow(params)) {
      
      param <- params[i,1]
      var_id <- params[i,2]
      crop_df <- get_agrarian_region_INE(INE_param_id = '0000023', var_id = var_id, year = seq(2011,2017), muni_id = 'PT')
      
      # fill the dataframe with only the national values
      crop_df[2:7, ] <- crop_df[1,]
      crop_df[, 1] <- as.character(seq(1,7))

      AR <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Agrarian', subfolder = 'Yields', subfolderX2 = 'Cereals' , pattern = 'Wheat') #template
      
      data_yrs <- paste0('X',seq(2011,2017))
      AR[, data_yrs] <- sapply(data_yrs, function(x) crop_df[, gsub('X','',x)])
      
      fill_yrs <- paste0('X',seq(1987,2010))
      AR[, fill_yrs] <- sapply(fill_yrs, function(x) AR[, x] <- AR[, 'X2011'])
      
      export_file(module = 'Nutrients', file = AR, folder = 'Activity_data', filename = param, subfolder = 'Raw_data_Agrarian', subfolderX2 = 'Yields', subfolderX3 = main_param)
    }
  }
  correct_missing_values_vars(param = 'Yields', main_var = main_param)
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
    export_file(module = 'Nutrients', file = crop_df, folder = 'Activity_data', filename = crop_name, subfolder = 'Raw_data_Agrarian', subfolderX2 = 'Areas', subfolderX3 = params_df[i,2])
  }
}



##  CORRECT VALUES --------------------------------------------------------------------------------------------------


correct_missing_values_vars <- function(param, main_var) {
  # SELF-EXPLANATORY, CORRECTS "-"
  # param is Areas, Yields
  # main_var is e.g., Cereals
  
  var_path <- get_mainfolder_sub(module = 'Nutrients',main_folder = 'Activity_data', pattern = 'Raw_data_Agrarian')
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
    var_path <- get_mainfolder_sub(module = 'Nutrients',main_folder = 'Activity_data', pattern = 'Raw_data_Agrarian')
    var_path <- file.path(var_path,i)
    main_vars <- list.files(path = var_path)
    
    # now correct every main_var
    for (j in main_vars) {
      
      correct_missing_values_vars(param = i, main_var = j)
    }
  }
}


##  TIMSERIES LINEAR EXTRAPOLATION ---------------------------------------------------------------------------------


interpolate_other_crops_timeseries <- function(param, main_crop, crop) {
  # param is either Areas or Yields
  # RULE: if estimated value is lower than min or max, the estimated value is set to either the min or max
  # linear extrapolation
  
  print('========= Reading')
  crop_df <- get_activity_data(module = 'Nutrients',folder = 'Raw_data_Agrarian', pattern = crop, subfolder = param, subfolderX2 = main_crop)
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
    if (crop=='Extensive_pasture') {
      xout <- c(1987,1988,1990,1991,1992,1994,1996,1998,2000,2001,2002,2004,2006,2008,2010,2011,2012,2014,2015,2016,2017)
    } else if (crop=='Intensive_pasture') {
      xout <- c(1987,1988,1990,1991,1992,1994,1996,1998,2000,2001,2002,2004,2006,2008,2010,2011,2012,2014,2015,2017)
    } 
    # also adapt for animal population
    else if (main_crop =='Equides') {
      xout <- c(2014,2015,2017)
    } else if (main_crop == 'Sheep') {
      xout <- c(2016)
    }
    else {
      xout <- c(1987,1988,1990,1991,1992,1994,1996,1998,2000,2001,2002,2004,2006,2008,2010,2011,2012,2014,2015,2017)
    }
    
   # new_df = data.frame(y=as.numeric(df[1,]), x = xout)
    
    
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
  
  params_df <-  get_activity_data(module = 'Nutrients',folder = 'Raw_data_Agrarian', pattern = 'Other_crops_ids')
  
  for (i in 1:nrow(params_df)) {
    
    main <- as.character(params_df[i, 'Main_crop'])
    crops <- as.character(params_df[i, 'Crop'])
    print('======== Interpolating ')
    crop_df <- interpolate_other_crops_timeseries(param = 'Areas', main_crop = main, crop =crops)
    
    print('========== Exporting ....')
    export_file(module = 'Nutrients',file = crop_df, folder = 'Activity_data', filename = crops, subfolder = 'Raw_data_Agrarian', subfolderX2 = 'Areas', subfolderX3 = main)
  }
}



## SCRAP AND POPULATE ANIIMAL DATA -------------------------------------------------------------------------

get_agrarian_bovine <- function(var_id, 
                                year = seq(1987,2017), 
                                muni_id = as.character(seq(1,7))) {
  # modifies get_agrarian_region_INE to account for the different dataset parameters related to cattle
  
  df <- data.frame()
  print(muni_id)
  for (i in seq_along(muni_id)) {
    
    print(paste0('Agrarian region: ', i))
    df[i, 'id'] <- i
    
    for (j in year) {
      
      print(paste0('Year: ', j))
      js_year <- paste0('S6A', j, '2')
      df[i, as.character(j)] <- get_INE_data(INE_param_id = '0000543', js_year, muni_id[i], var_id)
    }
  }
  return(df)
}


main_populate_animals_pop_DB <- function() {
  # EXCEPTIONS: FORAGE, HORTICULTURE, POULTRY, RABBITS
  
  # this populates the activity data for crop parameters (yields, acreage) based on the "Crop_ids" file
  # parses Statistics Portugal
  # units: YIELDS (kg FM yr-1)
  # units: AREAS (ha yr-1)
  
  params_df <-  get_activity_data(module = 'Nutrients',folder = 'Raw_data_Agrarian', pattern = 'Animals_id')
  
  for (i in 1:nrow(params_df)) {
    
    INE_param_id <- substr(x = params_df[i,1], start = 2, stop = 8)
    var_id <- params_df[i, 'Animals_id']
    main_cat <- params_df[i,'Main_animals']

    if (main_cat == 'Bovine') {
      
      animals_df <- get_agrarian_bovine(var_id = var_id)
    }
    else if (main_cat == 'Equides') {
      
      animals_df <- get_agrarian_region_INE(INE_param_id = INE_param_id, var_id = var_id, year = seq(1987,2016))
    }
    else if (main_cat == 'Rabbits' | main_cat == 'Poultry') {
      
      animals_df <- get_agrarian_region_INE(INE_param_id = INE_param_id, var_id = var_id, year = c(1989,1993,1995,1997,1999,2003,2005,2007,2009,2013,2016),muni_id = as.character(seq(11,17)))
    }
    else {
      
      animals_df <- get_agrarian_region_INE(INE_param_id = INE_param_id, var_id = var_id)
    }
    
    # create directory for params
    path <- create_activityData_folders(module = 'Nutrients', folder = 'Activity_data', subfolder = 'Raw_data_Agrarian', subfolderX2 = 'Animals')
    animal_name <- params_df[i, 'Animals']
    
    print('========== Exporting ....')
    export_file(module = 'Nutrients',
                file = animals_df, folder = 'Activity_data', 
                filename = animal_name, subfolder = 'Raw_data_Agrarian', subfolderX2 = 'Animals', subfolderX3 = main_cat)
    # clean the work environment
    rm(list=c('params_df', 'var_id', 'animals_df', 'path', 'animal_name'))
  }
}


correct_animal_population_DB <- function() {
  # fix   "o" "*"
  
  animals <- c('Bovine','Equides','Goats','Pigs','Poultry','Rabbits','Sheep')

  for (i in animals) {
    correct_missing_values_vars(param = 'Animals', main_var = i)
  }
}


correct_animal_population_numbers <- function() {
  # add + "000" because units are given in thousands (except rabbits, poultry)
  
  animals <- c('Bovine','Equides','Goats','Pigs','Sheep')
  var_path <- get_mainfolder_sub(module = 'Nutrients',main_folder = 'Activity_data', pattern = 'Raw_data_Agrarian')
  var_path <- file.path(var_path,'Animals')
  
  for (i in animals) {
    main_vars <- list.files(path = var_path, pattern = i, full.names = TRUE)
    vars_file <- list.files(path = main_vars, full.names = T)
    
    for (z in vars_file) {
      print(z)
      r_file <- read.csv(z, stringsAsFactors = F)
      r_file <- r_file * 1000
      r_file[,1] <- r_file[,1]/1000
      
      data.table::fwrite(x = r_file, file = z, row.names = FALSE)
    }
  }
}



loop_interpolate_animals <- function(param='Animals') {
  
  animals <- c('Equides','Sheep','Poultry','Rabbits')
  
  #horses - 2014,2015, 2017
  # ewes - 2016
  # poultry, rabbits default function
  
  for (i in animals) {
    
    names <- list_dir_files(module = 'Nutrients', main_folder = 'Activity_data', folder = 'Raw_data_Agrarian', subfolder = 'Animals', subfolderX2 = i)
    
    for (j in names) {
      
      animal_df <- interpolate_other_crops_timeseries(param = 'Animals', main_crop = i, crop = j)
      export_file(module = 'Nutrients', file = animal_df, filename = j, folder = 'Activity_data', subfolder = 'Raw_data_Agrarian', subfolderX2 = 'Animals', subfolderX3 = i)
    }
  }
}


disaggregate_poultry_hens <- function() {
  # based on Statistics Portugal historical trend (2003-2019) of hens data
  # average ratio of laying hens/ (laying + rep hens) is 0.30
  
  hens <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Agrarian', subfolder = 'Animals', subfolderX2 = 'Poultry', pattern = 'Rep_laying_hens')
  
  calc_cols <- seq(2, ncol(hens))
  
  rep_hens <- hens
  rep_hens[, calc_cols] <- sapply(rep_hens[, calc_cols], function(x) round(x*0.3,0))
  export_file(module = 'Nutrients', file = rep_hens, filename = 'Rep_hens', folder = 'Activity_data', subfolder = 'Raw_data_Agrarian', subfolderX2 = 'Animals', subfolderX3 = 'Poultry')
  
  laying_hens <- hens
  laying_hens[, calc_cols] <- sapply(laying_hens[, calc_cols], function(x) round(x*0.7,0))
  export_file(module = 'Nutrients', file = laying_hens, filename = 'Laying_hens', folder = 'Activity_data', subfolder = 'Raw_data_Agrarian', subfolderX2 = 'Animals', subfolderX3 = 'Poultry')
  
  # delete Rep_laying_hens
  file.remove('./Nutrients/Activity_data/Raw_data_Agrarian/Animals/Poultry/Rep_laying_hens.csv')
  rm(list=c('hens','calc_cols','rep_hens','laying_hens'))
}




