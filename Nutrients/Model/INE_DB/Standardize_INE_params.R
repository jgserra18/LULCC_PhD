source('./Nutrients/Model/INE_DB/Populate_INE_AgrarianRegion.R')




##  AGGREGATE DAT AT THE AGRARIAN REGION ---------------------------------------------------------------------------


create_AR_template <- function() {
  # creates default template for AR conversions
  
  df <- data.frame(id=seq(1,7))
  return(df)
}


aggregate_INE_muni_agrarian <- function(AR_id, df_merge, year_sum) {
  # AR_id is the agrarian region id
  # year_sum is the column name to sum
  # sums the total acreage of a given municipality (or animal population) within a specified agrarian region
  
  disagg_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Spatial_disaggregation')
  
  # subset based on the AR_id
  sb_df <- subset(disagg_df, agrarian_region_id == AR_id)
  sb_df <- plyr::join(sb_df, df_merge)
  # compute the Muni_sum within the AR
  AR_sum <- sum(sb_df[, paste0('X', year_sum)])
  
  return(AR_sum)
  rm(list=c('disagg_df', 'sb_df'))
}

loop_INE_muni_agrarian <- function(df_merge, 
                                   yrs = c('1989','1999','2009')
                                   ) {
  # loopes the AG_census years while it computes the aggregated Muni --> AR for the same years
  
  df <- create_AR_template()

  for (i in 1:nrow(df)) {
    
    for (j in yrs) {
      
      df[i, j] <- aggregate_INE_muni_agrarian(AR_id = i, df_merge = df_merge, year_sum = j)
    }
  }
  return(df)
}



compute_INE_muni_agrarian <- function(INE_param, main_param, param, folder) {
  # INE_param is either Areas or Animals
  # main_param is the main cat (e.g., Bovines, Cereals)
  # param is the subcat (e.g., Dairy_cows, Wheat)
  
  # specificy cases w/ irrigated and rainfed data
  if (missing(folder)==FALSE) {

    if (param == 'Maize' | param == 'Potato') {
     
       rainfed <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Agrarian', subfolder = INE_param, subfolderX2 = main_param, pattern = paste0('Rainfed_', tolower(param)))
      irrig <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Agrarian', subfolder = INE_param, subfolderX2 = main_param, pattern = paste0('Irrigated_', tolower(param)))
      
      param_df <- rainfed + irrig
      param_df[, 1] <- seq(1,7)
      rm(list=c('rainfed','irrig'))
    } 
    else {
      param_df <- get_activity_data(module = 'Nutrients', folder = folder, subfolder = INE_param, subfolderX2 = main_param, pattern = param)
      param_df <- loop_INE_muni_agrarian(param_df)
      names(param_df) <- c('id','X1989','X1999','X2009')
    }
  }
  else {
    folder <- 'Raw_data_Municipality'
    
    # call the param file for a given animal or crop acreage
    param_df <- get_activity_data(module = 'Nutrients', folder = folder, subfolder = INE_param, subfolderX2 = main_param, pattern = param)
    # calculate AR for the AG_census years
    param_df <- loop_INE_muni_agrarian(param_df)
    names(param_df) <- c('id','X1989','X1999','X2009')
  }
  return(param_df)
}



##  INTERPOLATION FUNCTION  --------------------------------------------------------------------------------------


linear_interpolation <- function(muni_df,
                                 xout = c(1987,1988,seq(1990,1998), seq(2000,2008), seq(2010,2017)),
                                 yrs = as.numeric(seq(1987,2017))
                                 ) {
  # general function to linearly interpolating MUNI_ID
  # muni_df must already have (i) all the years as columns; (ii) AG_CENSUS_YRS collated to the data.frame
  
  for (i in 1:nrow(muni_df)) {
    
    new_df <- approx(x= yrs, y = muni_df[i,], 
                     xout = xout, rule = 2)
    
    inter_years <- new_df[[1]]
    inter_values <- round(new_df[[2]], 0)
    
    ctr <- 0
    for (j in inter_years) {
      ctr <- ctr + 1
      muni_df[i, as.character(j)] <- inter_values[ctr]
    }
  }
  return(muni_df)
  rm(list=c('new_df','inter_years','inter_values','ctr'))
}
  



##  CORRECT MUNICIPALITY DATA BASED ON AR DATA  ------------------------------------------------------------------


correct_crop_animal_exceptions <- function() {
  
  crop_exceptions <- data.frame(Cereals = c('Maize'),
                   Pulses = c('Other_pulses'),
                   Fresh_fruits = c('other_fresh'),
                   Industry_crops = c('Other_industry'),
                   Potato = c('Potato'))
  
  animal_exceptions <- data.frame(Pigs = c('Sows_50'),
                                  Poultry = c('Rep_laying_hens'))
}


general_data_correction_function <- function(INE_param, main_param, param) {
  # general function to correct AG_census_muni data absed on annual AR_data
  
  AR <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Agrarian', subfolder = INE_param, subfolderX2 = main_param , pattern = param)
  AR <- AR[, c('id','X1989','X1999','X2009')]
  
  # 1 - sum AG_census_municipality to the AR level
  AR_census <- compute_INE_muni_agrarian(INE_param, main_param, param)
  
  # 2 - AG_census / AR
  adj_data <- AR_census
  adj_data <- AR / AR_census
  adj_data[, 'id'] <- seq(1,7)
  names(adj_data)[1] <- 'agrarian_region_id'
  adj_data <- data_cleaning(adj_data)
  
  # 3 - correct AG_census_muni based on #2
  muni_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', subfolder = INE_param, subfolderX2 = main_param, pattern = param)
  disagg_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Spatial_disaggregation')
  disagg_df <- plyr::join(disagg_df, adj_data)
  
  print('Correcting data --------')
  yrs <- c('X1989','X1999','X2009')
  muni_df[, yrs] <- sapply(yrs, function(x) round(disagg_df[, x] * muni_df[, x], 0))
  
  return(muni_df)
  rm(list=c('AR','AR_census','adj_data','muni_df','disagg_df','yrs'))
}


correct_irrigated_rainfed_crops_INE_muni <- function(INE_param = 'Areas',
                                            main_param = 'Cereals',
                                            param = 'Maize') {
  # corrects crops where data outside AG_census yrs is disaggregated into Rainfed_ and Irrigated
  # corrects AG_census municipality for the total acreage of potato and maize
  

  # 1 - sum irrigated and rainfed maize/potato
  rainfed_maize <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Agrarian', subfolder = 'Areas', subfolderX2 = main_param , pattern = paste0('Rainfed_', tolower(param)))
  irrig_maize <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Agrarian', subfolder = 'Areas', subfolderX2 = main_param , pattern = paste0('Irrigated_', tolower(param)))
  AR_maize <- irrig_maize + rainfed_maize
  AR_maize[, 'id'] <- seq(1,7)
  AR_maize <- AR_maize[, c('id','X1989','X1999','X2009')]
  
  # 2 - sum AG_census_muni maize/potato
  AR_census_maize <- compute_INE_muni_agrarian(INE_param, main_param, param)

  # 3 - Ag_census_muni / AR_maize /potato
  adj_maize <- AR_census_maize
  adj_maize <- AR_maize / AR_census_maize
  adj_maize[, 'id'] <- seq(1,7)
  names(adj_maize)[1] <- 'agrarian_region_id'
  adj_maize <- data_cleaning(adj_maize)
  
  # 4 - correct Ag_census_muni maize/potato based on #3 
  muni_maize <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', subfolder = INE_param, subfolderX2 = main_param, pattern = param)
  disagg_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Spatial_disaggregation')
  disagg_df <- plyr::join(disagg_df, adj_maize)

  yrs <- c('X1989','X1999','X2009')
  muni_maize[, yrs] <- sapply(yrs, function(x) round(disagg_df[, x] * muni_maize[, x], 1))
  
  return(muni_maize)
  rm(list=c('rainfed_maize','irrig_maize', 'adj_maize','muni_maize','disagg_df','yrs'))
}



compute_disaggregated_irrigated_rainfed_crops_INE_muni <- function(management,
                                                                   INE_param = 'Areas',
                                                                   main_param = 'Cereals',
                                                                   param = 'Maize'
                                                                   ) {
  # management is either Rainfed or Irrigated
  # while correct_irrigated_rainfed_crops_INE_muni corrects the municipality data for the AG_census based on the sum of rainfed and irrigated in the other years
  # this function disaggregates the new_muni into its rainfed and irrigated fraction  
  
  new_muni <- correct_irrigated_rainfed_crops_INE_muni(INE_param, main_param, param)
  AR_new_muni <-loop_INE_muni_agrarian(new_muni)
  names(AR_new_muni) <- c('id','X1989','X1999','X2009')
  
  manag_maize <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Agrarian', subfolder = 'Areas', subfolderX2 = main_param , pattern = paste0(management,'_', tolower(param)))
  manag_maize <- manag_maize[, c('id','X1989','X1999','X2009')]
  
  # calculate FRAC of a given management practice (rainfed, irrigated) at the AR
  FRAC_AR_manag_maize <- manag_maize / AR_new_muni
  FRAC_AR_manag_maize[, 'id'] <- seq(1,7)
  names(FRAC_AR_manag_maize) <- c('agrarian_region_id','X1989','X1999','X2009')
  FRAC_AR_manag_maize <- data_cleaning(FRAC_AR_manag_maize)
  
  # disaggregate to the municipality
  disagg_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Spatial_disaggregation')
  disagg_df <- plyr::join(disagg_df, FRAC_AR_manag_maize)
  
  yrs <- c('X1989','X1999','X2009')
  new_muni[, yrs] <- sapply(yrs, function(x) round(disagg_df[, x] * new_muni[, x], 1))
  
  return(new_muni)
  rm(list=c('new_muni','AR_new_muni','manag_maize','FRAC_AR_manag_maize','disagg_df'))
}


correct_other_fresh_fruits <- function(INE_param = 'Areas',
                                       main_param = 'Fresh_fruits'
                                       ) {
  
  plum <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Agrarian', subfolder = INE_param, subfolderX2 = main_param , pattern = 'Plum')
  fig <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Agrarian', subfolder = INE_param, subfolderX2 = main_param , pattern = 'Fig')
  AR_other <- fig + plum
  AR_other[, 'id'] <- seq(1,7)
  AR_other <- AR_other[, c('id','X1989','X1999','X2009')]
  
  # 2 - sum AG_census_muni other fresh fruits
  AR_census_other_fresh <- compute_INE_muni_agrarian(INE_param, main_param, param = 'other_fresh')
  
  # 3 - Ag_census_muni / AR
  adj_other <- AR_census_other_fresh
  adj_other <- AR_other / AR_census_other_fresh
  adj_other[, 'id'] <- seq(1,7)
  names(adj_other)[1] <- 'agrarian_region_id'
  adj_other <- data_cleaning(adj_other)
  
  # 4 - correct Ag_census_muni  based on #3
  muni_other <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', subfolder = INE_param, subfolderX2 = main_param, pattern = 'other_fresh')
  disagg_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Spatial_disaggregation')
  disagg_df <- plyr::join(disagg_df, adj_other)
  
  yrs <- c('X1989','X1999','X2009')
  muni_other[, yrs] <- sapply(yrs, function(x) round(disagg_df[, x] * muni_other[, x], 1))
  
  return(muni_other)
  rm(list=c('plum','fig','AR_other','AR_other','AR_census_other_fresh','adj_other','disagg_df','yrs'))
}



correct_other_pulses <- function(INE_param = 'Areas',
                                 main_param = 'Pulses',
                                 param = 'Other_dried_pulses'
                                 ) {
  # other pulses were linearly interpolated from the years outside of the AG_census
  # interpolation followed the rule where min and max estimated values can't be lower or higher than the min and max registered in the
  # AG_census year
  
  template <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  
  df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  yrs <- seq(1987,2017)
  df[, as.character(yrs)] <- NA
  
  crop_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', subfolder = 'Areas', subfolderX2 = main_param , pattern = param)
  names(crop_df) <- c('Muni_ID','ID','Muni','1989','1999','2009')
  
  new_yrs <- c(1989,1999,2009)
  df[, as.character(new_yrs)] <- sapply(new_yrs, function(x) round(df[, as.character(x)] <- crop_df[, as.character(x)], 1))
 # dff <- df[, -c(1,2,3)] 
  df <- df[, c('Muni_ID','Muni','ID','1989','1999','2009')]
  names(df) <- c('Muni_ID','Muni','ID','X1989','X1999','X2009')
  # linear interpolation 
 # xout <- c(1987,1988,seq(1990,1998), seq(2000,2008), seq(2010,2017))
 # df <- linear_interpolation(muni_df = dff)
 # template <- cbind(template, df)
  
  return(df)
 # rm(list=c('df','yrs','new_yrs','crop_df','xout','dff','inter_years','inter_values','ctr','template'))
}


correct_tomatoes <- function(INE_param = 'Areas',
                             main_param = 'Industry_crops',
                             param = 'Tomato') {

  # 1 - sum AG_census other_industry at the AR level
  # 2- correct AG_census based on AR_tomato for 1989,1999,2009
  
  AR_tomato <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Agrarian', subfolder = 'Areas', subfolderX2 = 'Industry_crops', pattern = 'Tomato')
  AR_tomato <- AR_tomato[, c('id','X1989','X1999','X2009')]
  AG_muni_other_industry <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', subfolder = 'Areas', subfolderX2 = 'Industry_crops', pattern = 'Other_industry')
  
  # 1 - sum AG_census other_industry at the AR level
  AR_AG_muni_other <- compute_INE_muni_agrarian('Areas','Industry_crops','Other_industry')
  
  # 2 - calculate FRAC_AR
  FRAC_AR <- AR_tomato / AR_AG_muni_other
  FRAC_AR[, 'id'] <- seq(1,7)
  names(FRAC_AR)[1] <- 'agrarian_region_id'
  FRAC_AR <- data_cleaning(FRAC_AR)
  
  # 3 - correct AG_census_muni based on #2
  disagg_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Spatial_disaggregation')
  disagg_df <- plyr::join(disagg_df, FRAC_AR)
  
  print('Correcting data --------')
  yrs <- c('X1989','X1999','X2009')
  AG_muni_other_industry[, yrs] <- sapply(yrs, function(x) round(disagg_df[, x] * AG_muni_other_industry[, x], 1))
  
  return(AG_muni_other_industry)
  rm(list=c('AR_tomato','AG_muni_other_industry','AR_AG_muni_other','FRAC_AR','disagg_df','yrs'))
}




correct_animals_params <- function(INE_param = 'Animals', 
                                   main_param = 'Pigs',
                                   muni_param = 'Sows_50', 
                                   AR_param1 = 'Non_pregnant_sows',
                                   AR_param2 = 'Pregnant_sows'
                                   ) {
  # corrects AG_muni params for Sows and Rep_laying hens based on AR data for the disaggregated reproductive and non-reproductive species
  # muni_param is the same as the commonly used param, only to better specify the differences between AR and muni params here used
  
  # 1 - sum AG_census muni_param
  # 2 - correct AR_AG_census based on the sum of AR_param1 and AR_param2 (AR_params)
  # 3 - calculate FRAC between AR_params and AR_AG_census
  # 4 - Calculate new_AG_census = AG_census * FRAC 
  
  # 1 - sum AG_census muni_param
  AG_muni <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', subfolder = INE_param, subfolderX2 = main_param, pattern = muni_param)
  AR_AG_muni <- loop_INE_muni_agrarian(df_merge = AG_muni)
  names(AR_AG_muni) <- c('id','X1989','X1999','X2009')
  
  # 2 - correct AR_AG_census based on the sum of AR_param1 and AR_param2 (AR_params)
  AR1 <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Agrarian', subfolder = INE_param, subfolderX2 = main_param, pattern = AR_param1)
  AR2 <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Agrarian', subfolder = INE_param, subfolderX2 = main_param, pattern = AR_param2)
  AR1 <- AR1[, c('id','X1989','X1999','X2009')]
  AR2 <- AR2[, c('id','X1989','X1999','X2009')]
  
  AR <- AR1 + AR2
  AR[, 1] <- seq(1,7)
  
  # 3 - calculate FRAC between AR_params and AR_AG_census
  FRAC_AR <- AR / AR_AG_muni
  FRAC_AR[, 1] <- seq(1,7)
  names(FRAC_AR)[1] <- 'agrarian_region_id'
  FRAC_AR <- data_cleaning(FRAC_AR)
  
  # 4 - spatial disaggregation 
  disagg_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Spatial_disaggregation')
  disagg_df <- plyr::join(disagg_df, FRAC_AR)
  
  # 5 - calculate new_AG_muni = AG_muni * FRAC_AR
  print('Correcting data --------')
  yrs <- c('X1989','X1999','X2009')
  AG_muni[, yrs] <- sapply(yrs, function(x) round(disagg_df[, x] * AG_muni[, x],0))
  
  return(AG_muni)
  rm(list=c('AR_AG_muni','AR1','AR2','AR','FRAC_AR','disagg_df'))
}


return_reproduction_animal_params <-  function(INE_param = 'Animals',
                                            main_param = 'Pigs',
                                            param = 'Sows_50') {
  
  # returns a list with the different AR_params
  # index 1 = non pregnant animals
  # index 2 = pregnant animals
  
  if (param == 'Sows_50') {
    store <- list(AR1='Non_pregnant_sows', AR2='Pregnant_sows')
  } 
  else {
    store <- list(AR1='Laying_hens', AR2='Rep_hens')
  }
  return(store)  
}


compute_reproduction_disaggregated_animal_params <- function(rep_animal = TRUE,
                                                             INE_param = 'Animals',
                                                             main_param = 'Pigs',
                                                             param = 'Sows_50') {
  
  # based on AG_census general animal param, specify categories from AR to disaggregate

  AR1 <- return_reproduction_animal_params(INE_params, main_param, param)[[1]]
  AR2 <- return_reproduction_animal_params(INE_params, main_param, param)[[2]]
  
  # 1 - get newly corrected AG_muni and calculated AR
  AG_new_muni <- correct_animals_params(INE_param, main_param, param, AR1, AR2)
  AR_new_muni <- loop_INE_muni_agrarian(AG_new_muni)
  

  # 2 - call AR param
  ifelse(rep_animal == TRUE, AR <- AR2, AR <- AR1)
  AR <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Agrarian', subfolder = INE_param, subfolderX2 = main_param, pattern = AR)
  AR <- AR[, c('id','X1989','X1999','X2009')]
  
  # 3 - calculate FRAC between AR and AR_new_muni
  FRAC_AR <- AR / AR_new_muni
  FRAC_AR[, 1] <- seq(1,7)
  names(FRAC_AR)[1] <- 'agrarian_region_id'
  FRAC_AR <- data_cleaning(FRAC_AR)
  
  # 4 - spatial disaggregation 
  disagg_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Spatial_disaggregation')
  disagg_df <- plyr::join(disagg_df, FRAC_AR)
  
  # 5 - calculate new_AG_muni = AG_muni * FRAC_AR
  print('Correcting data --------')
  yrs <- c('X1989','X1999','X2009')
  AG_new_muni[, yrs] <- sapply(yrs, function(x) round(disagg_df[, x] * AG_new_muni[, x], 0))
  
  
  # 6 - give disaggregated names and export
  f_param <- ifelse(rep_animal == TRUE, AR2, AR1)
  export_file(module = 'Nutrients', 
              file = AG_new_muni, 
              filename = f_param, 
              folder = 'Activity_data', 
              subfolder = 'Correct_data_Municipality', 
              subfolderX2 = INE_param, 
              subfolderX3 = main_param)
  
  rm(list=c('AR1','AR2','AG_new_muni','AR_new_muni','FRAC_AR','disagg_df','yrs'))
}

call_specific_param_correction <- function(INE_param='',main_param='', param, management='', rep_animal = TRUE) {
  # when param is spcieified, this function calls the required correct_XXX 
  
  # crop specifications ---------------------------------------------------------------------------------------------------------------------------------
  if (param == 'Maize' | param == 'Potato') {
    
    df <- compute_disaggregated_irrigated_rainfed_crops_INE_muni(management = management, INE_param = 'Areas', main_param = main_param, param = param)
  }
  else if (param == 'other_fresh') {
    
    df <- correct_other_fresh_fruits()
  }
  else if (param == 'Other_dried_pulses') {
    
    df <- correct_other_pulses()
  }
  else if (param == 'Tomato') {
    
    df <- correct_tomatoes()
  }
  
  # animal specifications ---------------------------------------------------------------------------------------------------------------------------------
  else if (param == 'Sows_50' | param == 'Rep_laying_hens') {
    
    df <- compute_reproduction_disaggregated_animal_params(rep_animal, INE_param, main_param, param)
  }
  
  else {
    df <- general_data_correction_function(INE_param, main_param, param)
  }
  return(df)
}


subset_standard_params <- function(INE_param) {
  
  standard_params <- get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', pattern = 'Standard_params')
  
  if (INE_param == 'Animals') {
    
    standard_params <- standard_params[, 3:4]
    standard_params <- standard_params[-which(is.na(standard_params)==TRUE), ]
  }
  return(standard_params)
}

compute_corrected_INE_param_AG_census <- function(INE_param) {
  # loops around each main_param and param of the specified INE_param (Animals, Areas)
  # writes each file to Correct_data_Municipality
  # downscales and corrects each param based on the AR aggregation
  # ONLY FOR THE AG_CENSUS_YEARS (1989,1999,2009)
  
  standard_params <- subset_standard_params(INE_param)
  
  param_col <- ifelse(INE_param == 'Areas', 'Crop', 'Animals')
  main_param_col <- ifelse(INE_param == 'Areas', 'Main_crop', 'Main_animals')

  
  for (i in 1:nrow(standard_params)) {
    
    main_param <- standard_params[i, main_param_col]
    param <- standard_params[i, param_col]
    
    print(paste0('Correcting and downscaling ', param))
    
    # specify irrigated and rainfed Maize and Potato
    if (param == 'Maize' | main_param == 'Potato') {
      
      manag <- c('Rainfed','Irrigated')
      
      for (j in manag) {
        
        df <- call_specific_param_correction(INE_param = INE_param, main_param = main_param, param = param, management = j)
        f_name <- paste0(j,'_', tolower(param))
        export_file(module = 'Nutrients', file = df, filename = f_name, folder = 'Activity_data', subfolder = 'Correct_data_Municipality', subfolderX2 = INE_param, subfolderX3 = main_param)
      }
    }
    # specific sows and hens
    else if (param == 'Sows_50' | param == 'Rep_laying_hens') {
      
      sapply(c(TRUE, FALSE), function(x) df <- call_specific_param_correction(INE_param = INE_param, main_param = main_param, param = param, rep_animal = x))
    }
    
    else {
      df <- call_specific_param_correction(INE_param = INE_param, main_param = main_param, param = param)
      export_file(module = 'Nutrients', file = df, filename = param, folder = 'Activity_data', subfolder = 'Correct_data_Municipality', subfolderX2 = INE_param, subfolderX3 = main_param)
    }
  }
  rm(list=c('standard_params','param_col','main_param_col','main_param','param','manag','df','f_name'))
}


compute_correct_ALL_params <- function() {
  
  INE_params <- c('Animals','Areas')
  sapply(INE_params, compute_corrected_INE_param_AG_census)
}


## INTERPOLATED NON-CENSUS YEARS  -----------------------------------------------------------------------------

# 0 - Correct and standardize params

# 1 - Establish time rules based on the availability of census years
# AG_census years can influence 3 years for both the right and left time (e.g., 1989 --> 1987 - 1992)
# For in-between years (e.g., 1993-1995) the average between in-between census is computed and used to smooth the data
# for the years after 2009, it is assumed that this census year is the new_muni

# 2 - Aggregate at the agrarian region level the annual data.frame created in step #1 (AG_AR)

# 3 - Call AR data from Statistics Portugal for all years (AR)

# 4 - Calculate the FRAC_AR between #2 and #3
# FRAC_AR = AG_AR / AR

# 5 - Calculate the interpolated and corrected new_muni
# new_muni_T <- muni_AG_census * FRAC_AR 



compute_AVG_AG_census <- function(df, period, AG_yr1, AG_yr2) {
  # function to average the parameters of the AG_census yr
  
  for (i in 1:nrow(df)) {
    
    df[i, period] <- round(mean(c(df[i, AG_yr1], df[i, AG_yr2])), 1)
  }
  return(df[, period])
}


average_AG_census_interpolation_period <- function(INE_param, main_param, param,
                                                   period_1 = as.character(seq(1987,1992)), #AG1
                                                   period_2 = as.character(seq(1993,1995)), #AVG1
                                                   period_3 = as.character(seq(1996,2002)), #AG2
                                                   period_4 = as.character(seq(2003,2005)), #AVG2
                                                   period_5 = as.character(seq(2006, 2017)) #AG3
                                                   ) {
  # TO BE USED ONLY AFTER compute_corrected_INE_param_AG_census
  # selects the appropriate year for AG_census (AG1,AG2,AG3) or averages the years in-between AG_census (AVG1,AVG2)
  # populates a data.frame with the different new_muni templates according to the year 
  
  df <-  get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = INE_param, subfolderX2 = main_param, pattern = param)
  store <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  
  yrs <- as.character(seq(1987,2017))
  
  for (i in yrs) {
    
    if (i %in% period_1) {
      store[, paste0('X',i)] <- df[, 'X1989'] 
    } 
    else if (i %in% period_2) {
      store[, paste0('X',i)]<- compute_AVG_AG_census(df, 'AVG1','X1989','X1999')
    } 
    else if (i %in% period_3) {
      store[, paste0('X',i)] <- df[, 'X1999'] 
    }
    else if (i %in% period_4) {
      store[, paste0('X',i)] <- compute_AVG_AG_census(df, 'AVG2','X1999','X2009')
    }
    else {
      store[, paste0('X',i)] <- df[,'X2009']
    }
  }
  return(store)
  rm(list='df')
}


compute_annual_interpolated_param_func <- function(INE_param, main_param, param) {
  # calculates corrected new_muni for a given param 
  # time rules: expressed in average_AG_census_interpolation_period()
  
  
  # 1 - compile new_muni data based on the time-rules established in average_AG_census_interpolation_period
  AG_muni <- average_AG_census_interpolation_period(INE_param, main_param, param)
  # 2 - call the AR data for a given param 
  AR <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Agrarian', subfolder = INE_param, subfolderX2 = main_param, pattern = param)
  # 3 - compute the new_muni at the AR level
  AG_AR <- loop_INE_muni_agrarian(df_merge = AG_muni, yrs = as.character(seq(1987,2017)))
  
  calc_yrs <- paste0('X',seq(1987,2017))
  
  names(AG_AR) <- c('id', paste0('X',seq(1987,2017)))
  
  for (i in calc_yrs) {
    
    # 1 - calculate FRAC of AR_yr / AR_AG_census
    FRAC_AR <- data.frame(agrarian_region_id = seq(1,7))
    FRAC_AR[, 'FRAC_AR'] <- AR[, i] / AG_AR[, i]
    FRAC_AR <- data_cleaning(FRAC_AR)
    # 2 - call spatial disaggregation and create a template calculation
    disagg_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Spatial_disaggregation')
    disagg_df <- plyr::join(disagg_df, FRAC_AR)
    
    # 4 - calculate new_muni
    AG_muni[, i] <- round(AG_muni[, i] * disagg_df[, 'FRAC_AR'], 1)
  }
  return(AG_muni)
  rm(list=c('AR','AG_AR','FRAC_AR','disagg_df'))
}



linear_interpolation_other_params_func <- function(INE_param, main_param, param) {
  # support function to linear interpolate other_dried_pulses and other_fresh
  # this is used since there are no data outside the AG_census years
  
  template <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  
  param_interpol <-  get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', subfolder = INE_param, subfolderX2 = main_param , pattern = param)
  names(param_interpol) <- gsub('X','',names(param_interpol))
  
  # create empty dataframe where calculations will proceed
  yrs <- seq(1987,2017)
  df[, as.character(yrs)] <- NA
  
  # collated AG_census dataa to the empty data.frame
  new_yrs <- c(1989,1999,2009)
  df[, as.character(new_yrs)] <- sapply(new_yrs, function(x) round(df[, as.character(x)] <- param_interpol[, as.character(x)], 1))
  df <- df[, -c(1,2,3)]
  
  # interpolate the data to the other years
  df <- linear_interpolation(muni_df = df)
  template <- cbind(template, df)

  return(template)
  rm(list=c('param_interpol','yrs','df','new_yrs','xout'))
}

linear_interpolation_reproduction_animals_func <- function(INE_param, main_param, param) {
  # interpolating function for Sows and Hens
  
  ifelse(param == 'Sows_50', rep_animals <- c('Pregnant_sows', 'Non_pregnant_sows'), rep_animals <- c('Laying_hens', 'Rep_hens'))
    
    for (i in rep_animals) {
      
      param_interpol <- compute_annual_interpolated_param_func(INE_param, main_param, i)
      export_file(module = 'Nutrients', 
                  file = param_interpol, 
                  filename = i, 
                  folder = 'Activity_data', 
                  subfolder = 'Correct_data_Municipality', 
                  subfolderX2 = INE_param, 
                  subfolderX3 = main_param)
    }
}


loop_annual_interpolated_param <- function(INE_param) {
  # computes the interpolated AG_muni for the years in-between AG_census for each param of a given INE_param
  # calls specific functions for some crops (Maize, Potato, other_dried_pulses, other_fresh) and animals (pregnant animals)
  
  
  standard_params <- get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', pattern = 'Standard_params')
  
  param_col <- ifelse(INE_param == 'Areas', 'Crop', 'Animals')
  main_param_col <- ifelse(INE_param == 'Areas', 'Main_crop', 'Main_animals')
  
  for (i in 1:nrow(standard_params)) {
    
    main_param <- standard_params[i, main_param_col]
    param <- standard_params[i, param_col]
    
    print(paste0('================= Downscaling and interpolating in-between AG_census municipality data for ', param))
    
    if (param == 'Maize' | param == 'Potato') {
      
      manag <- c('Rainfed','Irrigated')
      
      for (j in manag) {
        f_param <- paste0(j, '_', tolower(param))
        param_interpol <- compute_annual_interpolated_param_func(INE_param, main_param, f_param)
        export_file(module = 'Nutrients', file = param_interpol, filename = f_param, folder = 'Activity_data', subfolder = 'Correct_data_Municipality', subfolderX2 = INE_param, subfolderX3 = main_param)
      }
    }
    
    # other_dried_pulses and other_fresh must be linealry interpolated between 1989, 1999 and 2009 as no AR data is available
    else if (param == 'Other_dried_pulses' | param == 'other_fresh') {
      
      param_interpol <- linear_interpolation_other_params_func(INE_param, main_param, param)
      export_file(module = 'Nutrients', file = param_interpol, filename = param, folder = 'Activity_data', subfolder = 'Correct_data_Municipality', subfolderX2 = INE_param, subfolderX3 = main_param)
    }
    
    else if (param == 'Sows_50' | param == 'Rep_laying_hens') {
      
      linear_interpolation_reproduction_animals_func(INE_param, main_param, param)
    }
    
    else {
      
      param_interpol <- compute_annual_interpolated_param_func(INE_param, main_param, param)
      export_file(module = 'Nutrients', file = param_interpol, filename = param, folder = 'Activity_data', subfolder = 'Correct_data_Municipality', subfolderX2 = INE_param, subfolderX3 = main_param)
    }
  }
  rm(list=c('standard_params','param_col','main_param_col'))
}


round0_animal_population <- function() {
  
  standard_params <- get_activity_data(module = 'Nutrients', folder = 'General_params', pattern = 'Params_list')
  
  param_col <- 'Animals'
  main_param_col <- 'Main_animals'
  
  for (i in 1:nrow(standard_params)) {
    
    main_param <- standard_params[i, main_param_col]
    param <- standard_params[i, param_col]
    
    df <- get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Animals', subfolderX2 = main_param, pattern = param)
    yrs <- paste0('X', seq(1987,2017))
    df[, yrs] <- sapply(yrs, function(x) df[, x] <- round(df[,x], 0))
    export_file(module = 'Nutrients', file = df, filename = param, folder = 'Activity_data', subfolder = 'Correct_data_Municipality', subfolderX2 = 'Animals', subfolderX3 = main_param)
  }
}
