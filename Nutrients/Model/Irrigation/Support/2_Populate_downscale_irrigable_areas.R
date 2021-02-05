source('./Nutrients/Model/INE_DB/Population_INE_Municipality.R')
source('./Nutrients/Model/INE_DB/Populate_INE_AgrarianRegion.R')
source('./Nutrients/Model/INE_DB/Standardize_INE_params.R')
source('./Main/Data_operations.R')


library(doParallel)


# get irrigable areas per municipality/AR for 1989, 1999 and 2009 --------------------------------
# then interpolate to the other years at the agrarian region -------------------------------------

get_irrigable_municipality_areas_AG_census = function(INE_param_id = '0004391',
                                                      year = c('1989','1999','2009')) {
  
  cl <- registerDoParallel(cores=4)
  getDoParWorkers()
  
  df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  
  for (j in year) {
    
    js_year <- paste0('S7A', j)
    store <- foreach(i=1:nrow(df), 
                     .export = c('update_get_INE_data', 'get_INE_data', 'get_activity_data'),
                     .combine = 'rbind',
                     .packages = 'jsonlite') %dopar% {
                       
                       INE_value = update_get_INE_data(INE_param_id = INE_param_id, year = js_year, muni_id = as.character(df[i, 'ID']), var_id = 'T')[[1]][[1]]
                       
                       data.frame(val = INE_value,
                                  i = i)
                     }
    df[, j] <- store[, 'val']
  }
  stopImplicitCluster()
  
  # correct awkward values
  df[, seq(4,6)] = sapply(seq(4,6), function(x) as.numeric(df[,x]))
  df[which(is.na(df)==TRUE, arr.ind = T), 4:6] = 0
  names(df)[4:6] = c('X1989','X1999','X2009')
  export_file(module = 'Nutrients', 
              folder = 'Activity_data', 
              subfolder = 'Correct_data_Municipality', 
              subfolderX2 = 'Irrigation', 
              subfolderX3 = 'Irrigable_areas',
              subfolderX4 = 'Municipality',
              filename = 'Total', 
              file = df)
}



get_irrigable_AR = function(INE_param_id = '0003008',
                            year = c('1989','1993','1995','1997','1999','2005','2007','2009','2013','2016'),
                            muni_id = as.character(seq(11,17)),
                            var_id = 'T') {
  
  df <- data.frame()
  print(muni_id)
  for (i in seq_along(muni_id)) {
    
    print(paste0('Agrarian region: ', i))
    df[i, 'id'] <- i
    
    for (j in year) {
      
      print(paste0('Year: ', j))
      js_year <- paste0('S7A', j)
      df[i, as.character(j)] <- get_INE_data(INE_param_id, js_year, muni_id[i], var_id, 'Def')
    }
  }
  export_file(module = 'Nutrients', 
              folder = 'Activity_data', 
              subfolder = 'Correct_data_Municipality', 
              subfolderX2 = 'Irrigation', 
              subfolderX3 = 'Irrigable_areas',
              subfolderX4 = 'Agrarian_region',
              filename = 'Total', 
              file = df)
}


adjust_irrigable_AR = function(AR_df) {
  # corrects AR data according to municipality sums for AG_census years
  
  muni_df = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Irrigation', subfolderX2 = 'Irrigable_areas', subfolderX3 = 'Municipality', pattern = 'Total')
  
  AG_census_tot = colSums(muni_df[, 4:6])
  AR_census_tot = colSums(AR_df[, c('X1989','X1999','X2009')])
  
  yrs = c('X1989','X1999','X2009')
  
  ctr = 0
  for (i in yrs) {
    ctr = ctr + 1
    
    AR_df[, i] = round(AR_df[, i] * AG_census_tot[ctr] /AR_census_tot[ctr], 0)
    
  }
  
  return(AR_df)
}



linearly_interpolate_AR = function() {
  # updates data collated using get_irrigable_AR()
  # linearly interpolates from the remaining years
  # scale: Agrarian region
  
  AR_df = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Irrigation', subfolderX2 = 'Irrigable_areas', subfolderX3 = 'Agrarian_region', pattern = 'Total')
  AR_df = adjust_irrigable_AR(AR_df)
  names(AR_df) <- gsub('X','',names(AR_df))
  
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
  new_yrs <- names(AR_df)[-1]
  
  for (i in new_yrs) {
    df[, as.character(i)] <- AR_df[, as.character(i)]
  }
  df <- df[, -1]
  df = sapply(df, as.numeric)
  new_df = df[, new_yrs] # df with only years with data

  xout <- c(1987,1988,1990,1991,1992,1994,1996,1998,2000,2001,2002,2003, 2004,2006,2008,2010,2011,2012,2014,2015,2017)
  
  for (i in 1:nrow(new_df)) {
    
    AR_aei = data.frame(yrs = as.numeric(new_yrs),
                    aei = as.numeric(new_df[i,]))
    lm_model = lm(aei~yrs,AR_aei)
    pred_vals =  round( predict(lm_model, newdata =  data.frame(yrs =  xout)) , 0)
    pred_vals <- ifelse(pred_vals<0, 0, round(pred_vals, 0))
    
    df[i, as.character(xout)] = pred_vals
  }
  
  df = as.data.frame(df)
  
  
  df[, 'id'] <- seq(1,7)
  df <- df[, c('id', as.character(seq(1987,2017)))]
  names(df) = c('id', paste0('X',seq(1987,2017)))
  export_file(module = 'Nutrients', 
              folder = 'Activity_data', 
              subfolder = 'Correct_data_Municipality', 
              subfolderX2 = 'Irrigation', 
              subfolderX3 = 'Irrigable_areas',
              subfolderX4 = 'Agrarian_region',
              filename = 'Total', 
              file = df)
}


# downscales to the municipality scale the irrigable areas ---------------------------------

average_irrigable_AG_census_interpolation_period <- function(muni_df) {
  #' @note similar to compute_interpolate_non_AG_census_yrs
  #' @description linearly interpolates irrigable census AG_census for the whole period
  # TO BE USED ONLY AFTER compute_corrected_INE_param_AG_census
  
  # populates a data.frame with the different new_muni templates according to the year 
  yrs_in = c(1989,1999,2009)
  # yrs_out are the remaining one, 1987-2017
  muni_df = muni_df[, -c(1,2,3)]
  muni_df = apply_linearExtrapolation(file = muni_df, yrs_in = yrs_in)
  
  return(muni_df)
}


compute_irrigable_annual_interpolated_param_func <- function() {
  # calculates corrected new_muni for a given param 
  # time rules: expressed in average_AG_census_interpolation_period()
  # updates irrigable municipality areas
  
  # 1 - compile new_muni data based on the time-rules established in average_AG_census_interpolation_period
  muni_df = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Irrigation', subfolderX2 = 'Irrigable_areas', subfolderX3 = 'Municipality', pattern = 'Total')
  AG_muni = average_irrigable_AG_census_interpolation_period(muni_df)
  # 2 - call the AR data for a given param 
  AR = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Irrigation', subfolderX2 = 'Irrigable_areas', subfolderX3 = 'Agrarian_region', pattern = 'Total')
  # 3 - compute the new_muni at the AR level
  AG_AR =compute_temporal_sumIF_admin_df(admin = 'Agrarian_region', merge_df = AG_muni)
  AG_AR = AG_AR[order(AG_AR$Admin_id),]
  
  
  calc_yrs <- paste0('X',seq(1987,2017))
  
  names(AG_AR) <- c('id', paste0('X',seq(1987,2017)))
  
  for (i in calc_yrs) {
    
    # 1 - calculate FRAC of AR_yr / AR_AG_census
    FRAC_AR <- data.frame(agrarian_region_id = seq(1,7))
    FRAC_AR[, 'FRAC_AR'] <- AR[, i] / AG_AR[, i]
    FRAC_AR <- data_cleaning(FRAC_AR)
    # 2 - call spatial disaggregation and create a template calculation
    disagg_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Spatial_disaggregation')
    disagg_df <- plyr::join(disagg_df, FRAC_AR, 'agrarian_region_id')
    
    # 4 - calculate new_muni
    AG_muni[, i] <- round(AG_muni[, i] * disagg_df[, 'FRAC_AR'], 1)
  }
  
  export_file(module = 'Nutrients', 
              folder = 'Activity_data', 
              subfolder = 'Correct_data_Municipality', 
              subfolderX2 = 'Irrigation', 
              subfolderX3 = 'Irrigable_areas',
              subfolderX4 = 'Municipality',
              filename = 'Total', 
              file = AG_muni)
  rm(list=c('AR','AG_AR','FRAC_AR','disagg_df'))
}



