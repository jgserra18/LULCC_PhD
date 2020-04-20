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
  sb_df <- merge(sb_df, df_merge, 'ID')
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


correct_crop_exceptions <- function() {
  
  df <- data.frame(Cereals = c('Maize'),
                   Pulses = c('Other_pulses'),
                   Fresh_fruits = c('other_fresh'),
                   Industry_crops = c('Other_industry'),
                   Potato = c('Potato'))
}


general_data_correction_function <- function(INE_param, main_param, param) {
  # general function to correct AG_census_muni data absed on annual AR_data
  
  AR <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Agrarian', subfolder = INE_param, subfolderX2 = main_param , pattern = param)
  AR <- AR[, c('id','X1989','X1999','X2009')]
  
  # 1 - sum AG_census_municipality to the AR level
  AR_census <- compute_INE_muni_agrarian(INE_param, main_param, param)
  
  # 2 - AG_census / AR
  adj_data <- AR_census
  adj_data <- AR_census / AR
  adj_data[, 'id'] <- seq(1,7)
  names(adj_data)[1] <- 'agrarian_region_id'
  
  # 3 - correct AG_census_muni based on #2
  muni_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', subfolder = INE_param, subfolderX2 = main_param, pattern = param)
  disagg_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Spatial_disaggregation')
  disagg_df <- merge(disagg_df, adj_data, 'agrarian_region_id')
  
  print('Correcting data --------')
  yrs <- c('X1989','X1999','X2009')
  muni_df[, yrs] <- sapply(yrs, function(x) round(disagg_df[, x] * muni_df[, x], 0))
  
  export_file(module = 'Nutrients',
              file = muni_df, folder = 'Activity_data', 
              filename = param, subfolder = 'Correct_data_Municipality', subfolderX2 = INE_param, subfolderX3 = main_param)
 # rm(list=c('AR','AR_census','adj_data','muni_df','disagg_df','yrs','muni_df'))
}




correct_irrigated_rainfed_crops <- function(INE_param = 'Areas',
                                            main_param = 'Cereals',
                                            param = 'Maize') {
  # corrects crops where data outside AG_census yrs is disaggregated into Rainfed_ and Irrigated
  # Potato, Maize
  

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
  adj_maize <- AR_census_maize / AR_maize
  adj_maize[, 'id'] <- seq(1,7)
  names(adj_maize)[1] <- 'agrarian_region_id'

  # 4 - correct Ag_census_muni maize/potato based on #3 
  muni_maize <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', subfolder = INE_param, subfolderX2 = main_param, pattern = param)
  disagg_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Spatial_disaggregation')
  disagg_df <- merge(disagg_df, adj_maize, 'agrarian_region_id')

  yrs <- c('X1989','X1999','X2009')
  muni_maize[, yrs] <- sapply(yrs, function(x) round(disagg_df[, x] * muni_maize[, x], 1))
  
  return(muni_maize)
  export_file(module = 'Nutrients',
              file = muni_maize, folder = 'Activity_data', 
              filename = param, subfolder = 'Correct_data_Municipality', subfolderX2 = 'Areas', subfolderX3 = main_param)
  rm(list=c('rainfed_maize','irrig_maize', 'adj_maize','muni_maize','disagg_df','yrs'))
}


correct_other_fresh_fruits <- function(INE_param = 'Areas',
                                       main_param = 'Fresh_fruits'
                                       ) {
  
  plum <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Agrarian', subfolder = INE_param, subfolderX2 = main_param , pattern = 'Plum')
  fig <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Agrarian', subfolder = INE_param, subfolderX2 = main_param , pattern = 'Fig')
  AR_other <- fig + plum
  AR_maize[, 'id'] <- seq(1,7)
  AR_other <- AR_other[, c('id','X1989','X1999','X2009')]
  
  # 2 - sum AG_census_muni other fresh fruits
  AR_census_other_fresh <- compute_INE_muni_agrarian(INE_param, main_param, param = 'other_fresh')
  
  # 3 - Ag_census_muni / AR
  adj_other <- AR_census_other_fresh
  adj_other <- AR_census_other_fresh / AR_other
  adj_other[, 'id'] <- seq(1,7)
  names(adj_other)[1] <- 'agrarian_region_id'
  
  # 4 - correct Ag_census_muni  based on #3
  muni_other <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', subfolder = INE_param, subfolderX2 = main_param, pattern = 'other_fresh')
  disagg_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Spatial_disaggregation')
  disagg_df <- merge(disagg_df, adj_other, 'agrarian_region_id')
  
  yrs <- c('X1989','X1999','X2009')
  muni_other[, yrs] <- sapply(yrs, function(x) round(disagg_df[, x] * muni_other[, x], 1))
  
  export_file(module = 'Nutrients',
              file = muni_other, folder = 'Activity_data', 
              filename = 'other_fresh', subfolder = 'Correct_data_Municipality', subfolderX2 = 'Areas', subfolderX3 = main_param)
}



correct_other_pulses <- function(INE_param = 'Areas',
                                 main_param = 'Pulses',
                                 param = 'Other_dried_pulses') {
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
  df[, as.character(new_yrs)] <- sapply(new_yrs, function(x) df[, as.character(x)] <- crop_df[, as.character(x)])
  dff <- df[, -c(1,2,3)] 
  
  
  # linear interpolation 
  xout <- c(1987,1988,seq(1990,1998), seq(2000,2008), seq(2010,2017))
  df <- linear_interpolation(muni_df = dff)
  template <- cbind(template, df)
  
  return(template)
  export_file(module = 'Nutrients',
              file = template, folder = 'Activity_data', 
              filename = param, subfolder = 'Correct_data_Municipality', subfolderX2 = 'Areas', subfolderX3 = main_param)
 rm(list=c('df','yrs','new_yrs','crop_df','xout','dff','inter_years','inter_values','ctr','template'))
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
  
  # 3 - correct AG_census_muni based on #2
  disagg_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Spatial_disaggregation')
  disagg_df <- merge(disagg_df, FRAC_AR, 'agrarian_region_id')
  
  print('Correcting data --------')
  yrs <- c('X1989','X1999','X2009')
  AG_muni_other_industry[, yrs] <- sapply(yrs, function(x) round(disagg_df[, x] * AG_muni_other_industry[, x], 0))
  
  export_file(module = 'Nutrients',
              file = AG_muni_other_industry, folder = 'Activity_data', 
              filename = param, subfolder = 'Correct_data_Municipality', subfolderX2 = INE_param, subfolderX3 = main_param)
  rm(list=c('AR_tomato','AG_muni_other_industry','AR_AG_muni_other','FRAC_AR','disagg_df','yrs'))
}




compute_corrected_INE_param <- function(INE_param) {
  
  
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


compute_annual_interpolated_param <- function(INE_param, main_param, param) {
  # calculates corrected new_muni for a given param 
  # time rules: expressed in average_AG_census_interpolation_period()
  
  
  # 1 - compile new_muni data based on the time-rules established in average_AG_census_interpolation_period
  AG_muni <- average_AG_census_interpolation_period(INE_param, main_param, param)
  # 2 - call the AR data for a given param 
  AR <- compute_INE_muni_agrarian(INE_param, main_param, param, 'Raw_data_Agrarian')
  # 3 - compute the new_muni at the AR level
  AG_AR <- compute_INE_muni_agrarian(INE_param, main_param, param, 'Raw_data_Agrarian')
  names(AG_AR) <- c('id', paste0('X',seq(1987,2017)))
  
  calc_yrs <- paste0('X',seq(1987,2017))x
  
  for (i in calc_yrs) {
    
    # 1 - calculate FRAC of AR_yr / AR_AG_census
    FRAC_AR <- data.frame(agrarian_region_id = seq(1,7))
    FRAC_AR[, 'FRAC_AR'] <- AG_AR_maize[, i] / AR_maize[, i]
    
    # 2 - call spatial disaggregation and create a template calculation
    disagg_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Spatial_disaggregation')
    disagg_df <- merge(disagg_df, FRAC_AR, 'agrarian_region_id')
    
    # 4 - calculate new_muni
    AG_muni[, i] <- round(AG_muni[, i] * disagg_df[, 'FRAC_AR'], 1)
  }
  return(AG_muni)
  rm(list=c('AR','AG_AR','FRAC_AR','disagg_df'))
}




