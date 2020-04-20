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

loop_INE_muni_agrarian <- function(df_merge) {
  # loopes the AG_census years while it computes the aggregated Muni --> AR for the same years
  
  df <- create_AR_template()
  yrs <- c('1989','1999','2009')
  
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
  
  if (missing(folder)==TRUE) {
    folder <- 'Raw_data_Municipality'
  } 
  else {
    folder <- 'Correct_data_Municipality'
  }
  # call the param file for a given animal or crop acreage
  param_df <- get_activity_data(module = 'Nutrients', folder = folder, subfolder = INE_param, subfolderX2 = main_param, pattern = param)
  
  # calculate AR for the AG_census years
  param_df <- loop_INE_muni_agrarian(param_df)
  names(param_df) <- c('id','X1989','X1999','X2009')
  
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
  

## INTERPOLATED NON-CENSUS YEARS  -----------------------------------------------------------------------------


# FORMULA --> new_muni_T <- muni_AG_census * (AR_T / AR_AG_census)

# 1 - sum muni to AR function
# 2 - define ref period of interpolation (1989 --> 1987-1995 || 1999 --> 1996 - 2005 || 2009 --> 2006 - 2017)
# 3 - correct municipality data based on the AR_AG_Census and AR
# 3 - Read acreage files for all crops
# 3.1 - Read AR data for each year
# 3.2 - Call

set_interpolation_period <- function(year,
                                     period_1 = as.character(seq(1987,1995)),
                                     period_2 = as.character(seq(1996,2005)),
                                     period_3 = as.character(seq(2006,2017))
                                     ) {
  # finda the closes AG_census year before interpolating
  
  if (year %in% period_1) {
    AG_yr <- 1989
  } 
  else if (year %in% period_2) {
    AG_yr <- 1999
  } 
  else {
    AG_yr <- 2009
  }
  
  return(AG_yr)
}


AG_maize <-  get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = 'Cereals' , pattern = 'Maize')
AG_AR_maize <-  compute_INE_muni_agrarian('Areas','Cereals','Maize','Correct_data_Municipality')

calc_yrs = c(1987,1988,seq(1990,1998), seq(2000,2008), seq(2010,2017))

for (i in calc_yrs) {
  
  AG_yr <- set_interpolation_period(year = i)
  
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
  
  
  
}





