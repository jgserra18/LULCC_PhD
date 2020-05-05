source('./Main/Global_functions.R')


disaggregate_admin <- function(admin, INE_param, main_param, param, df) {

  # spatially disaggregate 
  disagg_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Spatial_disaggregation')
  
  if (missing(df) == TRUE) {
    
    if (admin =='Agrarian') {
      
      file_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Agrarian', subfolder = INE_param, subfolderX2 = main_param, pattern = param)
      names(file_df)[1] <- 'agrarian_region_id'
      disagg_df <- merge(disagg_df, file_df, 'agrarian_region_id')
      disagg_df <- disagg_df[, -seq(2,8)]
    }
    
    else if (admin == 'Municipality') {
      
      file_df <- get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = INE_param, subfolderX2 = main_param, pattern = param)
      file_df <- file_df[, -c(2,3)]
      disagg_df <- merge(disagg_df, file_df, 'Muni_ID', sort = F)
      disagg_df <- disagg_df[, -seq(4,8)]
    }
  }
  
  return(disagg_df)
  rm(list='file_df')
}



## implement NUTS2 or NUTS3 --------------

sumIf_admin_regions <- function(admin, INE_param, main_param, param) {
  
  disagg_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Spatial_disaggregation')
  data_df <- disaggregate_admin(admin, INE_param, main_param, param)

  df <- data.frame(nuts2_ID = c('11','16','17','18','15'))

  for (i in 1:nrow(df)) {
    
    new_df <- subset(disagg_df, nuts2_ID == df[i,1])
    new_df <- new_df[, c(1,2)]
    new_df <- merge(new_df, data_df, 'Muni_ID')
    
    yrs <- paste0('X', seq(1987,2017))
    df[i, yrs] <- sapply(yrs, function(x) round(sum(new_df[, x]), 0))
  }
  
  return(df)
}


## SUM NATIONAL VALUES ----------------------------------------

compute_mainland_total <- function(calc_df) {
  # computes the total sum of a dataframe at the mainland 
  
  df <- data.frame(id = 'mainland')
  
  yrs <- paste0('X', seq(1987,2017))
  df[, yrs] <- sapply(yrs, function(x) df[, x] <- 0)
  
  df[1, yrs] <- sapply(yrs, function(x) sum(calc_df[,x]))

  return(df)
}



