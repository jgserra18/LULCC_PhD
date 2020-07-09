source('./Main/Global_functions.R')


## linear extrapolation ---------------------------------- 

# needs to be corrected
general_linear_extrapolation_3years <- function(file_df, existing_years, xout=c(seq(1987,1989), seq(1991,1998), seq(2000,2008), seq(2010,2017))) {
  
  new_file <- file_df[, -c(1,2)]
  names(new_file) <- gsub('X','',names(new_file))
  
  store <- file_df[, c(1,2)]
  
  calc_cols <- paste0('X', seq(1987,2017))
  store[, calc_cols] <- sapply(calc_cols, function(x) store[,] <- NA)

  populate_cols <- existing_years
  store[, populate_cols] <- sapply(populate_cols, function(x) store[,x] <- file_df[,x])
  
  
  for (i in 1:nrow(new_file)) {
    
    calc_df <- data.frame(y = c(new_file[i,1], new_file[i,2], new_file[i,3]), x = c(1990,1999,2009))
    lm_model <- lm(y~x, calc_df)
    lm_prediction <- round(
      predict(lm_model, newdata =  data.frame(x =  xout)), 2)
    
    names(lm_prediction) <- paste0('X', xout)
    lm_prediction <- ifelse(lm_prediction>1, 1, round(lm_prediction,2))
    lm_prediction <- ifelse(lm_prediction<0, 0, round(lm_prediction, 2))
    store[i, names(lm_prediction)] <- lm_prediction
  }
  return(store)
  rm(list=c('new_file','calc_cols','populate_cols','calc_df', 'lm_model','lm_prediction'))
}




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




general_sumIF_NUTS2 = function(admin, data_df) {
  
  disagg_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Spatial_disaggregation')
  df <- data.frame(nuts2_ID = c('11','16','17','18','15'))
  
  for (i in 1:nrow(df)) {
    
    new_df <- subset(disagg_df, nuts2_ID == df[i,1])
    new_df <- new_df[, c(1,2)]
    new_df <- merge(new_df, data_df, by='Muni_ID')
    
    yrs <- paste0('X', seq(1987,2017))
    df[i, yrs] <- sapply(yrs, function(x) round(sum(new_df[, x]), 0))
  }
  return(df)
}


## implement NUTS2 or NUTS3 --------------

sumIf_admin_regions <- function(admin, INE_param, main_param, param) {
  
  disagg_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Spatial_disaggregation')
  
  store_df = disaggregate_admin(admin, INE_param, main_param, param)

  df <- data.frame(nuts2_ID = c('11','16','17','18','15'))

  for (i in 1:nrow(df)) {
    
    new_df <- subset(disagg_df, nuts2_ID == df[i,1])
    new_df <- new_df[, c(1,2)]
    new_df <- merge(new_df, store_df, by='Muni_ID')
    
    yrs <- paste0('X', seq(1987,2017))
    df[i, yrs] <- sapply(yrs, function(x) round(sum(new_df[, x]), 0))
  }
  
  return(df)
}



general_func_sumIF_admin_df = function(admin, merge_df, merge_col) {
  # general function to aggregate statistics to a higher resolution
  
  disagg_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Spatial_disaggregation')
  
  if (missing(merge_col)==TRUE) {
    
    disagg_df = plyr::join(disagg_df, merge_df)
  }
  else {
    
    disagg_df = plyr::join(disagg_df, merge_df, 'Muni_ID')
  }

  
  admin_shp = get_activity_data(module = 'LULCC', folder = 'Admin', pattern = admin)
  store_df = data.frame(Admin_id = unique(admin_shp$Admin_id))
  
  for (i in 1:nrow(store_df)) {
    
    
    sb_df = subset(disagg_df, select = merge_col)
    store_df[i, merge_col] = sum(sb_df)
  }
  return(store_df)
}




compute_temporal_sumIF_admin_df = function(admin, merge_df) {
  # USE FOR INSTANCE TO SCALE ATMOSPHERIC DEPOSITION
  # to be applied to dfs with temporal data
  
  disagg_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Spatial_disaggregation')
  disagg_df = plyr::join(disagg_df, merge_df, 'Muni_ID')
  
  admin_shp = get_activity_data(module = 'LULCC', folder = 'Admin', pattern = admin)
  store_df = data.frame(Admin_id = unique(admin_shp$Admin_id))
  
  yrs = paste0('X', seq(1987,2017))
  
  if (admin=='NUTS3') { col_patt = 'nuts3_ID' } else if ( admin=='NUTS2') { col_patt = 'nuts2_ID' } else if (admin=='AR' | admin == 'Agrarian_region') { col_patt = 'agrarian_region_id'}
  
  for (i in 1:nrow(store_df)) {
    
    for (yr in yrs) {
      
      find_admin_id = which(disagg_df[, col_patt] == store_df[i, 1])
      sb_df = disagg_df[find_admin_id, yr]
      store_df[i, yr] = sum(sb_df)
    }
  }
  return(store_df)
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



