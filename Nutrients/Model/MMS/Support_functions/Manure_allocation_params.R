source('./Main/Global_functions.R')
source('./Nutrients/Model/INE_DB/Population_INE_Municipality.R')


# GET DATA FOR 2009 FROM STATISTICS PORTUGAL ----------------------------------------------------------------------------------------



manure_params_2009 <- function() {
  
  params = data.frame(param = c('FRAC_River_discharge','FRAC_Fertiliser','FRAC_Transport'),
                      var_id = c('1','2','5'))
  return(params)
}


get_manure_params_2009 <- function() {
  
  df <- manure_params_2009()
  
  for (i in 1:nrow(df)) {
    
    muni_param <- get_municipality_INE(INE_param_id = '0005815', var_id = df[i, 'var_id'], year = '2009', other_params = 'Def')
    names(muni_param)[4] <- 'X2009'
    muni_param[, 'X2009'] <- muni_param[, 'X2009']/100
    export_file(module = 'Nutrients',
                file = muni_param, 
                folder = 'Activity_data', 
                filename = df[i, 'param'], 
                subfolder = 'General_params', 
                subfolderX2 = 'Animals',
                subfolderX3 = 'Manure_allocation')
  }
}



# DEVELOPMENT OF HISTORICAL SCENARIOS FOR THE DIFFERENT FRACS ----------------------------------------------------------------------------------------

source('./Nutrients/Model/MMS/Spreading/Spreading.R')



get_historical_scenario_annual_rate <- function(Manure_frac, year) {
  
  historical_rates <- get_activity_data(module = 'Nutrients', folder = 'Scenarios', subfolder = 'Manure', subfolderX2 = 'Manure_use', pattern = 'manure_allocation')
  
  yr_row <- which(historical_rates[, 1] == year)
  
  if (Manure_frac == 'Fertiliser') {
    
    yr_rate <- historical_rates[yr_row, 'Rate_Spreading']
  }
  else {
    
    yr_rate <- historical_rates[yr_row, 'Rate_discharge']
  }
  
  return(yr_rate)
  rm(list=c('historical_rates','yr_row'))
}




// what about biogas ?? 
// what will happen if in the future the share of biogas gets bigger? Apply EMEP algorithm and assess emissions

construct_historical_FRAC_spreading_scenario <- function() {
  
  FRAC_spreading <- manure_spreading_allocation_FRAC(manure_allocation = 'Fertiliser') # for 2009
  
  yrs <- paste0('X', seq(1987,2017))
  df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  df[, yrs] <- sapply(yrs, function(x) df[,x] <- 0)
  df[, 'X2009'] <- FRAC_spreading[, 'X2009']
  
  # construct pre-2009 scenario based on 2009 data and using fixed rates with basis on portuguese legislation -------
  past_yr <- seq(2008,1987,-1)
  
  for (i in past_yr) {
    
    yr_rate <- get_historical_scenario_annual_rate(Manure_frac = 'Fertiliser', year = paste0('X', i))
    
    # get FRAC from t + 1
    t_1 <- as.integer(i) + 1
    df[, paste0('X', i)] <- round(df[, paste0('X', t_1)] * (1 - yr_rate), 4)
  }

  # construct pos-2009 scenario based on 2009 data and using fixed rates with basis on portuguese legislation -------
  future_yrs <- seq(2010,2017,1)
  
  for (j in future_yrs) {
    
    yr_rate <- get_historical_scenario_annual_rate(Manure_frac = 'Fertiliser', year = paste0('X', j))
    t_1 <- as.integer(j) - 1
    df[, paste0('X', j)] <- round(df[, paste0('X', t_1)] +  ( df[, paste0('X', t_1)] * yr_rate), 4)
  }
  
  # correct if allocation rates are higer than 1
  correct_ids <- which(df[, seq(4, ncol(df))]>1, arr.ind = TRUE)
  
  for (z in 1:nrow(correct_ids)) {
    
    r <- correct_ids[z,1]
    c <- correct_ids[z, 2]
    
    df[r,c] <- 1
  }
  
  return(df)
}
d = construct_historical_FRAC_spreading_scenario()
View(d)
correct_ids <- which(d>1, arr.ind = TRUE)

