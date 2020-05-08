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

