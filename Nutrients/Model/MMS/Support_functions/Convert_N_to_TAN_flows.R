source('./Main/Global_functions.R')



call_and_convert_N_to_TAN_flows <- function(main_param, param, pathway, manure_type) {
  # calls and converts N flows from Housing, Grazing and Yards to TAN flows
  # REQUIRED TO CALCULATE GASEOUS N EMISSIONS USING EMEP 2016 -----------
  # unit: kg TAN yr-1
  
  pathway_maN_type <-  get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gross_manure', subfolder = 'N', subfolderX2 = pathway, subfolderX3 =manure_type, subfolderX4 = main_param, pattern = param)
  
  TAN_coef <- get_activity_data(module = 'Nutrients', subfolder = 'Nutrient_params', subfolderX2 = 'N', subfolderX3 = 'MMS', subfolderX4 = 'Other_params', pattern = 'TAN')
  TAN_coef <- TAN_coef[which(TAN_coef[, 'Main_animal']==main_param), 'TAN']
  
  yrs <- paste0('X', seq(1987,2017))
  pathway_maN_type[, yrs] <- sapply(yrs, function(x) round(pathway_maN_type[, x] * TAN_coef, 1))
  
  return(pathway_maN_type)
  rm(list=c('yrs','TAN_coef'))
}

convert_N_to_TAN_dataframe <- function(main_param, df) {
  # simply converts an existing dataframe with N flows to TAN flows
  # unit: kg TAN yr-1
  
  TAN_coef <- get_activity_data(module = 'Nutrients', subfolder = 'Nutrient_params', subfolderX2 = 'N', subfolderX3 = 'MMS', subfolderX4 = 'Other_params', pattern = 'TAN')
  TAN_coef <- TAN_coef[which(TAN_coef[, 'Main_animal']==main_param), 'TAN']
  
  yrs <- paste0('X', seq(1987,2017))
  df[, yrs] <- sapply(yrs, function(x) round(df[, x] * TAN_coef, 1))
  
  return(df)
  rm(list=c('TAN_coef','yrs'))
}
