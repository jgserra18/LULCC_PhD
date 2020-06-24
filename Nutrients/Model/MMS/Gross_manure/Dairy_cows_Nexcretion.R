source('./Main/Global_functions.R')
source('./Nutrients/Model/INE_DB/Populate_INE_AgrarianRegion.R')
source('./Main/Data_operations.R')


##  MILK PRODUCTION PER COw EXTRAPOLATION ---------------------------------------------------------------------------

milk_production_INE_var_id_mainParam = function(main_param) {
  
  df = data.frame(main_param = c('Bovine','Sheep','Goats'),
                  var_id = c('1','2','3'))
  df = df[which(df[, 1] == main_param), 2]
  
  return(df)
}



get_historical_dairy_milk_production <- function(main_param = 'Bovine') {
  # gets milk production for the yperiod 2003-2017@NUTS2 scale
  # unit: tonnes milk yr-1
  
  set_var_id = milk_production_INE_var_id_mainParam(main_param)
  
  milk_nuts2 <- get_agrarian_region_INE(INE_param_id = '0008608', var_id = set_var_id, year = seq(2003,2017), muni_id = c('11','16','17','18','15'))
  milk_nuts2 <- sapply(milk_nuts2, as.numeric)
  milk_nuts2 <- as.data.frame(milk_nuts2)
  names(milk_nuts2) <- c('nuts2_ID', paste0('X', seq(2003,2017)))
  
  file_name = ifelse(main_param=='Bovine', 'Dairy_NUTS2', paste0(main_param, '_NUTS2'))
  
  export_file(module = 'Nutrients', file = milk_nuts2, filename = file_name, folder = 'Activity_data', subfolder = 'General_params', subfolderX2 = 'Animals', subfolderX3 = 'Milk_production')
}



compute_milk_per_cow <- function() {
  # calculate milk produced per cow for 2003-2016@NUTS2
  # unit: kg milk cow-1 yr-1
  
  dairy_pop <- sumIf_admin_regions('Municipality','Animals','Bovine','Dairy_cows')
  
  # convert milk production to kg yr-1
  calc_cols <- paste0('X', seq(2003,2017))
  
  milk_prod <- get_activity_data(module = 'Nutrients', pattern = 'Dairy', folder = 'General_params', subfolder = 'Animals', subfolderX2 = 'Milk_production')
  milk_prod[, calc_cols] <- sapply(calc_cols, function(x) milk_prod[, x] * 1000)
  
  #  calculate milk prod per dairy cow
  milk_prod[, calc_cols] <- round(milk_prod[, calc_cols] / dairy_pop[, calc_cols], 1)
  milk_prod <- data_cleaning(milk_prod)
  
  return(milk_prod)
  rm(list=c('calc_cols','dairy_pop'))
}
milk_cow

compute_linear_extrapolation_milkPerCow_historical <- function() {
  
  
  milk_cow <- compute_milk_per_cow()
  names(milk_cow) <- gsub('X','', names(milk_cow))
  
  # create template df with all historical years
  # and populate milk_cow years (2003-2017)
  
  df <- data.frame(nuts2_ID = seq(1,5))
  yrs <- as.character(seq(1987,2017))
  df[, yrs] <- sapply(yrs, function(x) df[,x] <- NA)
  df[, as.character(seq(2003,2017))] <- milk_cow[,  as.character(seq(2003,2017))]
  
  for (i in 1:nrow(milk_cow)) {
    
    # make LM prediction for the remaining years (1987,2002)  
    calc_df <- data.frame(y =as.numeric(milk_cow[i,-1]), x = seq(2003,2017))
    lm_model <- lm(y ~ x, data = calc_df)
    lm_prediction <- predict(lm_model, newdata =  data.frame(x =  seq(1987,2002)))
    
    df[i, as.character(seq(1987,2002))] <- round(lm_prediction, 1)
  }
  
  return(df)
  rm(list=c('yrs','calc_df','lm_model','lm_prediction'))
}


convert_dairy_Nex_NUTS2_municipality <- function(NUTS2_df) {
  
  
  NUTS2_df[,1] <- c('11','16','17','18','15')
  
  disagg_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Spatial_disaggregation')
  disagg_df <- plyr::join(disagg_df, NUTS2_df)
  disagg_df <- disagg_df[, c('Muni_ID','ID','Muni', as.character(seq(1987,2017)))]
  
  return(disagg_df)
  rm(list=c('NUTS2_df'))
}


##  DAIRY COW N EXCRETION COEFFICIENT  ---------------------------------------------------------------------------


compute_annual_dairy_Nex <- function() {
  
  standard_productivity <- 7000 # kg milk per dairy cow
  standard_Nex <- 115 # kg N per dairy cow
  
  dairy_milk <- compute_linear_extrapolation_milkPerCow_historical()
  
  calc_cols <- as.character(seq(1987,2017))
  
  dairy_milk[, calc_cols] <- dairy_milk[, calc_cols] - 7000
  
  
  for (i in 1:nrow(dairy_milk)) {
    
    for (j in 2:ncol(dairy_milk)) {
      
      ifelse(dairy_milk[i,j]>0,
             dairy_milk[i,j] <- round(115+115*0.02*dairy_milk[i,j]/1000, 1),
             dairy_milk[i,j] <- round(115+115*0.1*dairy_milk[i,j]/1000, 1))
    }
  }
   dairy_milk <- convert_dairy_Nex_NUTS2_municipality(NUTS2_df = dairy_milk)
   export_file(module = 'Nutrients', file = dairy_milk, folder = 'Activity_data', filename = 'Dairy_Nex', subfolder = 'Nutrient_params', subfolderX2 = 'N', subfolderX3 = 'MMS', subfolderX4 = 'Excretion_coefficients')
}


##  DAIRY COW p EXCRETION COEFFICIENT  ---------------------------------------------------------------------------

compute_annual_dairy_P2O5_Pex <- function() {
  
  #CdBP18 ANNex Vi Nt/P2O5 ratio for dairy cow = 2.805
  # P2O5 to P = 0.4364
  
  
  dairy_Nex <- get_activity_data(module = 'Nutrients', pattern = 'Dairy_Nex', folder = 'Nutrient_params', subfolder = 'N', subfolderX2 = 'MMS', subfolderX3 = 'Excretion_coefficients')
  dairy_Pex <- dairy_Nex
  
  calc_cols <- paste0('X',seq(1987,2017))
  dairy_Pex[, calc_cols] <- sapply(calc_cols, function(x) round(dairy_Nex[, x] / 2.805 * 0.4364, 1))
  
  export_file(module = 'Nutrients', file = dairy_Pex, folder = 'Activity_data', filename = 'Dairy_Pex', subfolder = 'Nutrient_params', subfolderX2 = 'P', subfolderX3 = 'MMS', subfolderX4 = 'Excretion_coefficients')
  rm(list=c('dairy_Nex', 'dairy_Pex'))
}

