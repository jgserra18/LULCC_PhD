

check_error_rounding <- function(df, param) {
  
  row_sums <- rowSums(df[, paste0('X', seq(1987,2017))])
  
  for(i in seq_along(row_sums)) {
    
    if ((row_sums[i]>10) == FALSE) {
      next 
    }
    else if ((row_sums[i] < -10) == FALSE) {
      next       
    }
    else {
      
      print(paste0(param, ': WRONG !!!!'))
      print(row_sums[i])
      break
    }
  }
  print(paste0(param, ': All good.'))
}


check_if_conditions <- function(df, param) {
  
  
  check_0 <- which(df[, paste0('X',seq(1987,2017))] != 0)

  if (identical(integer(0), check_0)==TRUE) {
    print(paste0(param, ': All good.'))
  }
  else {
    check_error_rounding(df, param)
  }
}

loop_param_checker <- function(fun) {
  
  standard_params <- get_standard_params_list('Animals')
  
  for (i in 1:nrow(standard_params)) {
    
    main_param <- standard_params[i, 'Main_animals']
    param <- standard_params[i, 'Animals']
    
    check <- fun(main_param, param)
    check_if_conditions(check, param)
  }
}


## CHECK 1 -----------------------------
source('./Nutrients/Model/MMS/Gross_manure/Compute_Nutrient_excretion.R')

Nflow_check1 <- function(main_param, param) {
  
  animal_pop <-  get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Animals', subfolderX2 = main_param, pattern = param)
  Nex <- select_animal_Nutrient_excretion_coefficient(nutrient = 'N', param = param)
  tNex1 <- animal_pop
  yrs <- paste0('X', seq(1987,2017))
  
  ifelse(param == 'Dairy_cows',
         tNex1[, yrs] <- sapply(yrs, function(x) round(Nex[, x] * animal_pop[,x], 1)),
         tNex1[, yrs] <- sapply(yrs, function(x) round(Nex * animal_pop[,x], 1)))
    
  
  grazN <- get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gross_manure', subfolder = 'N', subfolderX2 = 'Grazing', subfolderX3 ='Total', subfolderX4 = main_param, pattern = param)
  yardN <- get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gross_manure', subfolder = 'N', subfolderX2 = 'Yards', subfolderX3 ='Total', subfolderX4 = main_param, pattern = param)
  houseN <- get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gross_manure', subfolder = 'N', subfolderX2 = 'Housing', subfolderX3 ='Total', subfolderX4 = main_param, pattern = param)
  
  TNex2 <- houseN
  TNex2[, yrs] <- sapply(yrs, function(x) round(houseN[, x] + yardN[,x] + grazN[, x] , 1))
  
  check <- TNex2
  check[, yrs] <- sapply(yrs, function(x) round(TNex2[,x] - tNex1[,x], 0))
  
  return(check)
}


## CHECK 2 -----------------------------

source('./Nutrients/Model/MMS/Gross_manure/Compute_Nutrient_excretion.R')
source('./Nutrients/Model/MMS/Support_functions/Convert_N_to_TAN_flows.R')


Nflow_check2 <- function(main_param, param) {
  
  Nex <- compute_animal_nutrient_excretion('N', main_param, param)
  TAN <- convert_N_to_TAN_dataframe(main_param = main_param, df = Nex)
  
  TAN_coef <- get_activity_data(module = 'Nutrients', subfolder = 'Nutrient_params', subfolderX2 = 'N', subfolderX3 = 'MMS', subfolderX4 = 'Other_params', pattern = 'TAN')
  TAN_coef <- TAN_coef[which(TAN_coef[, 'Main_animal']==main_param), 'TAN']

  check <- Nex
  
  yrs <- paste0('X', seq(1987,2017))
  check[, yrs] <- sapply(yrs, function(x) round(check[,x] * TAN_coef - TAN[,x], 0))
  
  return(check)
}

## CHECK 3 + 4---------------------------------

source('./Nutrients/Model/MMS/Housing/Housing.R')

Nflow_check3 <- function(main_param, param) {
  
  house_slurryN <-  get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gross_manure', subfolder = 'N', subfolderX2 = 'Housing', subfolderX3 ='Slurry', subfolderX4 = main_param, pattern = param)
  house_slurryTAN <- convert_N_to_TAN_dataframe(main_param = main_param, df = house_slurryN)
 
  house_solidN <-  get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gross_manure', subfolder = 'N', subfolderX2 = 'Housing', subfolderX3 ='Solid', subfolderX4 = main_param, pattern = param)
  house_solidTAN <- convert_N_to_TAN_dataframe(main_param = main_param, df = house_solidN)
  
  house_totalN <-  get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gross_manure', subfolder = 'N', subfolderX2 = 'Housing', subfolderX3 ='Total', subfolderX4 = main_param, pattern = param)
  house_totalTAN <- convert_N_to_TAN_dataframe(main_param = main_param, df = house_totalN)
  
  check_TAN <- house_totalTAN

  yrs <- paste0('X', seq(1987,2017))
  check_TAN[, yrs] <- sapply(yrs, function(x) round(house_totalTAN[,x] - (house_solidTAN[,x] + house_slurryTAN[,x]), 0))
  
  return(check_TAN)
}

## CHECK 4 ---------------------------------

Nflow_check4 <- function(main_param, param) {
  
  house_slurryN <-  get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gross_manure', subfolder = 'N', subfolderX2 = 'Housing', subfolderX3 ='Slurry', subfolderX4 = main_param, pattern = param)

  house_solidN <-  get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gross_manure', subfolder = 'N', subfolderX2 = 'Housing', subfolderX3 ='Solid', subfolderX4 = main_param, pattern = param)

  house_totalN <-  get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gross_manure', subfolder = 'N', subfolderX2 = 'Housing', subfolderX3 ='Total', subfolderX4 = main_param, pattern = param)

  check_N <- house_totalN
  
  yrs <- paste0('X', seq(1987,2017))
  check_N[, yrs] <- sapply(yrs, function(x) round(house_totalN[,x] - (house_solidN[,x] + house_slurryN[,x]), 0))
  
  return(check_N)
}

## CHECK 5 ----------------------------------------

source('./Nutrients/Model/MMS/Housing/Housing.R')
source('./Nutrients/Model/MMS/Storage/Storage.R')


Nflow_check5 <- function(main_param, param) {
  
  N_bedding <- general_func_animal_bedding(main_param, param, bedding_param = 'N_bedding')
  house_solidN <-  get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gross_manure', subfolder = 'N', subfolderX2 = 'Housing', subfolderX3 ='Solid', subfolderX4 = main_param, pattern = param)
  
  FRAC_MMS_slurry <-  linearly_intrapolate_share_MMS(general_param = 'Share_MMS', param = 'Slurry')
  FRAC_MMS_slurry <- subset(FRAC_MMS_slurry, Animals == param)
  
  E_house <- compute_housing_NH3_emissions(main_param, param, manure_type = 'Solid')
  Ex_house_N <- compute_solid_N_entering_storage(main_param, param, manure_type = 'Solid')
  
  check <- Ex_house_N
  
  yrs <- paste0('X', seq(1987,2017))
  check[, yrs] <- sapply(yrs, function(x) round(N_bedding[, x] + house_solidN[,x] * (1 - FRAC_MMS_slurry[, x]) - (E_house[,x] + Ex_house_N[, x]), 0))
  
  return(check)
}
loop_param_checker(Nflow_check6)

## CHECK 6 -----------------------------------------

Nflow_check6 <- function(main_param, param) {
  
  slurry_TAN_storage <- correct_slurry_TAN_storage(main_param, param)
  
  yardN <- get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gross_manure', subfolder = 'N', subfolderX2 = 'Yards', subfolderX3 ='Total', subfolderX4 = main_param, pattern = param)
  yardTAN <- convert_N_to_TAN_dataframe(main_param = main_param, df = yardN)
  
  house_slurryN <-  get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gross_manure', subfolder = 'N', subfolderX2 = 'Housing', subfolderX3 ='Slurry', subfolderX4 = main_param, pattern = param)
  house_slurryTAN <- convert_N_to_TAN_dataframe(main_param = main_param, df = house_slurryN)
  
  yard_NH3 <- compute_yards_NH3_emissions(main_param, param, 'Slurry')
  house_slurry_NH3 <- compute_housing_NH3_emissions(main_param, param, 'Slurry')
  
  X_store_slurry <-  set_manure_storage_fraction(main_param = main_param, manure_type = 'Slurry')
  
  
  storage_slurry_NH3 <- general_func_compute_storage_Nemissions(N_gas = 'NH3', main_param = main_param, param = param, manure_type = 'Slurry')
  storage_slurry_N2O <- general_func_compute_storage_Nemissions(N_gas = 'N2O', main_param = main_param, param = param, manure_type = 'Slurry')
  storage_slurry_NOx <- general_func_compute_storage_Nemissions(N_gas = 'NOx', main_param = main_param, param = param, manure_type = 'Slurry')
  storage_slurry_NN2 <- general_func_compute_storage_Nemissions(N_gas = 'NN2', main_param = main_param, param = param, manure_type = 'Slurry')
  E_storage <- storage_slurry_NN2
  
  spread_TAN <- compute_total_slurry_available_spreading(N_flow = 'TAN', main_param = main_param, param = param, manure_type = 'Slurry')
  
  check <- spread_TAN
  yrs <- paste0('X', seq(1987,2017))
  
  E_storage[, yrs] <- sapply(yrs, function(x) round(storage_slurry_NH3[,x] + storage_slurry_N2O[,x] + storage_slurry_NOx[,x] + storage_slurry_NN2[,x], 1))
  check[,yrs] <- sapply(yrs, function(x) round(slurry_TAN_storage[, x] + ( (yardTAN[,x] + house_slurryTAN[,x]) - (yard_NH3[,x] + house_slurry_NH3[,x])) * (1 - X_store_slurry) - E_storage[, x] - spread_TAN[,x], 0))
  
  return(check)
  rm(list=c('slurry_TAN_storage','yardN','yardTAN','house_slurryN','house_slurryTAN','yard_NH3','house_slurry_NH3','FRAC_MMS_slurry','storage_slurry_NH3','storage_slurry_N2O','storage_slurry_NOx','storage_slurry_NN2','E_storage'))
}

## CHECK 7 -----------------------------------------

Nflow_check7 <- function(main_param, param) {
  
  house_solidN <-  get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gross_manure', subfolder = 'N', subfolderX2 = 'Housing', subfolderX3 ='Solid', subfolderX4 = main_param, pattern = param)
  house_solidTAN <- convert_N_to_TAN_dataframe(main_param = main_param, df = house_solidN)
  
  Straw_bedding <- general_func_animal_bedding(main_param, param, bedding_param = 'Straw')
  f_imm = 0.0067
  
  E_house <- compute_housing_NH3_emissions(main_param, param, manure_type = 'Solid')
  E_storage <- general_func_compute_total_storage_emissions(main_param, param, 'Solid')
  
  applic_solid_TAN <- compute_solid_TAN_spreading(main_param, param, 'Solid')
  
  check <- E_house
  yrs <- paste0('X', seq(1987,2017))
  check[,yrs] <- sapply(yrs, function(x) round((house_solidTAN[,x]-(Straw_bedding[,x] * f_imm)-E_house[,x]) - E_storage[,x] - applic_solid_TAN[,x], 1))
  
  return(check)
}

## CHECK 8 -----------------------------------------

Nflow_check8 <- function(main_param, param) {
  
  house_slurryN <-  get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gross_manure', subfolder = 'N', subfolderX2 = 'Housing', subfolderX3 ='Slurry', subfolderX4 = main_param, pattern = param)
  yards_N <-  get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gross_manure', subfolder = 'N', subfolderX2 = 'Yards', subfolderX3 ='Total', subfolderX4 = main_param, pattern = param)
  E_house <- compute_housing_NH3_emissions(main_param, param, manure_type = 'Slurry')
  E_yards <- compute_yards_NH3_emissions(main_param = main_param, param = param, manure_type = 'Total')
  E_storage <- general_func_compute_total_storage_emissions(main_param, param, 'Slurry')
  E_app <- compute_manure_spreading_NH3_emissions(main_param, param, 'Slurry')
  net_slurry_spread <- compute_manure_spreading_net_N(N_flow = 'N',main_param =  main_param , param =  param, manure_type = 'Slurry')
  
  check <- E_house
  yrs <- paste0('X', seq(1987,2017))
  check[,yrs] <- sapply(yrs, function(x) round( (house_slurryN[,x] + yards_N[,x]) - (E_house[,x] + E_yards[,x] + E_storage[,x] + E_app[,x]) - net_slurry_spread[,x], 1))
  
  return(check)
}

## CHECK 9 -----------------------------------------

Nflow_check9 <- function(main_param, param) {
  
  house_solidN <-  get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gross_manure', subfolder = 'N', subfolderX2 = 'Housing', subfolderX3 ='Solid', subfolderX4 = main_param, pattern = param)
  N_bedding <- general_func_animal_bedding(main_param, param, bedding_param = 'N_bedding')
  E_house <- compute_housing_NH3_emissions(main_param, param, manure_type = 'Solid')
  X_store_solid <-  set_manure_storage_fraction(main_param = main_param, manure_type = 'Solid')
  E_storage <- general_func_compute_total_storage_emissions(main_param, param, 'Solid')
  E_app <- compute_manure_spreading_NH3_emissions(main_param, param, 'Solid')
  net_solid_spread <- compute_manure_spreading_net_N(N_flow = 'N',main_param =  main_param , param =  param, manure_type = 'Solid')
  
  
  check <- E_house
  yrs <- paste0('X', seq(1987,2017))
  check[,yrs] <- sapply(yrs, function(x) round( ((house_solidN[,x] + N_bedding[,x] - E_house[,x]) * X_store_solid) - net_solid_spread[,x] - E_app[,x] - net_solid_spread[,x], 1))
  
  return(check)
}

