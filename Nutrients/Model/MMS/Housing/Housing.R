source('./Main/Global_functions.R')
source('./Nutrients/Model/MMS/Support_functions/Convert_N_to_TAN_flows.R')
source('./Nutrients/Model/MMS/Support_functions/Find_EFs.R')

## N-NH3 emissions from yards -----------------------------------
## EMEP 2016

compute_yards_NH3_emissions <- function(main_param, param, manure_type) {
  # computes grazing NH3 emissions
  # unit: kg N-NH3 yr-1
  
  yards_maN <-  get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gross_manure', subfolder = 'N', subfolderX2 = 'Yards', subfolderX3 ='Total', subfolderX4 = main_param, pattern = param)
  yards_TAN <- convert_N_to_TAN_dataframe(main_param = main_param, df = yards_maN)
  
  EF <- select_animal_N_EFs(N_gas = 'NH3', pathway = 'Yards', param = param, manure_type = 'Solid') #for yards it is the same
  
  yrs <- paste0('X', seq(1987,2017))
  yards_TAN[, yrs] <- sapply(yrs, function(x) round(yards_TAN[,x] * EF, 1))
  yards_TAN = data_cleaning(yards_TAN)
  
  return(yards_TAN)
}


loop_yards_NH3_emissions <- function() {
  
  standard_params <- get_standard_params_list('Animals')
  
  yrs <- paste0('X', seq(1987,2017))
  store_total <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  store_total[, yrs] <- sapply(yrs, function(x) store_total[,x] = 0)
  

    for (i in 1:nrow(standard_params)) {
      
      main_param <- standard_params[i, 'Main_animals']
      param <- standard_params[i, 'Animals']
      
      yards_NH3 <- compute_yards_NH3_emissions(main_param = main_param, param = param, manure_type = 'Total')
      export_file(module = 'Nutrients', 
                  file = yards_NH3, 
                  filename = param, 
                  folder = 'Gas_N_emissions', 
                  subfolder = 'NH3', 
                  subfolderX2 = 'Yards',
                  subfolderX3 = 'Total',
                  subfolderX4 = main_param)
      
      store_total[, yrs] = sapply(yrs, function(x) round(yards_NH3[,x] + store_total[, x], 1))
    }
  export_file(module = 'Nutrients', 
              file = store_total, 
              filename = 'Total', 
              folder = 'Gas_N_emissions', 
              subfolder = 'NH3', 
              subfolderX2 = 'Yards',
              subfolderX3 = 'Total',
              subfolderX4 = 'Total')
  
  rm(list=c('standard_params','man_type','main_param','param','yards_NH3'))
}


## N-NH3 emissions from housing -----------------------------------
## EMEP 2016

compute_housing_NH3_emissions <- function(main_param, param, manure_type) {
  # computes grazing NH3 emissions
  # unit: kg N-NH3 yr-1
  
  house_maN <-  get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gross_manure', subfolder = 'N', subfolderX2 = 'Housing', subfolderX3 =manure_type, subfolderX4 = main_param, pattern = param)
  house_TAN <- convert_N_to_TAN_dataframe(main_param = main_param, df = house_maN)
  
  EF <- select_animal_N_EFs(N_gas = 'NH3', pathway = 'Yards', param = param, manure_type = manure_type)
  
  yrs <- paste0('X', seq(1987,2017))
  house_TAN[, yrs] <- sapply(yrs, function(x) round(house_TAN[,x] * EF, 1))
  return(house_TAN)
}


loop_housing_NH3_emissions = function() {
  
  standard_params <- get_standard_params_list('Animals')
  
  yrs <- paste0('X', seq(1987,2017))
  store_total <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  store_total[, yrs] <- sapply(yrs, function(x) store_total[,x] = 0)
  
  store_individual = store_total
  
  man_type = c('Solid','Slurry')
  
  for (j in man_type) {
    
      store_man_type = store_individual
      
      for (i in 1:nrow(standard_params)) {
        
        main_param <- standard_params[i, 'Main_animals']
        param <- standard_params[i, 'Animals']
        
        house_NH3 <- compute_housing_NH3_emissions(main_param = main_param, param = param, manure_type = j)
        house_NH3 = data_cleaning(house_NH3)
        export_file(module = 'Nutrients', 
                    file = house_NH3, 
                    filename = param, 
                    folder = 'Gas_N_emissions', 
                    subfolder = 'NH3', 
                    subfolderX2 = 'Housing',
                    subfolderX3 = j,
                    subfolderX4 = main_param)
        
        # add to the total df
        store_total[, yrs] = sapply(yrs, function(x) round(house_NH3[,x] + store_total[, x], 1))
        
        # add to individual dt
        store_man_type[, yrs] = sapply(yrs, function(x) round(house_NH3[,x] + store_man_type[, x], 1))
      }
      # export totala sum of a given a manure type
      export_file(module = 'Nutrients', 
                  file = store_man_type, 
                  filename = 'Total', 
                  folder = 'Gas_N_emissions', 
                  subfolder = 'NH3', 
                  subfolderX2 = 'Housing',
                  subfolderX3 = j,
                  subfolderX4 = 'Total')
      
  }
  export_file(module = 'Nutrients', 
              file = store_total, 
              filename = 'Total', 
              folder = 'Gas_N_emissions', 
              subfolder = 'NH3', 
              subfolderX2 = 'Housing',
              subfolderX3 = 'Total',
              subfolderX4 = 'Total')
  
  rm(list=c('standard_params','man_type','main_param','param','house_NH3', 'store_total', 'store_man_type'))
}



## COMPUTE N/TAN leaving animal housing systems (SOLID ONLY)  -----------------------------------
## EMEP 2016

## NOTE1 : THIS IS USED TO CALCULATE THE AMOUNT OF N/TAN ENTERING STORAGE IN SOLID SYSTEMS
## ALTHOUGH THE FUNCTIONS ARE IMPLEMENTED HERE, PLEASE SEE THEIR APPLITATION IN STORAGE_MMS.R
## NOTE2 : GO TO STORAGE.R

source('./Nutrients/Model/MMS/Support_functions/TimeExtrapolation_MMSparams.R')



general_func_animal_bedding <- function(main_param, param, bedding_param) {
  # computes total amount of bedding material (in tonnes or tonnes N)
  # bedding_param is "Straw" or "N_bedding"
  # unit: kg straw yr -1 or kg N straw yr-1
  
  bedding_params <- get_activity_data(module = 'Nutrients', subfolder = 'Nutrient_params', subfolderX2 = 'N', subfolderX3 = 'MMS', subfolderX4 = 'Other_params', pattern = 'Animal_bedding')
  bedding_params <- subset(bedding_params, Animal == param)
  bedding_params <- bedding_params[, bedding_param]
  
  FRAC_MMS_solid <-  linearly_intrapolate_share_MMS(general_param = 'Share_MMS', param = 'Solid')
  FRAC_MMS_solid <- subset(FRAC_MMS_solid, Animals == param)
  
  FRAC_housing <- set_FRAC_pathway(pathway = 'Housing', main_param, param)
  animal_pop <- get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Animals', subfolderX2 = main_param, pattern = param)
  
  yrs <- paste0('X', seq(1987,2017))
  animal_pop[, yrs] <- sapply(yrs, function(x) round(animal_pop[,x] * FRAC_MMS_solid[, x] * bedding_params * FRAC_housing[, x], 1))
  
  return(animal_pop)
  rm(list=c('bedding_params','FRAC_MMS_solid','animal_pop','yrs'))
}



