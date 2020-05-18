source('./Main/Global_functions.R')


## PHOSPHORUS AND NITROGEN IMPLEMENTATION -------------------------------------------------------------------------------------


select_animal_Nutrient_excretion_coefficient <- function(nutrient, param) {
  # finds the nutrient excretion rate for a given nutrient and animal
  # param is Animals (e.g., Non_dairy, etc)
  
  # get nutrient excretion coefficient datasets
  if (param == 'Dairy_cows') {
    
    animal_coef <- get_activity_data(module = 'Nutrients', subfolder = 'Nutrient_params', subfolderX2 = nutrient, subfolderX3 = 'MMS', subfolderX4 = 'Excretion_coefficients', pattern = 'Dairy')
  }
  else {
    
    nutrient_coef <- get_activity_data(module = 'Nutrients', subfolder = 'Nutrient_params', subfolderX2 = nutrient, subfolderX3 = 'MMS', subfolderX4 = 'Excretion_coefficients', pattern = 'Other')
    coef_id <- which(nutrient_coef[, 'Animals'] == param)
    animal_coef <- nutrient_coef[coef_id, ncol(nutrient_coef)]
    rm(list=c('nutrient_coef', 'coef_id'))
  }
  
  return(animal_coef)
}


compute_animal_nutrient_excretion <- function(nutrient, main_param, param) {
  # computes the total nutrient excretion for a given animal
  # unit: kg N yr-1 or kg P yr-1
  
  nutrient_coef <- select_animal_Nutrient_excretion_coefficient(nutrient, param)
  animal_pop <-  get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Animals', subfolderX2 = main_param, pattern = param)
  animal_excretion <- animal_pop
  
  calc_cols <- paste0('X', seq(1987,2017))
  
  ifelse(param == 'Dairy_cows',
    animal_excretion[, calc_cols] <- sapply(calc_cols, function(x) round(animal_pop[, x] * nutrient_coef[, x], 0)),
    animal_excretion[, calc_cols] <- sapply(calc_cols, function(x) round(animal_pop[, x] * nutrient_coef, 0)))
  
  return(animal_excretion)
  rm(list=c('nutrient_coef','animal_pop','calc_cols'))
}


## gross manure of a given nutrient --------------------------------------

compute_gross_manure_nutrient <- function(nutrient) {
  # computes the gross manure of a given nutrient for all animals
  
  standard_params <- get_standard_params_list('Animals')
  
  for (i in 1:nrow(standard_params)) {
    
    main_param <- standard_params[i, 'Main_animals']
    param <- standard_params[i, 'Animals']
    
    # compute annual nutrient excretion
    animal_excretion <- compute_animal_nutrient_excretion(nutrient, main_param, param)
    export_file(module = 'Nutrients', 
                file = animal_excretion, 
                filename = param, 
                folder = 'Gross_manure', 
                subfolder = nutrient, 
                subfolderX2 = paste0('Total_',nutrient,'excretion'), 
                subfolderX3 = main_param)
  }
  rm(list=c('standard_params','param_col','main_param_col','main_param','param','animal_excretion'))
}


loop_gross_manure_nutrient <- function() {
  
  n <- c('P','N')
  sapply(n, function(x) compute_gross_manure_nutrient(x))
}


####  GROSS MANURE ALLOCATION (HOUSING, GRAZING, YARDS) ------------------------------------

source('./Nutrients/Model/MMS/Support_functions/TimeExtrapolation_MMSparams.R')


## dairy cow gross manure conditions -----------------------------------------------------------------

grazing_dairy_cows_condition <- function(dairy_graz_frac) {
  # assumption: no grazing in Agrarian Region Entre Douro e Minho
  
  disagg_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Spatial_disaggregation')
  
  rows_AR_1 <- which(disagg_df$agrarian_region_id == '1')
  
  calc_cols <- paste0('X', seq(1987,2017))
  disagg_df[, calc_cols] <- sapply(calc_cols, function(x) disagg_df[,x] <- NA)
  
  disagg_df[-rows_AR_1, calc_cols] <- sapply(calc_cols, function(x) disagg_df[-rows_AR_1, x] <- dairy_graz_frac[, x])
  disagg_df[rows_AR_1, calc_cols] <- sapply(calc_cols, function(x) disagg_df[rows_AR_1, x] <- 0)
  
  return(disagg_df)
}


housing_dairy_cows_condition <- function(dairy_other_frac) {
  # rescales yard and housing frac for dairy cows based on the assumption that AR_region == 1 no grazing dairy
  
  ## get grazing FRAC timeseries (1987-2017)
  FRAC_grazing <- correct_share_MMS_pathway('Grazing','Dairy_cows')

  # rescale dairy_other_frac
  calc_cols <- paste0('X', seq(1987,2017))
  new_dairy_frac <- dairy_other_frac
  new_dairy_frac[, calc_cols] <- sapply(calc_cols, function(x) round(dairy_other_frac[, x] + FRAC_grazing[, x], 2))
  
  
  disagg_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Spatial_disaggregation')
  
  rows_AR_1 <- which(disagg_df$agrarian_region_id == '1')
  
  disagg_df[, calc_cols] <- sapply(calc_cols, function(x) disagg_df[,x] <- NA)
  
  disagg_df[-rows_AR_1, calc_cols] <- sapply(calc_cols, function(x) disagg_df[-rows_AR_1, x] <- dairy_other_frac[, x])
  disagg_df[rows_AR_1, calc_cols] <- sapply(calc_cols, function(x) disagg_df[rows_AR_1, x] <- new_dairy_frac[, x])
  
  return(disagg_df)
}


compute_dairy_manure_pathway <- function(dairy_man_nutrient, dairy_pathway_frac) {
  # calculates grazing manure N of dairy cattle
  # unit: kg N yr-1
  
  calc_cols <- paste0('X', seq(1987,2017))
  dairy_man_nutrient[, calc_cols] <- sapply(calc_cols, function(x) round(dairy_man_nutrient[, x] <- dairy_man_nutrient[, x] * dairy_pathway_frac[,x], 1))
  
  return(dairy_man_nutrient)
}


## ALLOCATE GROSS MANURE ----------------------------------------------------------------------------------------------------------------------


general_func_gross_manure_allocation <- function(nutrient, pathway) {
  # general function to ocmpute gross manure allocation
  

  standard_params <- get_standard_params_list(main_param = 'Animals')
  
  for (i in 1:nrow(standard_params)) {
    
    main_param <- standard_params[i, 'Main_animals']
    param <- standard_params[i, 'Animals']

    FRAC_pathway_animal <- correct_share_MMS_pathway(pathway = pathway, param = param)
    
    # get gross man N
    man_nutrient <- compute_animal_nutrient_excretion(nutrient, main_param, param)
    pathway_N <- man_nutrient
    
    
    if (param == 'Dairy_cows' && pathway == 'Grazing') {
      
      FRAC_pathway_animal <- grazing_dairy_cows_condition(dairy_graz_frac = FRAC_pathway_animal)
      pathway_N <- compute_dairy_manure_pathway(man_nutrient, FRAC_pathway_animal)
    }
    else if (param == 'Dairy_cows' && pathway == 'Housing') {
      
      FRAC_pathway_animal <- housing_dairy_cows_condition(dairy_other_frac =  FRAC_pathway_animal)
      pathway_N <- compute_dairy_manure_pathway(man_nutrient, FRAC_pathway_animal)
    }
    else {
      
      calc_cols <- paste0('X', seq(1987,2017))
      pathway_N[, calc_cols] <- sapply(calc_cols, function(x) round(pathway_N[, x] * FRAC_pathway_animal[, x], 1))
    }
    
    export_file(module = 'Nutrients', 
                file = pathway_N, 
                filename = param, 
                folder = 'Gross_manure', 
                subfolder = nutrient, 
                subfolderX2 = pathway, 
                subfolderX3 = 'Total',
                subfolderX4 = main_param)
  }
}

loop_manure_allocation_pathway <- function() {
  
  pathway <- c('Grazing','Housing','Yards')
  nutrients <- c('N','P')
  
  for (i in nutrients) {
    
    for (j in pathway) {
      print(paste0('Nutrient: ', i, '/n Pathway: ', j))
      general_func_gross_manure_allocation(nutrient = i, pathway = j)
    }
  }
}


## DISAGGREGATE GROSS MANURE PER MANURE TYPE  ----------------------------------------------------------------------------------------------------------------------


general_func_disaggregate_gross_manure_type <- function(main_param, param, manure_type, pathway, nutrient) {
  
  FRAC_manure_type <- linearly_intrapolate_share_MMS(general_param = 'Share_MMS', param = manure_type)
  animal_row <- which(FRAC_manure_type[, 'Animals'] == param)
  FRAC_manure_type <- FRAC_manure_type[animal_row, paste0('X', seq(1987,2017))]
  
  pathway_maN <-  get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gross_manure', subfolder = nutrient, subfolderX2 = pathway, subfolderX3 ='Total', subfolderX4 = main_param, pattern = param)
  
  
  yrs <- paste0('X', seq(1987,2017))
  pathway_maN[, yrs] <- sapply(yrs, function(x) round(pathway_maN[,x] * FRAC_manure_type[,x], 1))
  
  return(pathway_maN)
  rm(list=c('FRAC_manure_type','animal_row','yrs'))
}


compute_disaggregate_gross_manure_type_pathway <- function(manure_type, pathway, nutrient) {
  
  FRAC_pathway <- linearly_intrapolate_share_MMS(general_param = 'Distribution', param = pathway)
  
  standard_params <- get_standard_params_list(main_param = 'Animals')
  
  for (i in 1:nrow(standard_params)) {
    
    main_param <- standard_params[i, 'Main_animals']
    param <- standard_params[i, 'Animals']
    print(param)
    
    pathway_maN_type <- general_func_disaggregate_gross_manure_type(main_param, param, manure_type, pathway, nutrient)
    export_file(module = 'Nutrients', 
                file = pathway_maN_type, 
                filename = param, 
                folder = 'Gross_manure', 
                subfolder = nutrient, 
                subfolderX2 = pathway, 
                subfolderX3 = manure_type,
                subfolderX4= main_param)
  }
}


loop_disaggregate_gross_manure_type <- function() {
  
  manure_type <- c('Solid','Slurry')
  pathway <- c('Housing')
  nutrient <- c('N','P')

  for (i in manure_type) {
    
    for (j in pathway) {
      
      for (z in nutrient) {
        
        compute_disaggregate_gross_manure_type_pathway(manure_type = i, pathway = j, nutrient = z)
      }
    }
  }
}



## CARBON IMPLEMENTATION ------------------------------------------------------------

compute_carbon_gross_manure <- function(pathway, manure_type) {
  
  CN_manure <-  get_activity_data(module = 'Nutrients', folder = 'Nutrient_params', subfolder = 'C', subfolderX2 = 'MMS', subfolderX3 = 'Excretion_coefficients', pattern = 'CN')
  
  standard_params <- get_standard_params_list(main_param = 'Animals')
  
  for (i in 1:nrow(standard_params)) {
    
    main_param <- standard_params[i, 'Main_animals']
    param <- standard_params[i, 'Animals']
    
    # find CN manure
    ifelse(pathway == 'Grazing',
      main_row <- which(CN_manure[,'Animals'] == param & CN_manure[, 'Manure_type'] == 'Solid'),
      main_row <- which(CN_manure[,'Animals'] == param & CN_manure[, 'Manure_type'] == manure_type))
      
    main_CN <- CN_manure[main_row, 'C_N']
    
    pathway_maN_type <-  get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gross_manure', subfolder = 'N', subfolderX2 = pathway, subfolderX3 =manure_type, subfolderX4 = main_param, pattern = param)
    
    yrs <- paste0('X', seq(1987,2017))
    pathway_maN_type[, yrs] <- sapply(yrs, function(x) round(pathway_maN_type[, x] * main_CN, 1))
    
    export_file(module = 'Nutrients', 
                file = pathway_maN_type, 
                filename = param, 
                folder = 'Gross_manure', 
                subfolder = 'C', 
                subfolderX2 = pathway, 
                subfolderX3 = manure_type,
                subfolderX4= main_param)
  }
}

loop_gross_manure_C_all_params <- function() {
  
  pathway <- c('Grazing','Housing','Yards')
  manure_type <- c('Slurry','Solid')
  
  for (i in pathway) {

    for (j in manure_type) {

      if (i == 'Grazing' & (j == 'Slurry' | j == 'Solid')) {
        compute_carbon_gross_manure(pathway = i, manure_type = 'Total')
      }
      else if (i == 'Yards' & (j == 'Slurry')) {
        next 
      }
      else if (i == 'Yards' & j == 'Solid') {
        compute_carbon_gross_manure(pathway = i, manure_type = 'Total')
      }
      else {
        compute_carbon_gross_manure(pathway = i, manure_type = j)
      }
    }
  }
}



compute_gross_manure_C_totals <- function(pathway) {
  # compute totals = SLURRY + SOLID
  

  standard_params <- get_standard_params_list(main_param = 'Animals')
  
  for (i in 1:nrow(standard_params)) {
    
    main_param <- standard_params[i, 'Main_animals']
    param <- standard_params[i, 'Animals']
    
    pathway_maN_slurry <- get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gross_manure', subfolder = 'C', subfolderX2 = pathway, subfolderX3 ='Slurry', subfolderX4 = main_param, pattern = param)
    pathway_maN_solid <- get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gross_manure', subfolder = 'C', subfolderX2 = pathway, subfolderX3 ='Solid', subfolderX4 = main_param, pattern = param)
    
    yrs <- paste0('X', seq(1987,2017))
    
    pathway_maN_slurry[, yrs] <- sapply(yrs, function(x) round( pathway_maN_slurry[, x] + pathway_maN_solid[,x], 1))
    
    export_file(module = 'Nutrients', 
                file = pathway_maN_slurry, 
                filename = param, 
                folder = 'Gross_manure', 
                subfolder = 'C', 
                subfolderX2 = pathway, 
                subfolderX3 = 'Total',
                subfolderX4= main_param)
  }
  #rm(List=c('manure_type','standard_params','main_param','param', 'pathway_maN_slurry','pathway_maN_solid'))
}


loop_gross_manure_C_totals <- function() {
  
  pathway <- c('Housing')
  sapply(pathway, function(x) compute_gross_manure_C_totals(pathway = x))
}




compute_gross_manure_Cexcretion <- function(main_param, param) {
  # computes the total C excretion for a given animal
  
  graz_maN <- get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gross_manure', subfolder = 'C', subfolderX2 = 'Grazing', subfolderX3 ='Total', subfolderX4 = main_param, pattern = param)
  housing_maN <- get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gross_manure', subfolder = 'C', subfolderX2 = 'Housing', subfolderX3 ='Total', subfolderX4 = main_param, pattern = param)
  yards_maN <- get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gross_manure', subfolder = 'C', subfolderX2 = 'Yards', subfolderX3 ='Total', subfolderX4 = main_param, pattern = param)
  
  yrs <- paste0('X', seq(1988,2017))
  
  graz_maN[, yrs] <- sapply(yrs, function(x) round(graz_maN[, x] + housing_maN[, x] + yards_maN[, x], 1))
  
  export_file(module = 'Nutrients', 
              file = graz_maN, 
              filename = param, 
              folder = 'Gross_manure', 
              subfolder = 'C', 
              subfolderX2 = 'Total_Cexcretion', 
              subfolderX3 = main_param)
  rm(list=c('graz_maN','housing_maN','yards_maN','yrs','graz_maN'))
}


loop_gross_manure_Cexcretion <- function() {
  
  standard_params <- get_standard_params_list(main_param = 'Animals')
  
  for (i in 1:nrow(standard_params)) {
    
    main_param <- standard_params[i, 'Main_animals']
    param <- standard_params[i, 'Animals']
    compute_gross_manure_Cexcretion(main_param = main_param, param = param)
  }
}


## TOTALS BY ANIMAL CATEGORY AND TOTALS -------------------------------------------------------

compute_total_nutrient_flows_main_param_pathway <- function(nutrient, pathway, manure_type) {
  
  standard_params <- get_standard_params_list(main_param = 'Animals')
  yrs <- paste0('X', seq(1987,2017))
  
  main_params <- unique(standard_params$Main_animals)
  
  store_main_param <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  store_main_param[, yrs] <- sapply(yrs, function(x) store_main_param[,x] <- 0)

  for (i in main_params) {
    
    animal_rows <- which(standard_params[, 'Main_animals'] == i)
    params <- standard_params[animal_rows, 'Animals']
    
    store_param <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
    store_param[, yrs] <- sapply(yrs, function(x) store_param[,x] <- 0)
    
    
    for (j in params) {
      
      # add each dataframe within each main_param in a list
      # calculate total sum for a given main_param

      man_N <- get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gross_manure', subfolder = nutrient, subfolderX2 = pathway, subfolderX3 =manure_type, subfolderX4 = i, pattern = j)
      store_param[, yrs] <- sapply(yrs, function(x) store_param[,x] <- man_N[, x] + store_param[,x])
    }
    # export sum of param within main_param
    export_file(module = 'Nutrients', 
                file = store_param, 
                filename = i, 
                folder = 'Gross_manure', 
                subfolder = nutrient, 
                subfolderX2 = pathway, 
                subfolderX3 = manure_type,
                subfolderX4= 'Total')
    
    # store sum of main_param in main_param_df
    store_main_param[, yrs] <- sapply(yrs, function(x) store_main_param[,x] <- store_main_param[, x] + store_param[,x])
  }
  # export sum of main_param
  export_file(module = 'Nutrients', 
              file = store_main_param, 
              filename = 'Total_sum', 
              folder = 'Gross_manure', 
              subfolder = nutrient, 
              subfolderX2 = pathway, 
              subfolderX3 = manure_type,
              subfolderX4= 'Total')
}

