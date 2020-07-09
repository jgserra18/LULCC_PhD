source('./Nutrients/Model/Crop_production/Compute_crop_nutrient_offtake.R')




## FORAGE BNF ---------------------------------------------------------

compute_forage_BNF <- function(nutrient = 'N',
                               main_param = 'Pastures', 
                               param = c('Intensive_pasture','Extensive_pasture')) {
  # Forage N fixation = Area * N_retention_kgNha-1 * N_fixation_coeff ,, 
  # where N_fixed_kgNha-1 == N_retention_kgNha-1 * N_fixation_coeff 
  # unit: kg N yr-1
  

  forage_params <- get_activity_data(module = 'Nutrients', folder = 'Nutrient_params', subfolder = nutrient, subfolderX2 = 'Crops', subfolderX3 = 'BNF', pattern = 'Forage')
  yrs <- paste0('X', seq(1987,2017))
  
  for (i in param) {
    
    crop_area <- get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = main_param, pattern = i)
    Nfixed_kgNha <- find_crop_variable(df = forage_params, param_col = 'Crop', param = i, var = 'N_fixed')
    
    crop_area[, yrs] <- sapply(yrs, function(x) round(crop_area[, x] * Nfixed_kgNha, 1))
    
    export_file(module = 'Nutrients', 
                file = crop_area, 
                filename = i, 
                folder = 'BNF', 
                subfolder = nutrient, 
                subfolderX2 = main_param)
  }
}




## GRAIN LEGUMES BNF ---------------------------------------------------------

# 1 - calculate DM production (tonnes dm) [done]
# 2 - Calculate aboveground biomass
# 3 - Calculate aboveground N content
# 4 - calculate root biomass and N content
# 5 - Calculate rhizodeposition
# sum steps 3, 4 and 5
# apply Ndfa


get_Pulses_BNF_params <- function(param, var) {
  
  pulses_params <- get_activity_data(module = 'Nutrients', folder = 'Nutrient_params', subfolder = 'N', subfolderX2 = 'Crops', subfolderX3 = 'BNF', pattern = 'Grain')
  param_df <-  find_crop_variable(df = pulses_params, param_col = 'Crop', param = param, var = var)
  
  return(param_df)
}

compute_pulses_aboveground_production <- function(main_param = 'Pulses', param) {
  # step 2: calculate aboveground biomass
  # AB_biomass = DMproduction / Harvest Index
  # unit: kg dm yr-1
  
  yrs <- paste0('X',seq(1987,2017))
  
  pulse_DMprod <- compute_crop_DM_production(main_param, param)
  pulse_DMprod[, yrs] <- sapply(yrs, function(x) round(pulse_DMprod[, x] * 1000, 1))
  pulses_HI <- get_Pulses_BNF_params(param = param, var = 'harvest_index')
  
  yrs <- paste0('X',seq(1987,2017))
  pulse_DMprod[, yrs] <- sapply(yrs, function(x) round ( pulse_DMprod[, x] / pulses_HI, 1))
  
  return(pulse_DMprod)
}

compute_pulses_aboveground_N <- function(main_param = 'Pulses', param) {
  # step 3: calculate abouground N content
  # AB_N = ( AB_biomass * protein_frac / protein_toN ) / HI_N
  # unit: kg N yr-1
  
  pulses_ABprod <- compute_pulses_aboveground_production(main_param, param)
  
  # params
  pulse_protein_frac <- get_Pulses_BNF_params(param = param, var = 'FRAC_grain_protein_content')
  pulse_proteiN_conversion <- get_Pulses_BNF_params(param = param, var = 'protein_to_N')
  pulse_HI_N <- get_Pulses_BNF_params(param = param, var = 'harvest_N_index')
  
  # computation
  yrs <- paste0('X',seq(1987,2017))
  pulses_ABprod[, yrs] <- sapply(yrs, function(x) round( (pulses_ABprod[, x] * pulse_protein_frac / pulse_proteiN_conversion) / pulse_HI_N  , 1))
  
  return(pulses_ABprod)
}


compute_pulses_belowground_production <- function(main_param = 'Pulses', param) {
  # step 4: calculate belowground biomass 
  # BG prod = AB prod * BG/AB ratio
  # unit: kg dm yr-1
  
  pulses_ABprod <- compute_pulses_aboveground_production(main_param, param)
  pulse_BG_frac <- get_Pulses_BNF_params(param = param, var = 'root_shoot_ratio')

  # computation
  yrs <- paste0('X',seq(1987,2017))
  pulses_ABprod[, yrs] <- sapply(yrs, function(x) round( pulses_ABprod[, x] * pulse_BG_frac, 1))

  return(pulses_ABprod)
}


compute_pulses_belowground_N <- function(main_param = 'Pulses', param) {
  # step 5: calculate belowground N 
  # BG N = BG prod * BG_Ncontent
  # unit: kg N yr-1
  
  pulses_BGprod <- compute_pulses_belowground_production(main_param, param)
  pulse_BG_N <- get_Pulses_BNF_params(param = param, var = 'FRAC_root_N_content')
  
  # computation
  yrs <- paste0('X',seq(1987,2017))
  pulses_BGprod[, yrs] <- sapply(yrs, function(x) round( pulses_BGprod[, x] * pulse_BG_N, 1))
  
  return(pulses_BGprod)
}


compute_pulses_rhizodepositio_N <- function(main_param = 'Pulses', param) {
  # step 6: calculate rhizodeposition 
  # Rhizo_N = (AB_N + BG_N) + rhizo_frac
  # unit: kg N yr-1
  
  pulses_AB_N <- compute_pulses_aboveground_N(main_param = 'Pulses', param)
  pulses_BG_N <- compute_pulses_belowground_N(main_param = 'Pulses', param)
  pulses_rhizo_frac <- get_Pulses_BNF_params(param = param, var = 'FRAC_rhizodeposition')
  
  # computationtotal N biomass
  yrs <- paste0('X',seq(1987,2017))
  pulses_AB_N[, yrs] <- sapply(yrs, function(x) round( pulses_AB_N[, x] + pulses_BG_N[, x], 1))

  # calculate rhizodeposition 
  pulses_AB_N[, yrs] <- sapply(yrs, function(x) round( pulses_AB_N[, x] * pulses_rhizo_frac, 1))
  
  return(pulses_AB_N)
  rm(list=c('pulses_BG_N', 'pulses_rhizo_frac'))
}


compute_pulses_total_Nprod <- function(main_param = 'Pulses', param) {
  # step 7: calculate total N from the pulse
  # total N = AB_N + BG_N + rhizo_N
  # unit: kg N yr-1
  
  pulses_rhizo_N <- compute_pulses_rhizodepositio_N(main_param, param)
  pulses_AB_N <- compute_pulses_aboveground_N(main_param, param)
  pulses_BG_N <- compute_pulses_belowground_N(main_param, param)
  
  total <- pulses_AB_N
  # compute total pulse N production
  yrs <- paste0('X',seq(1987,2017))

  total[, yrs] <- sapply(yrs, function(x) round(pulses_AB_N[,x] + pulses_rhizo_N[, x] + pulses_BG_N[, x], 1))
  
  return(total)
  rm(list=c('pulses_rhizo_N', 'pulses_AB_N', 'pulses_BG_N'))
}


compute_pulses_Nfixation <- function(main_param = 'Pulses', param) {
  # step 8: calculate biological N fixation from legumes
  # BNF = total N * Ndfa
  # unit: kg N yr-1
  
  pulse_total_N <- compute_pulses_total_Nprod(main_param, param)
  pulse_Nfda <- get_Pulses_BNF_params(param = param, var = 'FRAC_Ndfa')
  
  yrs <- paste0('X',seq(1987,2017))
  
  pulse_total_N[, yrs] <- sapply(yrs, function(x) round( pulse_total_N[, x] * pulse_Nfda, 1))
  
  export_file(module = 'Nutrients', 
              file = pulse_total_N, 
              filename = param, 
              folder = 'BNF', 
              subfolder = 'N', 
              subfolderX2 = main_param)
}

## TOTAL BNF  ---------------------------------------------------------


compute_all_BNF <- function() {
  
  standard_params <- get_standard_params_list(main_param = 'Crops')
  standard_params <- standard_params[which(standard_params[, 'Main_crop'] == 'Pulses' | standard_params[, 'Main_crop'] == 'Pastures'), ]
  
  for (i in 1:nrow(standard_params)) {
    
    main_param <- standard_params[i, 'Main_crop']
    param <- standard_params[i, 'Crop']
    
    if (main_param == 'Pastures') {
      
      compute_forage_BNF()
    }
    else {
      compute_pulses_Nfixation(param = param)
    }
  }
}


compute_total_BFN <- function() {
  
  standard_params <- get_standard_params_list(main_param = 'Crops')
  standard_params <- standard_params[which(standard_params[, 'Main_crop'] == 'Pulses' | standard_params[, 'Main_crop'] == 'Pastures'), ]
  
  # create store df
  yrs <- paste0('X', seq(1987,2017))
  store_param <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  store_param[, yrs] <- sapply(yrs, function(x) store_param[,x] <- 0)
  
  
  for (i in 1:nrow(standard_params)) {
    
    main_param <- standard_params[i, 'Main_crop']
    param <- standard_params[i, 'Crop']
    
    crop_BNF <- get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'BNF', subfolder = 'N', subfolderX2 = main_param,  pattern = param)
    store_param[, yrs] <- sapply(yrs, function(x) round(store_param[,x] + crop_BNF[, x], 1))
  }
  export_file(module = 'Nutrients', 
              file = store_param, 
              filename = 'Total_sum', 
              folder = 'BNF', 
              subfolder = 'N', 
              subfolderX2 = 'Total')
}


compute_BNF_in_arableLand = function() {
  
  standard_params <- get_standard_params_list(main_param = 'Crops')
  standard_params <- standard_params[which(standard_params[, 'Main_crop'] == 'Pulses' | standard_params[, 'Main_crop'] == 'Pastures'), ]
  
  # create store df
  yrs <- paste0('X', seq(1987,2017))
  BNF_arable <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  BNF_arable[, yrs] <- sapply(yrs, function(x) BNF_arable[,x] <- 0)
  
  
  for (i in 1:nrow(standard_params)) {
    
    main_param <- standard_params[i, 'Main_crop']
    param <- standard_params[i, 'Crop']
    
    if (param == 'Extensive_pasture') { next }
    else {
      crop_BNF <- get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'BNF', subfolder = 'N', subfolderX2 = main_param,  pattern = param)
      BNF_arable[, yrs] <- sapply(yrs, function(x) round(BNF_arable[,x] + crop_BNF[, x], 1))
    }
  } 
  
  return(BNF_arable)
  rm(list='crop_BNF')
}
