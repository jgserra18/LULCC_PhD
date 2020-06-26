source('./Nutrients/Model/Fodder_production/Support/Get_energy_requirement_params.R')
source('./Nutrients/Model/Fodder_production/1_Energy_requirements.R')


#* GLEAM ASSUMPTIONS (Table 4.14)
#* cohorts - table 2.1
#* Nut retention is assumed to be 0 for:
#* --> Adult males for ruminants, pigs


get_animal_subclass = function(main_param) {
  
  if (main_param == 'Goats') {
    
    standard_params = c('Goats','Doeling','Buck')
  }
  else if (main_param == 'Sheep') {
    
    standard_params = c('Ewes_dairy','Ewes_other','Ram')
  }
  else {
    
    standard_params <- get_standard_params_list('Animals')
    standard_params = subset(standard_params, Main_animals == main_param)[, 'Animals']
  }
  return(standard_params)
}


modifier_get_animal_population = function(main_param, param) {
  
  
  if (main_param == 'Sheep' | main_param == 'Goats') {
    
    pop = get_activity_data(module = 'Nutrients', mainfolder = 'Activity_data', folder = 'General_params', subfolder = 'Animals', subfolderX2 = 'Diet', subfolderX3 = 'Ruminants', subfolderX4 = 'Population', subfolderX5 = main_param, pattern = param)
  }
  else {
    
    pop = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Animals', subfolderX2 = main_param, pattern = param)
  }
  return(pop)
}


# RUMINANTS N RETENTION ----------------------------------------------------------------------------------


is_dairy = function(main_param, param, goats_dairy=FALSE) {
  # checks if param is a dairy animal
  # returns TRUE or FALSE
  
  if (param == 'Dairy_cows' | param == 'Ewes_dairy' | (param == 'Goats' & goats_dairy == TRUE)) {
    
    return('TRUE')
  }
  else {
    return('FALSE')
  }
}



is_adult = function(main_param, param, goats_dairy) {
  
  if (param == 'Male_calf_2' | param == 'Non_dairy' | param == 'Buck' | param == 'Ran' | (param == 'Goats' & goats_dairy == FALSE)) {
    
    return('TRUE')
  }
  else {
    return('FALSE')
  }
}




set_Ruminants_param = function(main_param, param, goats_dairy = FALSE, animal_class = 'Ruminants') {
  # sets the parameters necessary to compute N retention for large and small ruminants, distinguishing dairy and non dairy animals
  
  if (param == 'Dairy_cows') {
    
    milk_hd = compute_linear_extrapolation_milkPerCow_historical()
    milk_hd = convert_dairy_Nex_NUTS2_municipality(milk_hd)
    names(milk_hd)[-seq(1,3)] = paste0('X', names(milk_hd)[-seq(1,3)])
    
    FRAC_protein = get_energy_requirement_params(animal_class, 'Nutrient_retention', 'Prot_Dairy_cows')
    Ckg = get_energy_requirement_params(animal_class, 'Maintenance', 'Weight', 'Female_calf-1')[, 'Weight']
    NE_gro = compute_net_energy_growth_largeRuminants(main_param, param = 'Female_calf_1-2', animal_class)
    DWG = get_energy_requirement_params(animal_class,'Growth', 'DWG', 'Female_calf_1-2')[, 'DWG']
    
    return(list(milk_hd = milk_hd, FRAC_protein = FRAC_protein, Ckg = Ckg, NE_gro = NE_gro, DWG = DWG))
  }
  
  else if (param == 'Ewes_dairy' | ( param == 'Goats' & goats_dairy == TRUE)) {
    
    youngling_param = ifelse(param == 'Ewes_dairy', 'Ewes_other', 'Doeling')
    
    milk_hd = compute_linearl_extrapolation_milk_perSheepGoat(main_param, param)
    FRAC_protein = ifelse(param == 'Ewes_dairy', 5.4, 3.1)
    DWG = get_energy_requirement_params(animal_class,'Growth', 'DWG', youngling_param)[, 'DWG']
    Ckg = get_energy_requirement_params(animal_class, 'Maintenance', 'Weight', youngling_param)[, 'Weight']
    NE_gro = compute_net_energy_growth_smallRuminants(main_param, param = youngling_param, animal_class)
    
    return(list(milk_hd = milk_hd, FRAC_protein = FRAC_protein, Ckg = Ckg, NE_gro = NE_gro, DWG = DWG))
  }
  
  else if ( param == 'Male_calf_1-2' | param == 'Non_dairy' | 
           param == 'Other_calf' | param == 'Female_calf_1-2' |
           param == 'Female_calf-1' | param == 'Beef_calf' | param == 'Female_calf_2') {
    
    DWG = get_energy_requirement_params(animal_class,'Growth', 'DWG', param)[, 'DWG']
    NE_gro = compute_net_energy_growth_largeRuminants(main_param, param, animal_class)
    
    return(list(DWG=DWG, NE_gro=NE_gro))
  }
  
  else if ( (param == 'Goats' & goats_dairy == FALSE) | 
            param == 'Doeling' | param == 'Ewes_other') {
    
    DWG = get_energy_requirement_params(animal_class,'Growth', 'DWG', param)[, 'DWG']
    NE_gro = compute_net_energy_growth_smallRuminants(main_param, param, animal_class)
    
    return(list(DWG=DWG, NE_gro=NE_gro))
  }
}


compute_dairy_ruminants_Nretention = function(main_param, param, goats_dairy = FALSE, animal_class='Ruminants') {
  # compute the N retention in dairy ruminants according to GLEAMS
  # unit: kg N head-1 yr-1
  
  if (param != 'Dairy_cows' & param != 'Ewes_dairy' & (param != 'Goats' & goats_dairy == FALSE)) {
    stop('This only applies to dairy cows.')
  }
  else {
    
    set_params = set_Ruminants_param(main_param, param, goats_dairy)
    
    milk_hd = set_params[['milk_hd']]
    FRAC_protein = set_params[['FRAC_protein']]
    Ckg = set_params[['Ckg']]
    NE_gro = set_params[['NE_gro']]
    DWG = set_params[['DWG']]
    
    yrs = paste0('X', seq(1987,2017))
    
    N_retention = milk_hd
    
    # species-specific equation
        if (param == 'Dairy_cows') {
          
          N_retention[, yrs] = sapply(yrs, function(x) round(
            ((milk_hd[, x]/365 * (FRAC_protein[, x] /100)/ 6.38) + 
              (Ckg/365 * (268- (7.03 * NE_gro/DWG))) * 
              0.001 / 6.25) * 365
            , 1))
        }
        else {
          
          N_retention[, yrs] = sapply(yrs, function(x) round(
            ((milk_hd[, x]/365 * (FRAC_protein/100)/ 6.38) + 
              (Ckg/365 * (268- (7.03 * NE_gro/DWG))) * 
              0.001 / 6.25) * 365
            , 1))
        }
    
    
    return(N_retention)
    rm(list=c('FRAC_protein','milk_per_cow','Ckg','DWG','NE_gro'))
  }
}


compute_other_ruminants_Nretention = function(main_param, param, goats_dairy = FALSE, animal_class='Ruminants') {
  # compute the N retention for non-dairy ruminants
  # Nretention is set to 0 for adult males
  # unit: kg N head-1 yr-1
  
  check = is_adult(main_param, param, goats_dairy)
  
  
  yrs = paste0('X', seq(1987,2017))
  N_retention_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  
  if (check == 'TRUE') {
    
    N_retention_df[, yrs] <- sapply(yrs, function(x) N_retention_df[,x] = 0)
  }
  else {
    
    set_params = set_Ruminants_param(main_param, param, goats_dairy)
    
    NE_gro = set_params[['NE_gro']]
    DWG = set_params[['DWG']]
    
    N_retention = round( (DWG * (268 - (7.03 * NE_gro/DWG)) * 0.001/6.25) * 365, 1)
    N_retention_df[, yrs] <- sapply(yrs, function(x) N_retention_df[,x] = N_retention)
    N_retention_df = data_cleaning(N_retention_df)
  }
  
  return(N_retention_df)
}




compute_goats_Nretention = function(animal_pop_df) {
  # compute the N retained in dairy and non_dairy goats
  # assumption: 10% of goats population is dairy
  # unit: kg N yr-1
  
  dairy_goats = animal_pop_df
  non_dairy_goats = animal_pop_df
  
  yrs = paste0('X', seq(1987,2017))
  
  # compute goat populations -------------------------------------------------------------
  dairy_goats[, yrs] = sapply(yrs, function(x) round(animal_pop_df[, x] * 0.1, 0))
  non_dairy_goats[, yrs] = sapply(yrs, function(x) round(animal_pop_df[, x] * 0.9, 0))
  
  # call Nretention (kg N head-1 yr-1)'
  dairy_Nret = compute_dairy_ruminants_Nretention(main_param = 'Goats', param = 'Goats', goats_dairy = TRUE)
  non_dairy_Nret = 0 
  
  # compute total N retained per yr-1
  dairy_goats[, yrs] = sapply(yrs, function(x) round(dairy_Nret[, x] * dairy_goats[,x], 1))
  non_dairy_goats[, yrs] = sapply(yrs, function(x) round(non_dairy_Nret * non_dairy_goats[,x], 1))
  
  return(list(dairy_Nret = dairy_goats, non_dairy_Nret = non_dairy_goats))
  rm(list=c('dairy_goats','non_dairy_goats'))
}


general_func_N_retained = function(main_param, param,  goats_dairy = FALSE) {
  # general function to compute the N retained in ruminants
  # unit: kg N yr-1
  
  pop = modifier_get_animal_population(main_param, param)

  if (param == 'Goats') {
    
    if (goats_dairy == TRUE) { N_retained = compute_goats_Nretention(pop)[[1]] } else { N_retained = compute_goats_Nretention(pop)[[2]]  }
  }
  else {
    
    check = is_dairy(main_param, param)

    if (check == 'TRUE') {
      
      N_retention = compute_dairy_cow_Nretention(main_param, param)
    }
    else {
      N_retention = compute_other_ruminants_Nretention(main_param, param)
    }
    
    yrs = paste0('X',seq(1987,2017))
    N_retained = pop
    N_retained[, yrs] = sapply(yrs, function(x) round(N_retention[, x] * pop[,x], 1))
  }
  
  return(N_retained)
}



compute_all_ruminants_Nretention = function() {
  
  ruminants = c('Bovine','Sheep','Goats')
  
  for (ruminant in ruminants) {
    
    animals = get_animal_subclass(ruminant)
    
    for (animal in animals) {

      if (animal == 'Goats') {
      
        # N retention for non_dairy goats is set to 0
        N_retained = general_func_N_retained(ruminant, animal, TRUE)
      }
      else {
        N_retained = general_func_N_retained(ruminant, animal)
      }

      export_file(module = 'Nutrients', 
                  file = N_retained, 
                  filename = animal, 
                  folder = 'Fodder_production', 
                  subfolder = 'N_retention', 
                  subfolderX2 = 'N',
                  subfolderX3 = ruminant)
    }
  }
}



# PIGS N RETENTION --------------------------------------------------------------------------


set_pigs_Nretention_params = function(main_param, param, management, female) {
  
  
  if (management == 'Grazing') {
    
        if (param == 'Pregnant_sows' | param == 'Non_pregnant_sows') {
          
          FR = 1.6 # parturition sow-1 yr-1
          LITSIZE = 11.5 # heads yr-1
          Ckg = 1.2 #kg
          Wkg = 34 #kg
          DWG_fat = 0.66 #kg head-1 day-1
          
          params = list(FR = FR, LITSIZE = LITSIZE, Ckg = Ckg,  Wkg = Wkg, DWG_fat = DWG_fat)
        }
        else if ((param == 'Other_swine' | param == 'Pigs_20' | param == 'Pigs_50') & female == TRUE) {
          
          FR = 1.6 # parturition sow-1 yr-1
          LITSIZE = 11.5 # heads yr-1
          Ckg = 1 #kg
          AFCF = 50 #days
          Wkg = 6 #kg
          DWG_fat = 0.4 #kg head-1 day-1
          
          params = list(FR = FR, LITSIZE = LITSIZE, Ckg = Ckg, AFCF = AFCF, Wkg = Wkg, DWG_fat = DWG_fat)
        }
        else {
          
          DWG_fat = 0.66
          
          params = list(DWG_fat = DWG_fat)
        }
  }
  
  else if (management == 'Industrial') {
    
        if (param == 'Pregnant_sows' | param == 'Non_pregnant_sows') {
          
          FR = 2.3 # parturition sow-1 yr-1
          LITSIZE = 9 # heads yr-1
          Ckg = 1.2 #kg
          Wkg = 7 #kg
          DWG_fat = 0.66 #kg head-1 day-1
          
          params = list(FR = FR, LITSIZE = LITSIZE, Ckg = Ckg, Wkg = Wkg, DWG_fat = DWG_fat)
        }
        else if ((param == 'Other_swine' | param == 'Pigs_20' | param == 'Pigs_50') & female == TRUE) {
          
          FR = 2.3 # parturition sow-1 yr-1
          LITSIZE = 9 # heads yr-1
          Ckg = 1.2 #kg
          AFCF = 34 #days
          Wkg = 7 #kg
          DWG_fat = 0.66 #kg head-1 day-1
          
          params = list(FR = FR, LITSIZE = LITSIZE, Ckg = Ckg, AFCF = AFCF, Wkg = Wkg, DWG_fat = DWG_fat)
        }
        else {
          
          DWG_fat = 0.66
          
          params = list(DWG_fat = DWG_fat)
        }
  }
  return(params)
}




d = compute_pigs_Nretention('Pigs','Other_swine','Industrial', T)
d
compute_pigs_Nretention = function(main_param, param, management, female=FALSE) {
  # computes the N retention for pigs according to the management
  #* POPULATION CATEGORY ALLOCATION
  #* Sows --> Af
  #* Other_swine --> 20 kg < pigs < 50 kg --> 0.50 are Rf, 0.50 are others
  #* Pigs_50 --> 50% female, 50% male (Nret=0)
  #* Pigs_20 --> 0.50 are Rf, 0.50 are others
  
  # unit: kg N hd-1 yr-1
  
  params = set_pigs_Nretention_params(main_param, param, management, female)
  
  if (param == 'Pregnant_sows' | param == 'Non_pregnant_sows') {
    
    FR = params[['FR']]
    LITSIZE = params[['LITSIZE']]
    Ckg = params[['Ckg']]
    Wkg = params[['Wkg']]
    DWG_fat = params[['DWG_fat']]
    
    Nretention = round(
      ((0.025 * LITSIZE * FR * (Wkg - Ckg)/0.98) + (0.025 * LITSIZE * FR * Ckg)), 2)
  }
  
  else if ((param == 'Other_swine' | param == 'Pigs_20' | param == 'Pigs_50') & female == TRUE) {
    
    FR = params[['FR']]
    LITSIZE = params[['LITSIZE']]
    Ckg = params[['Ckg']]
    Wkg = params[['Wkg']]
    DWG_fat = params[['DWG_fat']]
    AFCF = params[['AFCF']]
    
    Nretention = round(
      0.025 * DWG_fat * AFCF * (((0.025 * LITSIZE * FR * (Wkg - Ckg) / 0.98) + (0.025 * LITSIZE * FR * Ckg))), 2)
  }
  
  else if (param == 'Pigs_50' & female == FALSE) {
    
    Nretention = 0
  }
  
  else {
    
    DWG_fat =  params[['DWG_fat']]
    Nretention = round(DWG_fat * 0.025 * 365 ,2)
  }
  
  return(Nretention)
}






# CHICKENS  N RETENTION --------------------------------------------------------------------------


compute_broilers_DWG = function(main_param, param, management) {
  # computes theaverage daily gain of broilers
  # unit: kg dm-1 yr-1
  
  if (param != 'Broilers') {
    stop('Only for broilers!')
  }
  else {
    
      if (management == 'Grazing') {
      
      chick_wght_birth = 0.04 # kg 
      age_slaughter = 735 # days 
      weight_slaughter = compute_linear_extrapolation_broilers_slaughter_weight()
      DWG[, yrs] = sapply(yrs, function(x) round(weight_slaughter[,x] * chick_wght_birth * age_slaughter, 1))
    }
    
    else {
      
      chick_wght_birth = 0.05 # kg 
      weight_slaughter = compute_linear_extrapolation_broilers_slaughter_weight()
      age_slaughter = 44 # days 
      
      yrs = paste0('X', seq(1987,2017))
      DWG = weight_slaughter
      DWG[, yrs] = sapply(yrs, function(x) round(weight_slaughter[,x] * chick_wght_birth * age_slaughter, 1))
    }
    
    return(DWG)
    rm(list=c('chick_wght_birth','age_slaughter','weight_slaughter'))
  }
}



compute_hens_DWG = function(main_param, param) {
  
  
}





compute_laying_hens_Nretention = function(main_param, param, animal_class = 'Chicken') {
  # AF - adult females, repdocution (laying_hens)
  # RF = 0
  
  if (param != 'Rep_hens' | param != 'Laying_hens') {
    stop('Rep_hens or Laying_hens here!')
  }
  else {
    
    N_lw = 0.028 # kg N kg hd-1
    N_egg = 0.0185 # kg N kg egg-1
    EGG = compute_linear_extrapolation_eggs_perLayingHens(main_param, param)
    DWG = get_energy_requirement_params(animal_class, 'Growth', 'DWG',  younglin_param)[, 'DWG']
    
    yrs = paste0('X', seq(1987,2017))
    N_retention = EGG
    N_retention[, yrs] = sapply(yrs, function(x) round(
      N_lw * 
    ))
    
  }
}



