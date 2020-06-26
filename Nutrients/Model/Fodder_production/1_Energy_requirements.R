source('./Main/Global_functions.R')
source('./Main/Data_operations.R')
source('./Nutrients/Model/Fodder_production/Support/Get_energy_requirement_params.R')


## PARAM SIMPLIFIED GETTERS -------------------------------------------------------------------------------


get_energy_requirement_params = function(animal_class, stage, pattern, param) {
  # animal_class is Ruminants, Pigs, Chickens
  # stage is e.g., Energy or Growth
  # to shorten a bit get_activity-data
  
  param_df = get_activity_data(module = 'Nutrients', mainfolder = 'Activity_data', folder = 'General_params', subfolder = 'Animals', subfolderX2 = 'Diet', subfolderX3 = animal_class, subfolderX4 = stage, pattern = pattern)
  
  if (missing(param) == TRUE) {
    return(param_df)
  }
  else {
    
    param_df = param_df[which(param_df[, 'Animals'] == param), ]
    return(param_df)
  }
}



## RUMINANT ENERGY REQUIREMENTS ---------------------------------------------------------------------------


# energy for maintenance -----------
compute_net_energy_maintenance = function(param, animal_class = 'Ruminants') {
  # NE_main = C_main * weight^0.75
  # unit: Mj head-1 yr-1
  
  C_main = get_energy_requirement_params(animal_class, 'Maintenance', 'C_main', param)[, 'Cmain']
  animal_weight = get_energy_requirement_params(animal_class, 'Maintenance', 'Weight', param)[, 'Weight']
  
  NE_main = C_main * animal_weight^0.75
  
  return(NE_main)
}


# energy for activity -----------
compute_net_energy_activity = function(main_param, param, management, animal_class = 'Ruminants') {
  # NE_act = C_act * NE_main
  # unit: Mj head-1 day-1 
  
  NE_main = compute_net_energy_maintenance(param, animal_class)
  C_act = get_energy_requirement_params(animal_class, 'Energy', 'C_act_muni')
  
  # housing / yards params 
  if (main_param == 'Bovine' & (management == 'Yards' | management == 'Housing')) {
    
    select_col = 'House_largeRuminants'
  }
  else if (main_param == 'Sheep' | main_param == 'Goats' & (management == 'Yards' | management == 'Housing')) {
    
    select_col = 'House_smallRuminants'
  }
  # grazing params 
  else if (main_param == 'Bovine' & management == 'Grazing') {
    
    select_col = 'Pasture_largeRuminants'
  }
  else if (main_param == 'Sheep' | main_param == 'Goats' & management == 'Grazing') {
    
    select_col = 'Pasture_smallRuminants'
  }
  # select appropriate C_act according to management
  C_act = C_act[, c('Muni_ID','ID','Muni', select_col)]
  
  # compute NE_act
  NE_act = C_act
  NE_act[, select_col] = C_act[, select_col] * NE_main
  names(NE_act)[ncol(NE_act)] = 'NE_act'
  return(NE_act)
}





# energy for growth ------------------------------------------------------------------------


compute_net_energy_growth_largeRuminants = function(main_param, param, animal_class = 'Ruminants') {
  # set to 0 for older animals
  # for sheeps and goats this was excluded as lambs/goat kids were excluded
  # calculates net energy needed for growth (ie, weight gain)
  # only for younglings
  # unit: Mj head-1 day-1
  
  if (main_param != 'Bovine') {
    stop('Only for bovine.')
  }
  else {
  
      if (main_param == 'Bovine' & (param == 'Male_calf_1-2' | param == 'Other_calf' | param == 'Beef_calf' | param == 'Female_calf_1-2' | param == 'Female_calf-1')) {
        
        # get params
        LW = get_energy_requirement_params(animal_class, 'Maintenance', 'Weight', param)[, 'Weight']
        DWG =  get_energy_requirement_params(animal_class, 'Growth', 'DWG', param)[, 'DWG']
        C_gro =  get_energy_requirement_params(animal_class, 'Growth', 'C_gro', param)[, 'C']
        
        # is param female or animal? calculate net energy for growth accordingly 
    
        Akg = ifelse(param == 'Male_calf_1-2' | param == 'Other_calf' | param == 'Beef_calf',
           get_energy_requirement_params(animal_class, 'Maintenance', 'Weight', 'Male_calf_2')[, 'Weight'],
           get_energy_requirement_params(animal_class, 'Maintenance', 'Weight', 'Female_calf_2')[, 'Weight'])
        
            if (param == 'Male_calf_2' | param == 'Female_calf_2' | param == 'Non_dairy') {
              
              NE_gro = 0
            }
            else {
              # calculate NE_gro
              NE_gro = 22.02 * (LW / (C_gro * Akg))^0.75 * DWG^1.097
            }
      }
    
    else if (param == 'Male_calf_2' | param == 'Female_calf_2' | param == 'Non_dairy') {
      
      NE_gro = 0
    }
  }
  return(NE_gro)
}


compute_net_energy_growth_smallRuminants = function(main_param, param, animal_class = 'Ruminants') {
  # unit: Mj head-1 yr-1
  
  if (main_param == 'Sheep' | main_param == 'Goats' & ( param=='Doeling'| param=='Ewes_other')) {
    
    Ckg = ifelse(main_param == 'Sheep',
                 get_energy_requirement_params(animal_class, 'Maintenance', 'Weight', 'Doeling')[, 'Weight'],
                 get_energy_requirement_params(animal_class, 'Maintenance', 'Weight', 'Ewes_other')[, 'Weight'])
    DWG =  get_energy_requirement_params(animal_class, 'Growth', 'DWG', param)[, 'DWG']
    a =  get_energy_requirement_params(animal_class, 'Growth', 'C_gro', param)[, 'a']
    b =  get_energy_requirement_params(animal_class, 'Growth', 'C_gro', param)[, 'b']
    
    NE_gro = DWG * ( a + b * Ckg ) + 0.5 * b * DWG^2
  }
  else if (param =='Ram' | param == 'Buck' | param == 'Ewes_dairy' | param == 'Goats') {
    
    NE_gro = 0
  }
  else {
    stop('What? See this.')
  }
  return(NE_gro)
}



# energy for milk production -----------------------------------

compute_net_energy_milk = function(main_param, param, animal_class = 'Ruminants') {
  # computes energy for milk production
  # only dairy animals (10% goats, ewes_dairy, dairy_cows)
  # unit: Mj head-1 yr-1
  
  yrs = paste0('X', seq(1987,2017))
  
  if (main_param == 'Bovine' & param == 'Dairy_cows') {
    
    milk_per_cow = compute_linear_extrapolation_milkPerCow_historical()
    fat =  get_energy_requirement_params(animal_class, 'Milk', 'Dairy_cows')
    
    NE_lact = fat
    NE_lact[, yrs] = sapply(yrs, function(x) round( milk_per_cow[, x] * (1.47 + 0.40 * fat[,x]/100), 1))
  }
  
  else if ((main_param == 'Sheep' & param == 'Ewes_dairy') | (main_param == 'Goats' & param == 'Goats')) {
    # adjust EV_milk (7%) of 4.6 Mj kg milk-1 accordingly
    
    EV_milk = 4.6 # Mj kg milk-1 
    milk =  compute_linearl_extrapolation_milk_perSheepGoat(main_param, param)
    fat =  get_energy_requirement_params(animal_class, 'Milk', paste0('Fat_', main_param))
    
    NE_lact = fat
    NE_lact[, yrs] = sapply(yrs, function(x) round( milk[, x] * (fat[,x]/100 * EV_milk / 0.07), 1))
  }
  else {
    stop('What?')
  }
  
  return(NE_lact)
}


# energy for fibre ----------------------------------------------

compute_net_energy_fibre = function(main_param, param, animal_class='Ruminants') {
  # unit: Mk head-1 day-1
  
  if (main_param != 'Sheep') {
    stop('Main_param must be Sheep.')
  }
  else {
    
    wool_head = compute_linear_extrapolation_woolPerSheep(main_param)
    EV_fibre = 24 # Mj kg fibre-1
    
    NE_fibre = wool_head
    yrs = paste0('X', seq(1987,2017))
    NE_fibre[, yrs] = sapply(yrs, function(x) round( EV_fibre * wool_head[,x], 1))
    
    return(NE_fibre)
    rm(list=c('wool_head','EV_fibre'))
  }
}


# energy for pregnancy --------------------------

compute_net_energy_pregnancy_largeRuminants = function(main_param, param, animal_class='Ruminants') {
  
  if (main_param == 'Bovine') {
    stop('Main_param is Bovine.')
  }
  else {
    
    NE_main = compute_net_energy_maintenance(param)
    AFC = get_energy_requirement_params(animal_class, 'Pregnancy', 'AFC', param)[, 'AFC']
    FR = get_energy_requirement_params(animal_class, 'Pregnancy', 'Fertility_rate', param)[, 'FR']
    
    
  }
}







