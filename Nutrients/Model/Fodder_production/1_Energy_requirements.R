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
    
         if (param == 'Male_calf_1-2' | param == 'Other_calf' | param == 'Beef_calf') {
           Akg = get_energy_requirement_params(animal_class, 'Maintenance', 'Weight', 'Male_calf_2')[, 'Weight']
           
         }
        else {
          Akg = get_energy_requirement_params(animal_class, 'Maintenance', 'Weight', 'Female_calf_2')[, 'Weight']
        }
        
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
  # unit: Mj head-1 day-1
  
  yrs = paste0('X', seq(1987,2017))
  
  if (main_param == 'Bovine' & param == 'Dairy_cows') {
    
    DIM = 271
    milk_per_cow = compute_linear_extrapolation_milkPerCow_historical()
    milk_per_cow = convert_dairy_Nex_NUTS2_municipality(milk_per_cow)
    names(milk_per_cow)[4:ncol(milk_per_cow)] = paste0('X', names(milk_per_cow)[4:ncol(milk_per_cow)])
    
    fat =  get_energy_requirement_params(animal_class, 'Milk', 'Dairy_cows')
        
    NE_lact = milk_per_cow
    NE_lact[, yrs] = sapply(yrs, function(x) round( (milk_per_cow[, x]/271) * (1.47 + 0.40 * fat[,x]), 1))
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
  # unit: Mk head-1 day-1
  
  if (!(main_param=='Bovine')) {
    stop('Main_param is Bovine.')
  }
  else {
    
    NE_main = compute_net_energy_maintenance(param)
    FR = get_energy_requirement_params(animal_class, 'Pregnancy', 'Fertility_rate', param)[, 'FR']
    
    NE_preg = NE_main * 0.1 * FR 
    return(NE_preg)
  }
}


# ration energy in the feed intake for maintenance to digestible energy (REM)

compute_net_energy_REM = function(main_param, param, anima_class = 'Ruminants') {
  # unit: %
  
  if (param == 'Dairy_cows') {
    
    DIET_FE = 73 # %
  }
  else {
    stop('Not implemented yet.')
  }
  REM = round(1.123 - (4.092*0.001 * DIET_FE) + (1.126*10e-5 * (DIET_FE)^2) - (25.4/(DIET_FE)), 3)
  
  return(REM)
}


compute_net_energy_REG = function(main_param, param, anima_class = 'Ruminants') {
  
  
  if (param == 'Dairy_cows') {
    
    DIET_FE = 73 # %
  }
  else {
    stop('Not implemented yet.')
  }
  REG = round(1.164 - (5.160*0.001 * DIET_FE) + (1.308*1e-5 * (DIET_FE)^2) - (37.4/(DIET_FE)), 3)
  
  return(REG)
}




# total gross energy and total DMI ----------------------------------------------------------------------------------------------

compute_total_gross_energy_GE = function(main_param, param, animal_class = 'Ruminants') {
  # Eq 3.42 GLEAMS
  # gross energy requirements
  # unit: Mj head-1 day-1
  
  if (param == 'Dairy_cows') {
    
    yrs = paste0('X', seq(1987,2017))
    
    NE_main = compute_net_energy_maintenance(param, animal_class)
    #NE_act = compute_net_energy_activity( main_param, param, 'Housing', animal_class)
    NE_lact = compute_net_energy_milk(main_param, param, animal_class)
    NE_preg = compute_net_energy_pregnancy_largeRuminants(main_param, param, animal_class)
    REM = compute_net_energy_REM(main_param, param, animal_class)
    FE = 73
    
    pop = get_activity_data(module = 'Nutrients', subfolder = 'Correct_data_Municipality', subfolderX2 = 'Animals', subfolderX3 = main_param, pattern = param)
    pop[, yrs] = sapply(yrs, function(x) ifelse(pop[,x]>1, 1, pop[,x]))
    
    GE_tot = NE_lact
    GE_tot[,yrs] = sapply(yrs, function (x) ((NE_lact[, x] + NE_preg + NE_main + 0)/REM) * pop[,x])

    return(GE_tot)
    rm(list=c('NE_main','NE_lact','NE_preg','REM','FE'))
  }
  else {
    stop('Not implemented yet.')
  }
}



compute_total_DMI = function(main_param, param, animal_class = 'Ruminants') {
  # total dry matter intake based on gross eneergy requirements and the average diet based on NORFOR and Serra et al 2019 feed specifications
  # unit: kg DM head-1 day-1
  
  if (param == 'Dairy_cows') {
    
    DIET = 7.9 # average Mj kg DM-1 based on NorFor
    GE = compute_total_gross_energy_GE(main_param, param, animal_class)
    
    yrs = paste0('X',seq(1987,2017))
    DMI = GE
    DMI[,yrs] = sapply(yrs, function (x) round(GE[, x] / DIET , 2))
    
    return(DMI)
    rm(list=c('GE','DIET'))
  }
  else {
    stop('Not implemented yet.')
  }
}


