source('./Main/Global_functions.R')
source('./Nutrients/Model/Fodder_production/1_Energy_requirements.R')


# ENERGY REQUIRED FOR MILK PRODUCTION ------------------------------------------------------------------

compute_EnergyCorrectedMilk_ECM = function(milk_yield, FRAC_fat, FRAC_protein) {
  # Eq 3.2 
  # unit: kg milk day-1
  
  milk_prod = milk_yield / 271  # kg milk day
  FRAC_protein = FRAC_protein * 10 # % to g Kg-1
  FRAC_fat = FRAC_fat * 10# % to kg-1
  
  ECM = round(milk_prod * (0.25+0.122 * FRAC_fat/10 + 0.077 * FRAC_protein /10), 3)
  
  return(ECM)
}


compute_NetEnergyLactation_NEL = function(milk_yield, FRAC_fat, FRAC_protein) {
  # Eq 9.2
  # 1 kg ECM requires 3.14 MJ NEL
  # unit: MJ NEL day-1

  ECM = compute_EnergyCorrectedMilk_ECM(milk_yield)
  NEL_milk = ECM * 3.14 
  
  return(NEL_milk)
}


compute_NetEnergyLactation_NEL_DM = function(DMI, milk_yield, FRAC_fat, FRAC_protein) {
  # diet net energy value for lactation
  # Eq 8.10
  # unit: Mj kg DM-1
  # unnit: SFU kg DM_1
  
  SFU = 8.9 # Mj kg DM-1
  NEL = compute_NetEnergyLactation_NEL(milk_yield, FRAC_fat, FRAC_protein)
  NEL_DM = (NEL/ DMI) / SFU
  
  return(NEL_DM)
}


# FILL VALUE OF FEEDSTUF ------------------------------------------------------------------------------

compute_FV_roughage = function() {
  #FV_roughage = roughage fill value
  # equation 6.10
  # unit: FV kg DM-1
  
  OMD = 70 # %
  NDF = 330 # g kg DM-1
  
  FV_roughage = round((0.86-OMD*0.005)/(0.94+0.56*exp(-0.000029*(NDF/10)^2.9)), 3)
  
  return(FV_roughage)
}


compute_FV_concentrate = function() {
  
  #FV_concentrate = 0.22 # FV kg DM-1
  OMD = 90 # %
  NDF = 9 # g kg DM-1
  
  FV_concentrate = round((0.86-OMD*0.005)/(0.94+0.56*exp(-0.000029*(NDF/10)^2.9)), 3)
  return(FV_concentrate)
}


# INTAKE CAPACITY (DMI) -----------------------------------------------------------------------------------------


compute_IntakeCapacity_dairyCows = function(milk_yield, FRAC_fat, FRAC_protein) {
  # equivalent to the DMI
  # intake capacity, Eq 10.2
  # unit: kg DM head-1
  
  # set NorFor params Multiparous large dairy breeds
  a	= 2.82
  b	= 0.134
  c	= -0.0006
  d	= 0.55
  e	= 0.091
  f	= 575
  g	= 0.006
  
  DIM = 271 # days in milk
  ECM = compute_EnergyCorrectedMilk_ECM(milk_yield, FRAC_fat, FRAC_protein)
  BW = 600 # kg head-1
  
  IC_cow = round(a*DIM^(b) * e^(c*DIM) - DIM^(-d) + e*ECM + (BW-f)*g, 3)
  
  return(IC_cow)
}


compute_factorR = function() {
  # adjustment factor for the intake capacity
  # Eq 10.4
  # unit: 0 to 1; dimensionless
  
  roughage_apettite = 60 # arbitrary number corresponding to the % of intake of roughages
  factorR = round(0.0214 * (roughage_apettite/5-13) + 0.8502, 3)
  
  return(factorR)
}


compute_total_IntakeCapacity_DairyCows = function(milk_yield, FRAC_fat, FRAC_protein) {
  # Eq 10.3
  # computes the total DMI
  # unit: FV kg DMI hd-1
  
  IC_animal = compute_IntakeCapacity_dairyCows(milk_yield, FRAC_fat, FRAC_protein)
  IC_exercise = 0 # housing 0 and grazing 0.15
  factorR = compute_factorR()
  
  IC = (IC_animal + IC_exercise) * factorR
  
  return(IC)
}



# PREPARE INPUT DATA TO CONCENTRATE DEDUCTION ALGORITHM ------------------------------------------



compute_FV_SubR = function(roughage_DMI) {
  # roughage substituion factor based on the intake of starch and sugar
  # Eq 10.7
  # unit: 0 - 1; dimensionless
  
  ST_SU_DM = 125 /1000 # g (kg DM)-1
  ST_SU_intake = roughage_DMI * ST_SU_DM * 1000# g day-1

  FV_subrR = round(0.97 + 0.562*(ST_SU_DM/1000-0.2119) * 0.1 - 0.1932 * (ST_SU_intake/1000-5.122) * 0.05, 2)
  
  return(FV_subrR)
}



compute_FV_MR = function(milk_yield, FRAC_fat, FRAC_protein) {
  # roughage metabolic correction factor
  # Eq 10.8 
  # unit 0 - 1; dimensionless
  
  FV_r = compute_FV_roughage()
  IC = compute_total_IntakeCapacity_DairyCows(milk_yield, FRAC_fat, FRAC_protein)
  
  FV_MR = (1.453-(2.53/(1+exp((0.466-FV_r)/0.065)*IC/8)))
  
  return(FV_MR)
}



## CONCENTRATE OF MASS FEEDING DEDUCTION .------------------------------------------------------------


general_func_necessary_concentrate_FRAC = function(DMI, my, GE, frac_protein, frac_fat) {
  # reverse calculates the necessary concentrate mass feeding 
  # this is based on the assumption that there is a priority of roughage
  # roughage starts as the total DMI, if there is enough energy in feeding to fulfill gross energy requirements
  # then stop
  # else reduce roughage DMI by 1 and increase concentrate DMI by 1
  
  # start with DMI_c = 0
  DMI_r = DMI
  DMI_c = 0
  
  FV_subrR = compute_FV_SubR(DMI_r)
  R_side = DMI_r * compute_FV_roughage() * FV_subrR
  C_side = DMI_c * compute_FV_concentrate()
  FV_MR = compute_FV_MR(my, frac_fat, frac_protein)
  
  IC = compute_total_IntakeCapacity_DairyCows(my, frac_fat, frac_protein)
  Energy_feed = DMI_r * 0.81 * 7.9 + DMI_c * 1.22 * 10.9
  
  
  #while ( (FV_in > IC) | (Energy_feed < GE) ) {
  while (Energy_feed < GE) {
      
    DMI_r = DMI_r - 1
    DMI_c = DMI_c + 1 
    
    FV_subrR = compute_FV_SubR(DMI_r)
    R_side = DMI_r * compute_FV_roughage() * FV_subrR
    C_side = DMI_c * compute_FV_concentrate()
    FV_MR = compute_FV_MR(my, frac_fat, frac_protein)
    
    IC = compute_total_IntakeCapacity_DairyCows(my, frac_fat, frac_protein)
    Energy_feed = DMI_r * 0.81 * 7.9 + DMI_c * 1.22 * 10.9
  }
  
  conc_FRAC = round(DMI_c / (DMI_c + DMI_r), 4)
  
  return(conc_FRAC)
}



update_daily_milk_yield = function() {
  # unit: kg milk hd-1 yr-1
  
  yrs = paste0('X',seq(1987,2017))
  DIM = 271
  milk_per_cow = compute_linear_extrapolation_milkPerCow_historical()
  milk_per_cow = convert_dairy_Nex_NUTS2_municipality(milk_per_cow)
  names(milk_per_cow)[4:ncol(milk_per_cow)] = paste0('X', names(milk_per_cow)[4:ncol(milk_per_cow)])
  
  milk_per_cow[, yrs] = sapply(yrs, function(x) round( milk_per_cow[,x] / 271, 2))
  
  return(milk_per_cow)
}


compute_all_necessary_concentrate_FRAC = function() {
  
  # get necessary param for 1987-2017 for all municipalities ------------
  DMI_df = compute_total_DMI('Bovine','Dairy_cows')
  GE_df = compute_total_gross_energy_GE('Bovine','Dairy_cows')
  milk_per_cow_df = update_daily_milk_yield()
  FRAC_fat_df = get_energy_requirement_params('Ruminants', 'Milk', 'Dairy_cows')
  FRAC_protein_df = get_activity_data(module = 'Nutrients', subfolder = 'General_params', subfolderX2 = 'Animals', subfolderX3 = 'Diet', subfolderX4 = 'Ruminants', subfolderX5 = 'Nutrient_retention', pattern = 'Prot')


  conc_FRAC_df = DMI_df

  # start to go through each municipality and year
  for (c in 4:ncol(DMI_df)) {
    
    for (r in 1:nrow(DMI_df)) {
      
      if (DMI_df[r,c] == 0) {
        next 
      }
      else {
        
        DMI = DMI_df[r,c]
        GE = GE_df[r,c]
        
        my = milk_per_cow_df[r,c]
        fat = FRAC_fat_df[r,c]
        prot = FRAC_protein_df[r,c]
        
        conc_FRAC_df[r,c] = general_func_necessary_concentrate_FRAC(DMI, my, GE, prot, fat)
      }
    }
  }
  
  return(conc_FRAC_df)
  rm(list=c('DMI_df','GE_df','milk_per_cow_df','FRAC_fat_df','FRAC_protein_df','DMI','GE','my','fat','prot'))
}


export_necessary_cocnentrate_FRAC = function() {
  
  conc_FRAC = compute_all_necessary_concentrate_FRAC()
  export_file(module = 'Nutrients', 
              file = conc_FRAC, 
              filename = 'Dairy_cows', 
              folder = 'Fodder_production', 
              subfolder = 'FRAC_concentrate', 
              subfolderX2 = 'Bovine')
}

