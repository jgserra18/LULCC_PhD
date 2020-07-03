
# FEED ASSUMPTIONS ---------------------

#* PIGS: ration formulated for backyard pigs; for industrial pigs, 100% comes from concentrate or othern on-local feedstuff
#* Poultry: 100% comes from concentrate or other non-local feedstuff
#* Rabbits: 100% comes from concentrate or other non-local feedstuff

# RATIONS FORMULATED ------------------
#* Bovine: dairy cattle and other_cattle
#* Goats and Sheep
#* Backyard pigs
#* Horses

# DAIRY CATTLE PARTICULARITY ---------
#* Crop residues are fed as the difference of 1 - FRAC_other_roughage - FRAC_concentrates



export_file(module = 'Nutrients', 
            file = Nut_intake, 
            filename = param, 
            folder = 'Fodder_production', 
            subfolder = 'Nutrient_concentrates', 
            subfolderX2 = nutrient,
            subfolderX3 = management,
            subfolderX4 = main_param)


# NUTRIENTS IN CONCENTRATE FEEDING --------------------------------------------------------


compute_nutrient_only_concentrates = function(main_param, param, management = 'Total', nutrient ='N') {
  # computes the nutrient content of animals where the feeding ratio is 100% concentrates or other non-locall feedstuff
  # unit: kg nutrient yr-1
  
  if (main_param == 'Rabbits' | main_param == 'Poultry' | (main_param == 'Pigs' & management = 'Industrial')) {
    
    
    Nut_intake = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Fodder_production',subfolder = 'Nutrient_intake', subfolderX2 = nutrient, subfolderX3 = 'Total', subfolderX4 = main_param, pattern = param)
    return(Nut_intake)
  }
  else {
    stop('Only animals fed 100% of concentrate/non-local feedstuff.')
  }
}


compute_nutrient_concentrates_dairyCow = function(main_param = 'Bovine', param = 'Dairy_cows', management, nutrient = 'N') {
  
  
  Nut_intake = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Fodder_production',subfolder = 'Nutrient_intake', subfolderX2 = nutrient, subfolderX3 = management, subfolderX4 = main_param, pattern = param)
  FRAC_concentrate = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Fodder_production',subfolder = 'FRAC_concentrate', subfolderX2 = main_param, pattern = param)
  
  yrs = paste0('X',seq(1987,2017))
  
  Nut_concentrate = FRAC_concentrate
  Nut_concentrate[, yrs] = sapply(yrs, function(x) round(Nut_intake[,x] * FRAC_concentrate[,x], 1))
  
  return(Nut_concentrate)
  rm(list=c('Nut_intake','FRAC_concentrate'))
}

