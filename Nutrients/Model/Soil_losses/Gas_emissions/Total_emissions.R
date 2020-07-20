source('./Main/Global_functions.R')




compute_gaseous_emissions_field_app = function(gas, manure_surplus_fills_nutDemand = F, manure_method = 'Method 1', nutrient = 'N') {
  # computes the total gaseous emissions for a given gas for all field application
  # unit: kg N-gas yr-1
  
  if (manure_surplus_fills_nutDemand == TRUE) { folder_div = 'With_ManSurplus'} else { folder_div = 'Without_ManSurplus'}
  
  biosolids = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gas_N_emissions', subfolder = gas, subfolderX2 = 'Biosolid_application', subfolderX3 = manure_method, subfolderX4 = folder_div, subfolderX5 = 'Total', pattern = 'Total')
  fert  = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gas_N_emissions', subfolder = gas, subfolderX2 = 'Inorganic_fertiliser', subfolderX3 = manure_method, subfolderX4 = folder_div, subfolderX5 = 'Total', subfolderX6 = 'Total', pattern = 'Total')
  man_app = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gas_N_emissions', subfolder = gas, subfolderX2 = 'Manure_application', subfolderX3 = manure_method, subfolderX4 = folder_div, subfolderX5 = 'Total', subfolderX6 = 'Total', pattern = 'Total')

  field_app = man_app
  
  yrs = paste0('X', seq(1987,2017))  
  field_app[, yrs] = sapply(yrs, function(x) round(biosolids[,x]+fert[,x]+man_app[,x], 1))
  
  return(field_app)
  rm(list=c('biosolids','fert','man_app'))
}



compute_gaseous_emissions_MMS = function(gas) {
  
  if (gas == 'NH3') {
    
    params = c('Storage','Yards','Housing', 'Grazing')
  }
  else {
    
    params = c('Storage', 'Grazing')
  }
  
  
  yrs <- paste0('X', seq(1987,2017))
  store_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  store_df[, yrs] <- sapply(yrs, function(x) store_df[,x] <- 0)
  
  for (i in params) {
    
    N_emissions = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gas_N_emissions', subfolder = gas, subfolderX2 = i, subfolderX3 = 'Total', subfolderX4 = 'Total', pattern = 'Total')
    store_df[, yrs] = sapply(yrs, function(x) round(store_df[, x] + N_emissions[,x], 1))
  }
  
  return(store_df)
  rm(list=c('params','yrs','N_emissions'))
}
  

compute_total_gaseous_emissions = function(gas, manure_surplus_fills_nutDemand = F, manure_method = 'Method 1', nutrient = 'N') {
  

  MMS = compute_gaseous_emissions_MMS(gas)
  Field_app = compute_gaseous_emissions_field_app(gas, manure_surplus_fills_nutDemand, manure_method, nutrient)
  tot_emissions = Field_app
  
  yrs = paste0('X', seq(1987,2017))
  tot_emissions[, yrs] = sapply(yrs, function(x) round(MMS[,x] + Field_app[,x], 1))
  
  return(tot_emissions)
  rm(list=c('MMS','Field_app','yrs'))
}

loop_gaseous_emissions = function(manure_surplus_fills_nutDemand = F, manure_method = 'Method 1', nutrient = 'N') {
  
  
  if (manure_surplus_fills_nutDemand == TRUE) { folder_div = 'With_ManSurplus'} else { folder_div = 'Without_ManSurplus'}
  
  gas = c('NH3','N2O','NN2','NOx')
  for (i in gas) {
    print(i)
    tot_emissions = compute_total_gaseous_emissions(i, manure_surplus_fills_nutDemand, manure_method, nutrient)
    export_file(module = 'Nutrients', 
            file = tot_emissions, 
            filename = 'Total', 
            folder = 'Gas_N_emissions', 
            subfolder = i, 
            subfolderX2 = 'Total',
            subfolderX3 = manure_method, 
            subfolderX4 = folder_div,
            subfolderX5 = 'Total')
  }
}
loop_gaseous_emissions()
  