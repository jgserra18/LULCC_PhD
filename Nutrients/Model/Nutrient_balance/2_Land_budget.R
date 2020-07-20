source('./Nutrients/Model/Nutrient_balance/1_Prepare_parameters.R')


# land budget = N inputs - N outputs
# unit: kg N-P yr-1 

# COMPUTE LAND BUDGET -----------------------------------


compute_land_nutrient_budget = function(reference_area, per_area = FALSE, nutrient = 'N') {
  # computes the land balance
  # unit: kg N-P yr-1 or kg N-P ha-1 yr-1
  
  input_params = set_total_input_params(reference_area, nutrient)
  output_params = set_total_output_params(reference_area, nutrient)
  
  land_balance = output_params 
  yrs = paste0('X',seq(1987,2017))
  
  land_balance[, yrs] = sapply(yrs, function(x) round(input_params[,x] - output_params[,x], 0)) # kg N-P yr-1
  
  if (per_area == TRUE) {
    
    ref_area = get_reference_area(reference_area)
    land_balance[, yrs] = sapply(yrs, function(x) round(land_balance[,x] / ref_area[, x], 0))
  }
  land_balance = data_cleaning(land_balance)
  
  return(land_balance)
  rm(list=c('input_params','output_params'))
}


export_land_budget_data = function(reference_area, nutrient = 'N') {
  
  
  yrs = paste0('X',seq(1987,2017))
  ref_area = get_reference_area(reference_area)
  # compute IO params per area
  input_params = set_total_input_params(reference_area, nutrient)
  input_params[, yrs] = sapply(yrs, function(x) round(input_params[,x] / ref_area[, x], 0))
  input_params = data_cleaning(input_params)
  
  output_params = set_total_output_params(reference_area, nutrient)
  output_params[, yrs] = sapply(yrs, function(x) round(output_params[,x] / ref_area[, x], 0))
  output_params = data_cleaning(output_params)
  
  land_balance = compute_land_nutrient_budget(reference_area, per_area = TRUE, nutrient)
  
  data_to_export = list(tot_input  = input_params, tot_output = output_params, land_balance = land_balance) 

  ctr = 0
  for (data in data_to_export) {
    
    ctr = ctr + 1
    export_file(module = 'Nutrients', 
                folder = 'Nutrient_balances', 
                subfolder = 'Land_balance', 
                subfolderX2 = nutrient, 
                subfolderX3 = reference_area, 
                subfolderX4 = 'Total',
                file = data, 
                filename = names(data_to_export)[ctr])
  }
}



