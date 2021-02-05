source('./Nutrients/Model/Nutrient_balance/2_Land_budget.R')


compute_soil_surface_budget = function(reference_area, per_area = FALSE, nutrient = 'N') {
  # computes the SSNB
  # unit: kg N-P yr-1 or kg N-P ha-1 yr-1
  
  input_params = set_total_input_params(reference_area, nutrient)
  output_params = set_total_output_params(reference_area, nutrient)
  environment_loss = set_total_environmental_losses(reference_area = reference_area, nutrient = nutrient)
  
  ssnb = output_params 
  yrs = paste0('X',seq(1987,2017))
  
  ssnb[, yrs] = sapply(yrs, function(x) round(input_params[,x] - output_params[,x] - environment_loss[,x], 0)) # kg N-P yr-1
  
  if (per_area == TRUE) {
    
    ref_area = get_reference_area(reference_area)
    ssnb[, yrs] = sapply(yrs, function(x) round(ssnb[,x] / ref_area[, x], 0))
  }
  ssnb = data_cleaning(ssnb)
  
  return(ssnb)
  rm(list=c('input_params','output_params'))
}

export_ssnb_data = function(reference_area, nutrient = 'N') {
  
  
  ssnb = compute_soil_surface_budget(reference_area, per_area = TRUE, nutrient)
  export_file(module = 'Nutrients', 
              folder = 'Nutrient_balances', 
              subfolder = 'Soil_balance', 
              subfolderX2 = nutrient, 
              subfolderX3 = reference_area, 
              subfolderX4 = 'Total',
              file = ssnb, 
              filename = 'soil_surface_balance')
  
}
#export_ssnb_data('Cropland')
#export_ssnb_data('Grassland')
