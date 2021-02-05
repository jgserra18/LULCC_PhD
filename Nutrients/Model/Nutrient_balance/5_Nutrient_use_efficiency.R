source('./Nutrients/Model/Nutrient_balance/2_Land_budget.R')

require('ggplot2')

yrs  = paste0('X',seq(1987,2017))

# regular NUE assessment ------------------------------------------------------------------

compute_reference_area_NutUseEfficiency = function(reference_area, nutrient = 'N') {
  
  tot_inputs = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Nutrient_balances', subfolder = 'Land_balance', subfolderX2 = nutrient, subfolderX3 = reference_area, subfolderX4 = 'Total', pattern = 'input')
  tot_outputs = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Nutrient_balances', subfolder = 'Land_balance', subfolderX2 = nutrient, subfolderX3 = reference_area, subfolderX4 = 'Total', pattern = 'output')
  
  use_efficiency = tot_outputs
  use_efficiency[, yrs] = sapply(yrs, function(x) round(tot_outputs[,x]/tot_inputs[,x]*100, 0))
  use_efficiency = data_cleaning(use_efficiency)
  
  return(use_efficiency)
}

ggplot_NutUseEfficiency_refArea = function(reference_area, nutrient = 'N', ...) {
  
  nue_df = compute_reference_area_NutUseEfficiency(reference_area)
  
  df = data.frame(yrs=yrs)
  ctr = 1
  for (yr in yrs) {
    
    df[ctr, paste0('X',c('0','25','50','75','100'))] = quantile(nue_df[, yr])
    ctr = ctr + 1
  }
  df$yrs = as.numeric(gsub('X','',df$yrs))
  ggplot() + 
    geom_area(data = df, aes(x=yrs, y=X100, group=''), fill='red1', size=1) + 
    geom_area(data = df, aes(x=yrs, y=X75, group=''), fill='orange1', size=1) +
    geom_line(data = df, aes(x=yrs, y=X50, group=''), color='black', size=1)  + 
    geom_area(data = df, aes(x=yrs, y=X50, group=''), fill='yellow', size=1)  + 
    geom_area(data = df, aes(x=yrs, y=X25, group=''), fill='green1', size=1) + 
    theme_test() + 
    scale_x_continuous(expand = c(0,0))
}


  
# irrigation NUE assessment ------------------------------------------------------------------

compute_standard_NUE = function(nutrient = 'N') {
  #' @description calculates the standard NUE (UAA) = N_out / N_in
  #' @note # unit: kg N UAA-1 yr-1
  
  grass_ha  = get_reference_area('Grassland')
  arable_ha = get_reference_area('Cropland')
  
  grass_in = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Nutrient_balances', subfolder = 'Land_balance', subfolderX2 = nutrient, subfolderX3 = 'Grassland', subfolderX4 = 'Total', pattern = 'input')
  grass_out = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Nutrient_balances', subfolder = 'Land_balance', subfolderX2 = nutrient, subfolderX3 = 'Grassland', subfolderX4 = 'Total', pattern = 'output')
  
  crop_in = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Nutrient_balances', subfolder = 'Land_balance', subfolderX2 = nutrient, subfolderX3 = 'Cropland', subfolderX4 = 'Total', pattern = 'input')
  crop_out = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Nutrient_balances', subfolder = 'Land_balance', subfolderX2 = nutrient, subfolderX3 = 'Cropland', subfolderX4 = 'Total', pattern = 'output')

  tot_in = crop_in
  tot_out = crop_out
  
  tot_in[, yrs] = sapply(yrs, function(x) round( (grass_ha[,x] * grass_in[,x] + arable_ha[,x] * crop_in[,x]) / (grass_ha[,x] + arable_ha[,x]), 2))
  tot_out[, yrs] = sapply(yrs, function(x) round( (grass_ha[,x] * grass_out[,x] + arable_ha[,x] * crop_out[,x]) / (grass_ha[,x] + arable_ha[,x]), 2))
  
  tot_in = data_cleaning(tot_in)
  tot_out = data_cleaning(tot_out)
  
  # compute NUE
  nue = tot_in
  nue[, yrs] = sapply(yrs, function(x) round( (tot_out[,x]/tot_in[,x]) * 100,2))
  
  return(list(total_in = tot_in, total_out = tot_out, NUE_standard = nue))
}


irrig_yrs = paste0('X',seq(1995,2017))

compute_irrigation_NUE = function(nutrient='N') {
  #' @note  unit: kg N ha_UAA-1 yr-1
  #' @return returns a list where index 1 = new total N inputs(inc irrigation), 2 - NUE with irrigation as an input, 3 - NUE difference by including irrigation

  # NUE params dump
  NUE_params = compute_standard_NUE()
  standard_in = NUE_params[[1]]
  standard_out = NUE_params[[2]]
  standard_nue = NUE_params[[3]]
  
  # get irrig N @UAA
  irrigN = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Irrigation', subfolder = 'Irrigation_N', subfolderX2 = 'Total', subfolderX3 = 'Average_municipality', pattern = 'Avg_municipality_UAA')

  # calculate new total N inputs
  new_in = standard_in[, c('Muni_ID','ID','Muni', irrig_yrs)]
  new_in[, irrig_yrs] = sapply(irrig_yrs, function(x) irrigN[,x] + standard_in[,x])
  
  # calculate new NUE 
  irrig_NUE = standard_in[, c('Muni_ID','ID','Muni', irrig_yrs)]
  irrig_NUE[, irrig_yrs] = sapply(irrig_yrs, function(x) round( (standard_out[,x] / new_in[,x]) * 100, 2))
  
  # calculate difference in NUE
  dif_NUE = standard_in[, c('Muni_ID','ID','Muni', irrig_yrs)]
  dif_NUE[, irrig_yrs] = sapply(irrig_yrs, function(x) round( irrig_NUE[,x] - standard_nue[,x], 2)) # in %
  
  return_list = list(new_tot_input = new_in, irrig_NUE = irrig_NUE, dif_NUE = dif_NUE)
  return_list = lapply(1:length(return_list), function(x) return_list[[x]][, c('Muni_ID','ID','Muni', irrig_yrs)]) # subset irrig years
  return_list = lapply(1:length(return_list), function(x) data_cleaning(return_list[[x]])) # clean NAs
  
  return(list(new_tot_input = new_in, irrig_NUE = irrig_NUE, dif_NUE = dif_NUE, standard_NUE = NUE_params[[3]]))
}

