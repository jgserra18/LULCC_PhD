source('./Main/Global_functions.R')
source('./Nutrients/Model/Nutrient_balance/1_Prepare_parameters.R')


# Ymax = Y  * N_inp / ( N_inp * Y)
# Y = Crop_uptake + Fodder_uptake

# Sanz-Cobena A, Lassaletta L, Estellés F, et al. 
# Yield-scaled mitigation of ammonia emission from N fertilization: The Spanish case. 
# Environ Res Lett. 2014;9(12). doi:10.1088/1748-9326/9/12/125005


nutrient='N'
yrs = paste0('X',seq(1987,2017))



compute_longterm_integrated_annual_yield_Y = function() {
  # unit: kg N ha_cropland-1 yr-1
  
  crop_off = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Nutrient_balances', subfolder = 'Land_balance', subfolderX2 = 'N', subfolderX3 = 'Cropland', subfolderX4 = 'Output', pattern = 'crop_offtake')
  fodder_off = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Nutrient_balances', subfolder = 'Land_balance', subfolderX2 = 'N', subfolderX3 = 'Cropland', subfolderX4 = 'Output', pattern = 'fodder_offtake')
  
  Y = fodder_off
  Y[, yrs] = sapply(yrs, function(x) round(fodder_off[,x] + crop_off[,x], 0))
  
  return(Y)
  rm(list=c('crop_off','fodder_off'))
}



# calculate effective fertilisation rate, sum of N inputs minus NH3 emisisons ---------------------------------------------------------------------------------------------------

compute_total_manure_cropland  = function() {
  # computes total net manure N applied to cropland
  # unit: kg N yr-1
  
  store <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  store[, yrs] <- sapply(yrs, function(x) store[,x] <- 0)
  
  
  standard_params <- get_activity_data(module = 'Nutrients', folder = 'General_params', pattern = 'Params_list')

  for (i in 1:nrow(standard_params)) {
    
    main_param = standard_params[i,1]
    param = standard_params[i,2]

    if (param == 'Extensive_pasture' | param == 'Tomato' | param == 'Horticulture_intensive' | param == 'Horticulture_extensive') { 
      next 
    }
    else {
      # compute total manure N applied to each crop besides extensive pasture and vegetables
      crop_area = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = main_param, pattern = param)
      man_crop = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Fertilisation', subfolder = 'N', subfolderX2 = 'Manure_application_rates', 
                                   subfolderX3 = 'Method 1', subfolderX4 = 'Without_ManSurplus', subfolderX5 = 'Total', subfolderX6 = main_param, pattern = param)
      man_crop[, yrs] = sapply(yrs, function(x) round(man_crop[,x] * crop_area[,x], 1))
      
      # add to the total df
      store[, yrs] = sapply(yrs, function(x) store[,x] + man_crop[,x])
    }
  }
  
  return(store)
  rm(list=c('crop_area','man_crop'))
}

compute_total_net_grazing_ref_area = function(reference_area = 'Cropland') {
  #' @description computes net N grazing excreted onto cropland or grassland
  #' @usage unit: kg N yr-1
  
  past_type =ifelse(reference_area=='Cropland','Intensive_pasture','Extensive_pasture')
  
  Gman_pasture  = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gross_manure', subfolder = nutrient, subfolderX2 = 'Grazing', subfolderX3 = 'Total', subfolderX4 = 'Total', pattern = 'Total')
  Gman_pasture = compute_FRAC_pastures_nutrient_flows(nutrient_flow_df = Gman_pasture,pasture_type = past_type)
  
  NH3_pasture = read.csv('./Nutrients/Output/Gas_N_emissions/NH3/Grazing/Total/Total/Total_sum.csv')
  NH3_pasture = compute_FRAC_pastures_nutrient_flows(nutrient_flow_df = NH3_pasture,pasture_type = past_type)
  
  # compute net N grazing
  Gman_pasture[, yrs] = sapply(yrs, function(x) round(Gman_pasture[,x] - NH3_pasture[,x], 1))
  
  return(Gman_pasture)
  rm(list=c('past_type','NH3_pasture'))
}



compute_net_total_synthetic_fert = function(fertiliser_type) {
  # calculate net N returned to the soil from sludge and synthetic fertilisers following field NH3 emissions
  # unit: kg N yr-1
  
  if (fertiliser_type == 'Synthetic_fertiliser') {
    
    tot_fert = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Fertilisation', subfolder = nutrient, subfolderX2 = 'Inorganic_fertiliser', subfolderX3 = 'Method 1', subfolderX4 = 'Without_ManSurplus', subfolderX5 = 'Total',subfolderX6 = 'Adjusted', pattern = 'Adjusted')
    fert_nh3 = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gas_N_emissions', subfolder = 'NH3', 
                                 subfolderX2 = 'Inorganic_fertiliser', subfolderX3 ='Method 1', subfolderX4 = 'Without_ManSurplus', 
                                 subfolderX5 = 'Total', subfolderX6 = 'Total', pattern = 'Total')
    
  }
  else if (fertiliser_type == 'Sludge') {
    
    tot_fert =get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Fertilisation', subfolder = nutrient, subfolderX2 = 'Biosolids', subfolderX3 = 'Total', pattern = 'Total')
    fert_nh3 = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gas_N_emissions', subfolder = 'NH3', 
                                 subfolderX2 = 'Biosolid_application', subfolderX3 ='Method 1', subfolderX4 = 'Without_ManSurplus', 
                                 subfolderX5 = 'Total',  pattern = 'Total')
  }
  else {
    break('Only sludge and synthetic fertiliser.')
  }
  # calculate net N
  tot_fert[,yrs] = sapply(yrs, function(x) round(tot_fert[,x] - fert_nh3[,x], 1))
  
  return(tot_fert)
  rm(list=c('fert_nh3'))
}





compute_effective_fertilisation_rate = function() {
  # N_inp = BNF + Dep + Manure_cropland + (Fert - NH3)
  # unit: kg N ha_cropland-1 yr-1
  
  cropland = get_reference_area(reference_area = 'Cropland') # ha
  
  BNF = get_total_BNF(reference_area = 'Cropland', nutrient = 'N') # kg N ; pulses N fixation
  Dep = get_atmospheric_deposition('Cropland', nutrient = 'N') # kg N ; atmospheric deposition
 # man  = compute_total_manure_cropland() # kg N ; net manure N
  man  = get_gross_manure(reference_area = 'Cropland', nutrient = 'N') # kg N ; net manure N
  sludge = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Fertilisation', subfolder = nutrient, subfolderX2 = 'Biosolids', subfolderX3 = 'Total', pattern = 'Total') # kg N ; net sludge N
  syn_fert = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Fertilisation', subfolder = nutrient, subfolderX2 = 'Inorganic_fertiliser', subfolderX3 = 'Method 1', subfolderX4 = 'Without_ManSurplus', subfolderX5 = 'Total',subfolderX6 = 'Adjusted', pattern = 'Adjusted') # kg N ; net syn fert
  
  N_inp = syn_fert
  N_inp[,yrs] = sapply(yrs, function(x) round( ( BNF[,x] + Dep[,x] + man[,x] + sludge[,x] + syn_fert[,x] ) / cropland[,x], 1) )
  N_inp = data_cleaning(N_inp)
  
  return(N_inp)
  rm(list=c('cropland','BNF','Dep','man','sludge','syn_fert'))
}


# calculate Ymax for cropland, for 1987-2017 @ municipality ---------------------------------------------------------------------------------------------------


compute_Ymax = function() {
  # computes the Ymax value
  # unit : kg N ha_cropland-1 yr-1
  
  Y = compute_longterm_integrated_annual_yield_Y()
  N_inp = compute_effective_fertilisation_rate()

  Ymax = N_inp
  Ymax[, yrs] = sapply(yrs, function(x) round(Y[,x] * N_inp[,x] / ( N_inp[,x] - Y[,x] ) , 1))
  Ymax = data_cleaning(Ymax)
  
  return(Ymax)
}

d = compute_Ymax()
View(d)

require('ggplot2')

Historical_Ymax_interactive_municipality = function(muni_row) {
  #' @param muni_row row for a given municipality
  #' @description plots the historical Ymax for a given municipality 
  
  all_Ymax = compute_Ymax() # Ymax for all municipalities
  
  df = data.frame(N_inp = as.numeric(N_inp[muni_row,-seq(1,3)]),
                  Y = as.numeric(Y[muni_row,-seq(1,3)]), 
                  yr = seq(1987,2017))
  ggplot(df, aes(x=N_inp,y=Y, colour=yr)) + 
    geom_point(size=3) + 
    geom_path(colour='black',arrow = arrow(type='closed',ends='last', length=unit(0.15,'inches'))) + 
    geom_abline(slope = 1, intercept = 0, colour='black') + 
    scale_y_continuous(limits=c(0,1000)) +
    scale_x_continuous(limits=c(0,1000)) +
    theme_dark() +
    scale_colour_viridis_c(breaks=c(1990,1995,2000,2005,2010,2015))
}

Historical_Ymax_interactive_municipality(12)

