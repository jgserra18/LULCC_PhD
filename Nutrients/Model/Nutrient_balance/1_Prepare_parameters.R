source('./Main/Global_functions.R')
source('./Nutrients/Model/Crop_production/get_fodder_crops_residues.R')
source('./Nutrients/Model/Atmospheric_deposition/Compute_atmospheric_Ndeposition.R')
source('./Nutrients/Model/BNF/Compute_BNF.R')



# PREPARE INPUT-OUTPUT PARAMETERS TO CALCULATE NUTRIENT BALANCES ----------------------------------------------------------
# THIS INCLUDES:
##* - REFERENCE AREA (CROPLAND, GRASSLAND)
##* - INPUT PARAMS (DEPENDING ON NUTRIENT AND REFERENCE AREA)
##* - OUTPUT PARAMS 
##* - NUTRIENT RUNOFF LOSSES 
##* - GASEOUS N EMISSIONS


get_reference_area = function(reference_area) {
  # UAA can also be implemented
  # unit: ha
  
  if (reference_area == 'Cropland') {
    
    area = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Reference_areas', subfolder = 'Arable_land', pattern = 'Arable_land')
  }
  else {
    area = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = 'Pastures', pattern = 'Extensive_pasture')
  }
  return(area)
}




export_param_in_list = function(list_with_params, reference_area, is_input = TRUE, nutrient = 'N') {
  # to export IO list with all params
  # convert to kg N-P ref_area-1 yr-1
  
  yrs = paste0('X',seq(1987,2017))
  ref_area = get_reference_area(reference_area)
  
  IO_folder = ifelse(is_input == TRUE,'Inputs','Output')
  ctr = 0
  for (file in list_with_params) {
    
    ctr = ctr + 1
    file[,yrs] = sapply(yrs, function(x) round(file[,yrs] / ref_area[,yrs], 1))
    file = data_cleaning(file)
    export_file(module = 'Nutrients', 
                folder = 'Nutrient_balances', 
                subfolder = 'Land_balance', 
                subfolderX2 = nutrient, 
                subfolderX3 = reference_area, 
                subfolderX4 = IO_folder,
                file = file, 
                filename = names(list_with_params)[ctr])
  }
}


# CROPLAND -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# input parameters ----------------------------------------------------------------------------------------------------------------------------


get_total_BNF = function(reference_area, nutrient = 'N') {
  # Cropland = Pulses + Intensive_pasture
  # Grassland = Extensive_pasture
  
  if (reference_area == 'Cropland') { 
    BNF = compute_BNF_in_arableLand()
  }
  else {
    BNF = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'BNF', subfolder = nutrient, subfolderX2 = 'Pastures', pattern = 'Extensive_pasture')
  }
  
  return(BNF)
}

get_gross_manure = function(reference_area, nutrient = 'N') {
  # Cropland = tot gross man excretion - gross man excretion onto pastures
  # Grassland = gross man excreted onto pastures
  
  if (reference_area == 'Grassland') {
    
    gross_man_grass  = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gross_manure', subfolder = nutrient, subfolderX2 = 'Grazing', subfolderX3 = 'Total', subfolderX4 = 'Total', pattern = 'Total')
    return(gross_man_grass)
  }
  else {
    
    yrs = paste0('X',seq(1987,2017))
    gross_man_grass  = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gross_manure', subfolder = nutrient, subfolderX2 = 'Grazing', subfolderX3 = 'Total', subfolderX4 = 'Total', pattern = 'Total')
    gross_man_tot = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gross_manure', subfolder = nutrient, subfolderX2 = 'Total_Nexcretion', subfolderX3 = 'Total', pattern = 'Total')
    
    gross_man_crop = gross_man_tot
    gross_man_crop[, yrs] = sapply(yrs, function(x) round(gross_man_tot[, x] - gross_man_grass[, x], 0))
    
    return(gross_man_crop)
    rm(list=c('yrs','gross_man_grass','gross_man_tot'))
  }
}

get_synthetic_fertiliser = function(nutrient = 'N') {
  # default: Method 1, without man surplus
  # cropland = all fertiliser applied
  # grassland = assumed no fertilisation
  
  fert = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Fertilisation', subfolder = nutrient, subfolderX2 = 'Inorganic_fertiliser', subfolderX3 = 'Method 1', subfolderX4 = 'Without_ManSurplus', subfolderX5 = 'Total',subfolderX6 = 'Adjusted', pattern = 'Adjusted')
  return(fert)
}

get_atmospheric_deposition = function(reference_area, nutrient = 'N') {
  
  if (reference_area == 'Cropland') {
    atmN = compute_total_atmN_muni(reference_area = 'Cropland')
  }
  else {
    atmN = compute_total_atmN_muni(reference_area = 'Grassland')
  }
  return(atmN)
}

get_sewage_sludge = function(nutrient = 'N') {
  # default: Method 1, without man surplus
  # cropland = all fertiliser applied
  # grassland = assumed no fertilisation
  
  sludge = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Fertilisation', subfolder = nutrient, subfolderX2 = 'Biosolids', subfolderX3 = 'Total', pattern = 'Total')
  return(sludge)
}




set_total_input_params = function(reference_area, export = 'FALSE', nutrient = 'N') {
  # createa a list with all the input parameters
  # according to nutrients
  
  gross_man = get_gross_manure(reference_area, nutrient)
  fert = get_synthetic_fertiliser(nutrient)
  sludge = get_sewage_sludge(nutrient)
  
  yrs = paste0('X',seq(1987,2017))
  tot_input = sludge
  
  if (nutrient == 'N') {
    
    dep = get_atmospheric_deposition(reference_area, nutrient)
    bnf = get_total_BNF(reference_area, nutrient)
    
    # sum total inputs
    tot_input[, yrs] = sapply(yrs, function(x) round(gross_man[,x] + fert[,x] + sludge[,x] + dep[,x] + bnf[, x], 0))
    
    if (export == TRUE) {
      
      store_params = list(gross_manure = gross_man, synthetic_fertiliser = fert, sludge = sludge, atmospheric_deposition = dep, bnf = bnf)
      export_param_in_list(list_with_params = store_params, reference_area, is_input = TRUE, nutrient)
    }
    
    return(tot_input)
  }
  else {
    
    # sum total inputs
    tot_input[, yrs] = sapply(yrs, function(x) round(gross_man[,x] + fert[,x] + sludge[,x], 0))
    
    if (export == TRUE) {
      
      store_params = list(gross_manure = gross_man, synthetic_fertiliser = fert, sludge = sludge)
      export_param_in_list(list_with_params = store_params, reference_area, is_input = TRUE, nutrient)
    }
    return(tot_input)
  }
}



# output parameters ----------------------------------------------------------------------------------------------------------------------------

get_fodder_residues = function(reference_area, nutrient = 'N') {
  # fodder and non-fodder cereals
  # from "get_fodder_crop_residues.R"
  
  fodder_res = allocation_forage_residues_flows_per_referenceArea(reference_area, nutrient)
  return(fodder_res)
}


get_fodder_offtake = function(reference_area, nutrient = 'N') {
  # from "get_fodder_crop_residues.R"
  
  fodder_off = allocation_forage_offtake_flows_per_referenceArea(reference_area, nutrient)
  return(fodder_off)
}

get_crop_residues = function(reference_area, nutrient = 'N') {
  # note: the total doesn't include fodder crops !
  
  crop_res = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Crop_residues', subfolder = 'Removed', subfolderX2 = nutrient, subfolderX3 = 'Total', pattern = 'Total')
  return(crop_res)
}

get_crop_offtake = function(reference_area, nutrient = 'N') {
  # note: the total doesn't include fodder crops !
  
  crop_off = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Crop_offtake', subfolder = nutrient, subfolderX2 = 'Total', pattern = 'Total')
  return(crop_off)
}



set_total_output_params = function(reference_area, export = FALSE, nutrient = 'N') {
  # burnt residues are not included
  
  crop_res = get_crop_residues(reference_area, nutrient)
  fodder_res = get_fodder_residues(reference_area, nutrient)
  fodder_offtake = get_fodder_offtake(reference_area, nutrient)
  crop_offtake = get_crop_offtake(reference_area, nutrient)
  
  # sum total outputs
  yrs = paste0('X',seq(1987,2017))
  
  tot_out = crop_offtake
  tot_out[, yrs] = sapply(yrs, function(x) round(crop_res[,x] + fodder_res[,x] + fodder_offtake[,x] + crop_offtake[,x], 0))
  
  if (export == TRUE) {
    
    store_params = list(crop_residues = crop_res, fodder_residues = fodder_res, fodder_offtake = fodder_offtake, crop_offtake = crop_offtake)
    export_param_in_list(list_with_params = store_params, reference_area, is_input = FALSE, nutrient)
  }
  
  return(tot_out)
}



get_manure_transport = function(nutrient = 'N') {
  
  man_transport = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gross_manure', subfolder = nutrient, subfolderX2 = 'Transported', subfolderX3 = 'Total', pattern = 'Total')
  return(man_transport)
}


get_manure_discharged = function(nutrient = 'N') {
  
  man_discharge = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gross_manure', subfolder = nutrient, subfolderX2 = 'River_discharge', subfolderX3 = 'Total', pattern = 'Total')
  return(man_discharge)
}

get_crop_residues_burnt = function(nutrient = 'N') {
  
  res_burnt =  get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Crop_residues', subfolder = 'Burnt', subfolderX2 = 'N', subfolderX3 = 'Total', pattern = 'Total')
  return(res_burnt)
}



# SOIL NUTRIENT LOSSES ----------------------------------------------------


get_total_housing_emissions =   function(manure_surplus_fills = FALSE, 
                                         manure_method = 'Method 1', 
                                         nutrient = 'N') {
  
  housing_nh3 = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gas_N_emissions', subfolder = 'NH3', subfolderX2 = 'Housing', 
                              subfolderX3 = 'Total', subfolderX4 = 'Total', pattern = 'Total')
  return(housing_nh3)
}


get_total_storage_emissions = function(manure_surplus_fills = FALSE, 
                                       manure_method = 'Method 1', 
                                       nutrient = 'N') {
  
  yrs = paste0('X',seq(1987,2017))
  
  storage_nh3 = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gas_N_emissions', subfolder = 'NH3', subfolderX2 = 'Storage', 
                                  subfolderX3 = 'Total', subfolderX4 = 'Total', pattern = 'Total')
  storage_n2o = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gas_N_emissions', subfolder = 'N2O', subfolderX2 = 'Storage', 
                                  subfolderX3 = 'Total', subfolderX4 = 'Total', pattern = 'Total')
  storage_nox = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gas_N_emissions', subfolder = 'NOx', subfolderX2 = 'Storage', 
                                  subfolderX3 = 'Total', subfolderX4 = 'Total', pattern = 'Total')
  
  storage = storage_nox
  storage[, yrs] = sapply(yrs, function(x) round(storage_nh3[,x] + storage_n2o[,x] + storage_nox[,x] ,0))
  
  return(storage)
  rm(list=c('storage_nh3','storage_n2o','storage_nox'))
}


get_total_grazing_emissions  =  function(manure_surplus_fills = FALSE, 
                                         manure_method = 'Method 1', 
                                         nutrient = 'N') {
  
  yrs = paste0('X',seq(1987,2017))
  
  grazing_nh3 = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gas_N_emissions', subfolder = 'NH3', subfolderX2 = 'Grazing', 
                                  subfolderX3 = 'Total', subfolderX4 = 'Total', pattern = 'Total')
  grazing_n2o = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gas_N_emissions', subfolder = 'N2O', subfolderX2 = 'Grazing', 
                                  subfolderX3 = 'Total', pattern = 'Total')
  grazing_nox = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gas_N_emissions', subfolder = 'NOx', subfolderX2 = 'Grazing', 
                                  subfolderX3 = 'Total', pattern = 'Total')
  
  grazing = grazing_nox
  grazing[, yrs] = sapply(yrs, function(x) round(grazing_nh3[,x] + grazing_n2o[,x] + grazing_nox[,x] ,0))
  
  return(grazing)
  rm(list=c('grazing_nox','grazing_n2o','grazing_nh3'))
}


get_synthetic_fertiliser_application_emissions = function(manure_surplus_fills = FALSE, 
                                           manure_method = 'Method 1', 
                                           nutrient = 'N') {
  
  yrs = paste0('X',seq(1987,2017))
  if (manure_surplus_fills == TRUE) { folder_div = 'With_ManSurplus'} else { folder_div = 'Without_ManSurplus'}
  
  fert_nh3 = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gas_N_emissions', subfolder = 'NH3', 
                               subfolderX2 = 'Inorganic_fertiliser', subfolderX3 =manure_method, subfolderX4 = folder_div, 
                               subfolderX5 = 'Total', subfolderX6 = 'Total', pattern = 'Total')
  fert_n2o = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gas_N_emissions', subfolder = 'N2O', 
                               subfolderX2 = 'Inorganic_fertiliser', subfolderX3 =manure_method, subfolderX4 = folder_div, 
                               subfolderX5 = 'Total', pattern = 'Inorganic_fertiliser')
  fert_nox = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gas_N_emissions', subfolder = 'NOx', 
                    subfolderX2 = 'Inorganic_fertiliser', subfolderX3 =manure_method, subfolderX4 = folder_div, 
                    subfolderX5 = 'Total',  pattern = 'Inorganic_fertiliser')
  
  fert_emissions = fert_nh3
  fert_emissions[, yrs] = sapply(yrs, function(x) round(fert_nox[,x] + fert_n2o[,x] + fert_nh3[,x] ,0))
  
  return(fert_emissions)
  rm(list=c('fert_nh3','fert_n2o','fert_nox'))
}


get_manure_application_emissions = function(manure_surplus_fills = FALSE, 
                                                          manure_method = 'Method 1', 
                                                          nutrient = 'N') {
  
  yrs = paste0('X',seq(1987,2017))
  if (manure_surplus_fills == TRUE) { folder_div = 'With_ManSurplus'} else { folder_div = 'Without_ManSurplus'}
  
  fert_nh3 = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gas_N_emissions', subfolder = 'NH3', 
                               subfolderX2 = 'Manure', subfolderX3 =manure_method, subfolderX4 = folder_div, 
                               subfolderX5 = 'Total', subfolderX6 = 'Total', pattern = 'Total')
  fert_n2o = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gas_N_emissions', subfolder = 'N2O', 
                               subfolderX2 = 'Manure', subfolderX3 =manure_method, subfolderX4 = folder_div, 
                               subfolderX5 = 'Total', pattern = 'Manure')
  fert_nox = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gas_N_emissions', subfolder = 'NOx', 
                               subfolderX2 = 'Manure', subfolderX3 =manure_method, subfolderX4 = folder_div, 
                               subfolderX5 = 'Total',  pattern = 'Manure')
  
  fert_emissions = fert_nh3
  fert_emissions[, yrs] = sapply(yrs, function(x) round(fert_nox[,x] + fert_n2o[,x] + fert_nh3[,x] ,0))
  
  return(fert_emissions)
  rm(list=c('fert_nh3','fert_n2o','fert_nox'))
}


get_biosolid_application_emissions = function(manure_surplus_fills = FALSE, 
                                            manure_method = 'Method 1', 
                                            nutrient = 'N') {
  
  yrs = paste0('X',seq(1987,2017))
  if (manure_surplus_fills == TRUE) { folder_div = 'With_ManSurplus'} else { folder_div = 'Without_ManSurplus'}
  
  fert_nh3 = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gas_N_emissions', subfolder = 'NH3', 
                               subfolderX2 = 'Biosolid_application', subfolderX3 =manure_method, subfolderX4 = folder_div, 
                               subfolderX5 = 'Total',  pattern = 'Total')
  fert_n2o = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gas_N_emissions', subfolder = 'N2O', 
                               subfolderX2 = 'Biosolid', subfolderX3 =manure_method, subfolderX4 = folder_div, 
                               subfolderX5 = 'Total', pattern = 'Biosolid')
  fert_nox = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gas_N_emissions', subfolder = 'NOx', 
                               subfolderX2 = 'Biosolid', subfolderX3 =manure_method, subfolderX4 = folder_div, 
                               subfolderX5 = 'Total',  pattern = 'Biosolid')
  
  fert_emissions = fert_nh3
  fert_emissions[, yrs] = sapply(yrs, function(x) round(fert_nox[,x] + fert_n2o[,x] + fert_nh3[,x] ,0))
  
  return(fert_emissions)
  rm(list=c('fert_nh3','fert_n2o','fert_nox'))
}

get_crop_residues_emissions = function(manure_surplus_fills = FALSE, 
                                       manure_method = 'Method 1', 
                                       nutrient = 'N') {
  
  if (manure_surplus_fills == TRUE) { folder_div = 'With_ManSurplus'} else { folder_div = 'Without_ManSurplus'}
  
 res_n2o =  get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gas_N_emissions', subfolder = 'N2O', 
                              subfolderX2 = 'Crop_residues', subfolderX3 =manure_method, subfolderX4 = folder_div, 
                              subfolderX5 = 'Total', pattern = 'Crop_residues')
 return(res_n2o)
}


get_runoff_field_application = function(reference_area,
                                        manure_surplus_fills = FALSE, 
                                        manure_method = 'Method 1', 
                                        nutrient = 'N') {
  
  if (manure_surplus_fills == TRUE) { folder_div = 'With_ManSurplus'} else { folder_div = 'Without_ManSurplus'}
  
  if (reference_area == 'Cropland') {
    # minus in grassland
    
    rf_grazing = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Runoff', subfolder = 'Recent_memory', 
                                   subfolderX2 = nutrient, subfolderX3 =manure_method, subfolderX4 = folder_div, 
                                   subfolderX5 = 'Grazing', pattern = 'Total')
    rf_total = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Runoff', subfolder = 'Recent_memory', 
                                 subfolderX2 = nutrient, subfolderX3 =manure_method, subfolderX4 = folder_div, 
                                 subfolderX5 = 'Total', subfolderX6 = 'Total', pattern = 'Total')
   
    yrs = paste0('X',seq(1987,2017))
    rf_cropland = rf_total
    rf_cropland[, yrs] = sapply(yrs, function(x) round(rf_total[,x]-rf_grazing[,x], 0))
  }
  else {
    
    rf_grazing = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Runoff', subfolder = 'Recent_memory', 
                                   subfolderX2 = nutrient, subfolderX3 =manure_method, subfolderX4 = folder_div, 
                                   subfolderX5 = 'Grazing', pattern = 'Total')
    rf_total = rf_grazing
  }
  
  return(rf_total)
}



set_total_environmental_losses = function(reference_area,
                                        manure_surplus_fills = FALSE, 
                                        manure_method = 'Method 1', 
                                        nutrient = 'N') {
  yrs = paste0('X',seq(1987,2017))
  
  if (nutrient == 'P') {
    
    runoff = get_runoff_field_application(reference_area, manure_surplus_fills, manure_method, nutrient)
    total_loss = runoff
  }
  else {
    
    if (reference_area == 'Cropland') {
      storage = get_total_storage_emissions(manure_surplus_fills, manure_method, nutrient)
      housing = get_total_housing_emissions(manure_surplus_fills, manure_method, nutrient)
      fert_app = get_synthetic_fertiliser_application_emissions(manure_surplus_fills, manure_method, nutrient)
      man_app = get_manure_application_emissions(manure_surplus_fills, manure_method, nutrient)
      biosolid_app = get_biosolid_application_emissions(manure_surplus_fills, manure_method, nutrient)
      res_n2o = get_crop_residues_emissions(manure_surplus_fills, manure_method, nutrient)
      runoff = get_runoff_field_application(reference_area, manure_surplus_fills, manure_method, nutrient)
      res_burnt = get_crop_residues_burnt(nutrient)
      
      tot_loss = housing
      tot_loss[, yrs] = sapply(yrs, function(x) round(runoff[,x] + res_n2o[,x] + biosolid_app[,x] + man_app[,x] + fert_app[,x] + storage[,x] + housing[,x] + res_burnt[,x], 0))
      
      rm(list=c('storage','housing','fert_app','man_app','biosolid_app','res_n2o','runoff', 'res_burnt'))
    }
    else {
      
      grazing = get_total_grazing_emissions(manure_surplus_fills, manure_method, nutrient)
      total_loss = grazing
    }
  }
  return(tot_loss)
}
