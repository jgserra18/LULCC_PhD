source('./Main/Global_functions.R')
source('./Nutrients/Model/Crop_production/get_fodder_crops_residues.R')
source('./Nutrients/Model/Atmospheric_deposition/Compute_atmospheric_Ndeposition.R')
source('./Nutrients/Model/BNF/Compute_BNF.R')

# land budget = N inputs - N outputs
# unit: kg N-P yr-1 





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



# COMPUTE LAND BUDGET -----------------------------------

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


  