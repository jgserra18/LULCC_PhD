source('./Main/Global_functions.R')


## IPCC report - https://www.ipcc-nggip.iges.or.jp/public/2019rf/pdf/4_Volume4/19R_V4_Ch11_Soils_N2O_CO2.pdf 


# IPCC 2019 REFINEMENT METHOD ----------------------------------------------------


# set calculation method for the gas --- 
set_gas_Nloss = "N2O"


#* // uniformizar com as areas para os EFs de NH3

compute_annual_climatic_typology_muni = function() {
  # * Prec/ET0 > 1 - wet climate
  # * Prec/ET0 < 1 - dry climate
  
  yrs = paste0('X',seq(1987,2017))
  muni <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  muni[, yrs] <- sapply(yrs, function(x) muni[,x] = 0)
  
  muni_shp = get_activity_data(module = 'LULCC', subfolder = 'Admin', pattern = 'Municipality')

  for (yr in yrs) {
    
    prec = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Environmental_params', subfolder = 'Climatic', subfolderX2 = 'Prec', subfolderX3 = '1x1km', pattern = yr)
    et0 = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Environmental_params', subfolder = 'Climatic', subfolderX2 = 'ET0', subfolderX3 = '1x1km', pattern = yr)
    
    climate = prec/et0>1
    
    muni_shp[, yr] = exactextractr::exact_extract(climate, muni_shp, 'sum')
    muni[, yr] = round(muni_shp[, yr][[1]], 0)
    muni[, yr] = ifelse(muni[, yr]>1, 'Wet','Dry')
  }
  
  export_file(module = 'Nutrients', 
              file = muni, 
              filename = 'IPCC2019_ClimaticRegions', 
              folder = 'Activity_data', 
              subfolder = 'Environmental_params', 
              subfolderX2 = 'Climatic',
              subfolderX3 = 'Other_params')
}


# general func to compute climatic disaggregated N2O emissions using IPCC Tier 2 ----------

set_IPCC_N2O_EFs = function(N_source, main_param) {
  # sets the IPCC N2O efs for different N sources
  # N_source is synthetic_fertiliser, manure, etc
  # main_param is to distinguish grazing animals (cattle, poultry and pigs vs other animals)
  # unit: kg N-N2O (kg N applied)-1 yr-1
  
  EF_df = data.frame(climate=c('Dry','Wet'))
  
  if (N_source == 'Inorganic_fertiliser') {
    
    EF_df[, 'EF'] = c(0.005, 0.016)
  }
  else if (N_source == 'Biosolid' | N_source == 'Crop_residues' | N_source == 'Irrigation' | N_source == 'Manure') {
    
    EF_df[, 'EF'] = c(0.005, 0.006)
  }
  else if (N_source == 'Grazing' & (main_param == 'Bovine' | main_param == 'Pigs' | main_param == 'Poultry')) {
    
    EF_df[, 'EF'] = c(0.002, 0.006)
  }
  else if (N_source == 'Grazing' & (main_param != 'Bovine' & main_param != 'Pigs' & main_param != 'Poultry')) {
    
    EF_df[, 'EF'] = c(0.003, 0.003)
  }
  
  return(EF_df)
}


prepare_climatic_disaggregated_IPCC_EFs = function(N_source, main_param) {
  
  ef = set_IPCC_N2O_EFs(N_source, main_param)
  climatic_regions = get_activity_data(module = 'Nutrients', folder = 'Environmental_params', subfolder = 'Climatic', subfolderX2 = 'Other_params', pattern = 'IPCC2019')
  
  # set N2O EFs based on climtic region per municipality
  climatic_regions[which(climatic_regions=='Wet', arr.ind = T)] = ef[2,2]
  climatic_regions[which(climatic_regions=='Dry', arr.ind = T)] = ef[1,2]
  
  return(climatic_regions)
}


general_func_compute_direct_emissions = function(N_source, N_source_df, main_param) {
  # general func to compute direct N2O emissions from the different N inputs
  # unit: kg N-N2O yr-1
  
  yrs = paste0('X',seq(1987,2017))
  
  if (set_gas_Nloss == "N2O") {
    print('Calculating for N2O ========')
    ef = prepare_climatic_disaggregated_IPCC_EFs(N_source, main_param)
    ef[, yrs] = sapply(yrs, function(x) as.numeric(ef[, x]))
    # calculate direct N2O emission --
    
    N_source_df[, yrs] = sapply(yrs, function(x) round(N_source_df[, x] * ef[, x], 1))
    
  }
  else {
    print('Calculating for NOx ========')
    ef = 0.04 * 0.3 # kg N-NO2 (kg N applied)-1 yr-1
    N_source_df[, yrs] = sapply(yrs, function(x) round(N_source_df[, x] * ef, 1))
  }
  
  return(N_source_df)
}




# synthetic fertiliser ----------------------------------------------

compute_synthetic_fertiliser_N2O_emissions = function(N_source = 'Inorganic_fertiliser', 
                                                      write = FALSE, 
                                                      manure_surplus_fills = FALSE, 
                                                      manure_method = 'Method 1', 
                                                      nutrient = 'N') {
  # direct N2O emissions from synthetic fertiliser
  # unit: kg N-N2O yr-1
  
  if (manure_surplus_fills == TRUE) { folder_div = 'With_ManSurplus'} else { folder_div = 'Without_ManSurplus'}
  
  fertN = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Fertilisation', subfolder = 'N', 
                                    subfolderX2 = 'Inorganic_fertiliser', subfolderX3 = manure_method, subfolderX4 = folder_div, 
                                    subfolderX5 = 'Total', subfolderX6 = 'Adjusted', pattern = 'Adjusted')
  # calculate N2O emsisions
  fertN2O = fertN
  fertN2O = general_func_compute_direct_emissions(N_source = N_source, N_source_df = fertN2O)
  
  if (write == TRUE) {
    export_file(module = 'Nutrients', 
                file = fertN2O, 
                filename = 'Inorganic_fertiliser', 
                folder = 'Gas_N_emissions', 
                subfolder = set_gas_Nloss, 
                subfolderX2 = 'Inorganic_fertiliser',
                subfolderX3 = manure_method, 
                subfolderX4 = folder_div,
                subfolderX5 = 'Total')
  }
  
  return(fertN2O)
}

# other N inputs -----------------------------------------------------

compute_other_Ninputs_N2O_emissions = function(N_source,
                                               write = FALSE, 
                                               manure_surplus_fills = FALSE, 
                                               manure_method = 'Method 1', 
                                               nutrient = 'N') {
  
  if (manure_surplus_fills == TRUE) { folder_div = 'With_ManSurplus'} else { folder_div = 'Without_ManSurplus'}
  
  
  if (N_source == 'Biosolid') {
    
    N_inp = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Fertilisation', 
                              subfolder = 'N', subfolderX2 = 'Biosolids', subfolderX3 = 'Total', pattern = 'Total')
  }
  else if (N_source == 'Crop_residues') {
    
    N_inp = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Crop_residues', subfolder = 'Left', subfolderX2 = 'N', subfolderX3 = 'Total', pattern = 'Total')
  }
  else if (N_source == 'Manure') {
    
    N_inp = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Fertilisation', 
                              subfolder = 'N', subfolderX2 = 'Biosolids', subfolderX3 = 'Total', pattern = 'Total')
  }
  
  
  N2O_inp = N_inp
  N2O_inp = general_func_compute_direct_emissions(N_source = N_source, N_source_df = N2O_inp)
  
  
  if (write == TRUE) {
    export_file(module = 'Nutrients', 
                file = N2O_inp, 
                filename = N_source, 
                folder = 'Gas_N_emissions', 
                subfolder = set_gas_Nloss, 
                subfolderX2 = N_source,
                subfolderX3 = manure_method, 
                subfolderX4 = folder_div,
                subfolderX5 = 'Total')
  }
  
  return(N2O_inp)
}


compute_grazing_N2O_emissions = function(N_source = 'Grazing', write = FALSE) {
  # computes the total N2O emissions from grazing 
  # unit: kg N-N2O yr-1
  
  main_params = c('Bovine','Equides','Goats','Pigs','Poultry','Sheep','Rabbits')
  
  # store 
  yrs = paste0('X', seq(1987,2017))
  store = get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  store[, yrs] = sapply(yrs, function(x) store[,x] = 0)
  
  for (i in main_params) {
    
    grazN = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gross_manure', subfolder = 'N', subfolderX2 = 'Grazing', subfolderX3 = 'Total', subfolderX4 = 'Total', pattern = i)
    
    grazN2O = grazN
    grazN2O = general_func_compute_direct_emissions(N_source = N_source, N_source_df = grazN2O, main_param = i)
    
    if (write == TRUE) {
      export_file(module = 'Nutrients', 
                  file = grazN2O, 
                  filename = i, 
                  folder = 'Gas_N_emissions', 
                  subfolder = set_gas_Nloss, 
                  subfolderX2 = 'Grazing',
                  subfolderX3 = 'Total') 
    }
    
    
    # add to store
    store[, yrs] = sapply(yrs, function(x) store[,x] + grazN2O[, x])
  }
  
  
  if (write == TRUE) {
    export_file(module = 'Nutrients', 
                file = store, 
                filename = 'Total', 
                folder = 'Gas_N_emissions', 
                subfolder = set_gas_Nloss, 
                subfolderX2 = 'Grazing',
                subfolderX3 = 'Total') 
  }
  
  return(store)
  rm(list=c('grazN','grazN2O'))
}



compute_total_direct_N2O_emissions = function(write = FALSE,
                                              manure_surplus_fills = FALSE, 
                                              manure_method = 'Method 1', 
                                              nutrient = 'N') {
  # computes total diret N2O emissions
  # unit: kg N-N2O yr-1
  
  if (manure_surplus_fills == TRUE) { folder_div = 'With_ManSurplus'} else { folder_div = 'Without_ManSurplus'}
  yrs = paste0('X',seq(1987,2017))
  
  grazing = compute_grazing_N2O_emissions(write = write)
  biosolid = compute_other_Ninputs_N2O_emissions('Biosolid', write, manure_surplus_fills, manure_method)
  res = compute_other_Ninputs_N2O_emissions('Crop_residues', write, manure_surplus_fills, manure_method)
  man = compute_other_Ninputs_N2O_emissions('Manure', write, manure_surplus_fills, manure_method)
  fert = compute_synthetic_fertiliser_N2O_emissions(write = write, manure_surplus_fills = manure_surplus_fills, manure_method = manure_method)
  
  tot_N2O = fert
  tot_N2O[, yrs] = sapply(yrs, function(x) fert[,x] + man[, x] + res[,x] + biosolid[,x] + grazing[,x])
  
  
  if (write == TRUE) {
    export_file(module = 'Nutrients', 
                file = tot_N2O, 
                filename = 'Total', 
                folder = 'Gas_N_emissions', 
                subfolder = set_gas_Nloss, 
                subfolderX2 = 'Total',
                subfolderX3 = manure_method, 
                subfolderX4 = folder_div,
                subfolderX5 = 'Total')
  }
  
  return(tot_N2O)
  rm(list=c('grazing','biosolid','res','man','fert'))
}

