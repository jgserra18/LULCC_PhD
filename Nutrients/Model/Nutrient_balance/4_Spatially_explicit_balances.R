source('./Nutrients/Model/Nutrient_balance/3_Soil_budget.R')
source('./Main/General_GIS.R')
source('./Nutrients/Model/GIS_computations/LULCC_agriculture/Build_CroplandGrassland.R')




# compute adjusment factors for statistical reference area vs land use reference area ------------------------

compute_reference_area_adjustment_factor = function(reference_area) {
  # this function serves two main purposes:
  # 1 - calculates the total area (in hectare) for each municipality regarding the land-use based reference area
  # 2 - calculates adjustment factors between LU_ref_area and Statistical_ref_area
  # adjustment factors are used to adjust nutrient balance derived data
  # unit: %
  
  ref = ifelse(reference_area == 'Cropland','Arable_land','Grassland')
  # get statistical data on reference areas
  if (reference_area == 'Cropland') {
    
    statistical_ref_area = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Reference_areas', subfolder = 'Arable_land', pattern = 'Arable_land')
  }
  else {
    
    statistical_ref_area = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = 'Pastures', pattern = 'Extensive_pasture')
  }

  # prepare muni shp and store df 
  muni = get_activity_data(module = 'LULCC', folder = 'Admin', pattern = 'Municipality')
  store = get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  
  yrs = paste0('X',seq(1987,2017))

  for (yr in yrs) {
    # compute total land-use based area for the reference area (in hectares)
    r_ref_area =  compute_annual_LULC_cropland(year = yr, spatial_res = '500',LULC = reference_area)
    muni[, yr] = exactextractr::exact_extract(r_ref_area, muni, 'sum') * 5
  }
  
  # store the total land-use based reference area
  names(muni)[1] = 'Muni_ID'
  muni = as.data.frame(muni)
  store = plyr::join(store, muni[, c('Muni_ID',yrs)])

  # compute annual adjustment factors where adj = LU_ref / Stat_ref
  adj_factor = store
  adj_factor[, yrs] = sapply(yrs, function(x) round(store[, x] / statistical_ref_area[, x], 3))
  adj_factor = data_cleaning(adj_factor) # clean the "INf" where Stat_ref is 0 but there is Lu_ref
  names(adj_factor)[1] = 'Admin_id'
  
  # rasterize adjustment factors
  for (yr in yrs) {
    
    r_yr_adj = fasterize_admin_statistical(module = 'LULCC', admin_res = 'Municipality', file = adj_factor, name_id = 'Admin_id',name_field = yr, spatial_res = 100)
    r_yr_adj = resample_to_CLC(module = 'LULCC', raster_file = r_yr_adj, mask_CLC = TRUE, spatial_res = 'Native', ngb = FALSE)
    export_file(module = 'Nutrients', file = r_yr_adj, filename = paste0('Adjusment_factor_', yr), folder = 'Reference_areas', subfolder = 'Adjusment_factor', subfolderX2 = ref)
  }
#  return(adj_factor)
  rm(list=c('ref','statistical_ref_area','muni','store','r_ref_area', 'r_yr_adj'))
}



## SSNB ---------------------------------------------------------------------------

rasterize_nutrient_balances = function(reference_area, nutrient_balance, nutrient = 'N') {
  # creates spatially explicit nutrient balances at the municipality scale according to the LULCC output for crop- or grassland
  # done for the period 1987-2017
  # nutrient balnce = "Soil_balance" or "Land_balance"
  # unit: kg N-P ha-1 yr-1
  
  yrs = paste0('X',seq(1987,2017))
  file_name = ifelse(nutrient_balance == 'Soil_balance','soil_surface_balance','land_balance')
  
  nut_balance = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Nutrient_balances', subfolder = nutrient_balance, subfolderX2 = nutrient, subfolderX3 = reference_area, subfolderX4 = 'Total', pattern = 'soil_surface_balance')
  names(nut_balance)[1] = 'Admin_id'
  
  for (yr in yrs) {
    
    r_nut_balance = fasterize_admin_statistical(module = 'LULCC', admin_res = 'Municipality', file = nut_balance, name_id = 'Admin_id',name_field = yr, spatial_res = 100)
    r_nut_balance = resample_to_CLC(module = 'LULCC', raster_file = r_nut_balance, mask_CLC = TRUE, spatial_res = 'Native', ngb = FALSE)
    
    # get rasterized reference area based on the LULCC module and resample it to 100x100 (native)
    r_ref_area =  compute_annual_LULC_cropland(year = yr, spatial_res = '500',LULC = reference_area)
    r_ref_area = resample_to_CLC(module = 'LULCC', raster_file = r_ref_area, mask_CLC = TRUE, spatial_res = 'Native', ngb = TRUE)

    r_nut_balance = r_nut_balance * r_ref_area

    export_file(module = 'Nutrients', 
                folder = 'Nutrient_balances', 
                subfolder = nutrient_balance, 
                subfolderX2 = nutrient, 
                subfolderX3 = reference_area, 
                subfolderX4 = 'Spatially_explicit',
                file = r_nut_balance, 
                filename = paste0(file_name, '_', yr))
  }
  rm(list=c('nut_balance','r_nut_balance','r_ref_area'))
}
