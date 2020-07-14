source('./Nutrients/Model/Nutrient_balance/3_Soil_budget.R')
source('./Main/General_GIS.R')
source('./Nutrients/Model/GIS_computations/LULCC_agriculture/Build_CroplandGrassland.R')


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
    
    r_nut_balance = fasterize_admin_statistical(module = 'LULCC', admin_res = 'Municipality', file = nut_balance, name_id = 'Admin_id',name_field = yr, spatial_res = )
    r_nut_balance = resample_to_CLC(module = 'LULCC', raster_file = r_nut_balance, mask_CLC = TRUE, spatial_res = 'Native', ngb = FALSE)
    
    # get rasterized reference area based on the LULCC module and resample it to 100x100 (native)
    r_ref_area =  compute_annual_LULC_cropland(year = yr, spatial_res = '500',LULC = reference_area)
    r_fer_area = resample_to_CLC(module = 'LULCC', raster_file = r_ref_area, mask_CLC = TRUE, spatial_res = 'Native', ngb = TRUE)

    r_nut_balance = r_nut_balance * r_fer_area

    export_file(module = 'Nutrients', 
                folder = 'Nutrient_balances', 
                subfolder = nutrient_balance, 
                subfolderX2 = nutrient, 
                subfolderX3 = reference_area, 
                subfolderX4 = 'Spatially_explicit',
                file = r_nut_balance, 
                filename = paste0(file_name, '_', yr))
  }
}
rasterize_nutrient_balances('Cropland','Soil_balance','N')


