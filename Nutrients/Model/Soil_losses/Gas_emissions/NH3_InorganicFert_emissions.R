source('./Nutrients/Model/Fertilization/5_InorganicFertiliser.R')

#* 1 - get adjusted crop fertilization rates and get this in kg N yr-1
#* 2 - for the ith fertilizer product:
#* 2.1 - Calculate its fraction in total N applied
#* 2.2 - "Downscale" the application of this fertilizer product using this fraction
#* 3 - Apply correct EF (in g N-Nh3 kg N applied-1)


# FERTILISER PRODUCT NH3 EMISSION FACTOR  --------------------------------------------------------------------

get_soil_pH = function() {
  
  pH = get_activity_data(module = 'LULCC', folder = 'Environmental_params', subfolder = 'Native', pattern = 'pH')
  pH = reclassify(pH, rcl = c(-Inf, 70, 1, # normal pH
                              70, +Inf, 2)) # high pH
  normal_pH = pH == 1
  high_pH = pH == 2
  
  return(list(normal_pH = normal_pH, high_pH = high_pH))
  rm(list='pH')
}

compute_climatic_region = function(climatic_reg) {
  
  climatic_region = get_activity_data(module = 'Nutrients', folder = 'Environmental_params', subfolder = 'Environmental', pattern = 'Climatic_regions')
  
  if (climatic_reg == 'mediterranean') {
    reg = climatic_region == 1
  }
  else if (climatic_reg == 'temperate') {
    reg = climatic_region == 2
  }
  return(reg)
}

get_spatial_explicit_fert_product_NH3_EF = function(fert_prod, nutrient = 'N') {
  # gets spatiall explicit fertiliser NH3 emission factor
  # adds mosaic from temperate and mediterranean EFS for a given fertiliser product
  # unit: g N-NH3 kg N applied-1
  
  
  ef = get_activity_data(module = 'Nutrients', folder = 'Nutrient_params', subfolder = nutrient, subfolderX2 = 'Fertilisers', subfolderX3 = 'EFs', pattern = 'EF_NH3')
  ef = subset(ef, Product == fert_prod)

  normal_pH = get_soil_pH()[[1]]
  high_pH = get_soil_pH()[[2]]
  
  # mediterranean calculations  -----------------------
  
  med_reg = compute_climatic_region('mediterranean')
  mediterranean_normal = normal_pH * med_reg * ef[, 'mediterranean_high']
  mediterranean_high = high_pH * med_reg * ef[, 'mediterranean_high']
  
  med_ef_nh3 = mosaic(mediterranean_normal, mediterranean_high, fun=sum)
  med_ef_nh3 = mask(crop(med_ef_nh3, extent(med_reg)), med_reg)

  rm(list=c('mediterranean_normal','mediterranean_high'))
  
  # temperate calculations --------------------------
  
  temp_reg = compute_climatic_region('temperate')
  temperate_normal = normal_pH * temp_reg * ef[, 'temperate_normal']
  temperate_high = high_pH * temp_reg * ef[, 'temperate_high']
  
  temp_ef_nh3 = mosaic(temperate_normal, temperate_high, fun=sum)
  temp_ef_nh3 = mask(crop(temp_ef_nh3, extent(temp_reg)), temp_reg)

  rm(list=c('temperate_normal','temperate_high', 'temp_reg','med_reg'))
  
  ef_nh3 = mosaic(temp_ef_nh3, med_ef_nh3, fun=sum)
  ef_nh3[ef_nh3==0] = NA
  
  return(ef_nh3)
  rm(list=c('ef','normal_pH','high_pH', 'temp_ef_nh3','med_ef_nh3'))
}


export_spatial_explicit_fert_prod_EF_NH3 = function(nutrient = 'N') {
  # IMPORTANT FUNCTION ------------------------------
  # loops around the spatially explicit NH3 emission factors according to climate and soil pH
  # unit: g N-NH3 kg N applied-1
  
  fert_prod = c('AAN','CAN','AS','N_solutions','Other_Nstraight', 'Urea','AP','NK_compound','NPK_compound','Other_NP')
  
  for (prod in fert_prod) {
    
    EF_prod = get_spatial_explicit_fert_product_NH3_EF(prod)
    export_file(module = 'Nutrients', folder = 'Activity_data', subfolder = 'Nutrient_params', subfolderX2 = nutrient, subfolderX3 = 'Fertilisers', subfolderX4 = 'EFs', subfolderX5 = 'Spatially_explicit', file = EF_prod, filename = prod)
  }
}





# COMPUTE FERTILISER PRODUCT ANNUAL FRACTION AT THE MAINLAND --------------------------------------------------------------------

get_fert_product_names = function(nutrient = 'N') {
  
  IFASTAT_fert_nutrient = get_activity_data(module = 'Nutrients', folder = 'Nutrient_params', subfolder = nutrient, subfolderX2 = 'Fertilisers', subfolderX3 = 'Inorganic_fertiliser', pattern = 'IFASTAT')
  fert_products = unique(IFASTAT_fert_nutrient[, 'Product'])
  
  return(fert_products)
  rm(list = 'IFASTAT_fert_nutrient')
}



get_annual_FRAC_fert_product_total = function(fert_product, year, nutrient = 'N') {
  # calculates the FRACTION of a given fertilizer product (e.g., Urea) compared to the total N consumed in that year
  # unit: %
  
  year = gsub('X','', year)
  IFASTAT_fert_nutrient = get_activity_data(module = 'Nutrients', folder = 'Nutrient_params', subfolder = 'N', subfolderX2 = 'Fertilisers', subfolderX3 = 'Inorganic_fertiliser', pattern = 'IFASTAT')
  IFASTAT_fert_nutrient = IFASTAT_fert_nutrient[IFASTAT_fert_nutrient$Year == year, ]
  
  annual_FRAC = round( IFASTAT_fert_nutrient[IFASTAT_fert_nutrient[, 'Product'] == fert_product, 'Fert_ktN'] / sum(IFASTAT_fert_nutrient[, 'Fert_ktN']) , 3 )
  
  return(annual_FRAC)
  rm(list='IFASTAT_fert_nutrient')
}



# COMPUTE CROP FERTILISER NH3 EMISSIONS PER FERTILISER PRODUCT  --------------------------------------------------------------------

compute_total_crop_fert_N = function(main_param, param, nutrient = 'N', manure_method = 'Method I') {
  # unit: kg N yr-1
  
  crop_fert = get_activity_data(module = 'Nutrients', 
                                mainfolder =  'Output', 
                                folder = 'Fertilisation', 
                                subfolder = nutrient, 
                                subfolderX2 = 'Fertiliser_application_rates',
                                subfolderX3 = manure_method, 
                                subfolderX4 = main_param, 
                                pattern = param)
  crop_area = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = main_param, pattern = param)
  
  yrs = paste0('X', seq(1987,2017))
  
  crop_fert[, yrs] = sapply(yrs, function(x)crop_fert[, x] = round(crop_fert[, x] * crop_area[, x], 1))
  
  return(crop_fert)
}



downscale_crop_fert_product_app_rates = function(main_param, param, fert_product, nutrient = 'N', manure_method = 'Method I') {
  # downscale crop fertilization application rates (adjusted) based on the fraction of a given fert_product 
  # unit: kg nutrient yr-1

  
  yrs = paste0('X', seq(1987,2017))
  crop_fert_N = compute_total_crop_fert_N(main_param, param, nutrient, manure_method)
  

  crop_fert_N[, yrs] = sapply(yrs, function(x) {
    
    FRAC_fert_prod = get_annual_FRAC_fert_product_total(fert_product, x)
    crop_fert_N[, x] = round(crop_fert_N[, x] * FRAC_fert_prod, 1)
  })
  
  return(crop_fert_N)
  rm(list=c('yrs','FRAC_fert_prod'))
}


downscale_crop_fert_product_app_NH3 = function(main_param, param, fert_product, nutrient = 'N', manure_method = 'Method I') {
  
  # prepare crop fert product for further rasterization
  crop_fert_product = downscale_crop_fert_product_app_rates(main_param, param, fert_product)
  names(crop_fert_product)[1] = 'Admin_id'
  
  EF_NH3 = get_spatial_explicit_fert_product_NH3_EF(fert_prod)
  
  yrs = paste0('X', seq(1987,2017))
  
  for 
  crop_fert_product = fasterize_admin_statistical(module = 'LULCC', admin_res = 'Municipality', file = crop_fert_product, name_id = 'Admin_id', name_field = yr, spatial_res = 500)
  EF_NH3 = get_spatial_explicit_fert_product_NH3_EF(fert_prod)
  
}






# COMPUTE CROP FERTILISER NH3 EMISSIONS PER FERTILISER PRODUCT@MUNICIPALITY APPROACH  --------------------------------------------------------------------

source('./Main/General_GIS.R')


compute_annual_ph_cropland = function(year, muni_shp, r_pH) {
  # NOT USED CURRENTLY  -------------------------
  
  yr_cropland =  compute_annual_LULC_cropland(year)
  yr_cropland = resample(yr_cropland, r_pH, 'ngb')
  
  avg_ph_cropland_muni = exactextractr::exact_extract(r_pH, muni_shp, 'mean', include_cell = TRUE)
  
  return(avg_ph_cropland_muni)
}


loop_annual_ph_cropland = function() {
  # NOT USED CURRENTLY  -------------------------
  # because the soil pH annual variation is 0, only one year is considered to speed up the process
  # unit: avg pH per municipality 
  
  muni = get_activity_data('LULCC','Admin','Municipality')
  
  ph = get_activity_data(module = 'LULCC', folder = 'Environmental_params', subfolder = 'Native', pattern = 'pH')
  ph = resample_to_CLC(module = 'LULCC', raster_file = ph, mask_CLC = F, spatial_res = 'Native', ngb = TRUE)
  
  store_ph = get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  store_ph[, 'avg_ph'] = round(compute_annual_ph_cropland(year = 'X2000', muni_shp = muni, r_pH = ph)/10, 1)
  
  store_ph[store_ph$avg_ph>7, 'avg_ph'] = 'high_ph'
  store_ph[store_ph$avg_ph<=7, 'avg_ph'] = 'normal_ph'
  
  return(store_ph)
}



select_statistical_muni_fert_product_EF_NH3 = function(fert_product, muni_shp) {
  # calculates average emission factor for each municipality and for a given fert product
  # unit: g N-NH3 kg N applied-1
  
  spatial_fert_prod_EF = get_activity_data(module = 'Nutrients', folder = 'Nutrient_params', subfolder = 'N', subfolderX2 = 'Fertilisers', subfolderX3 = 'EFs', subfolderX4 = 'Spatially_explicit', pattern = fert_product)
  EF = get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  EF[, paste0('EF_',fert_product)] = round(exactextractr::exact_extract(spatial_fert_prod_EF, muni_shp, 'mean'), 0)
  
  return(EF)
  rm(list='spatial_fert_prod_EF')
}



compute_statistical_muni_crop_fert_NH3 = function(main_param, param, fert_product, muni_EF, nutrient = 'N', manure_method = 'Method I') {
  # computes NH3 emissions following crop fertiliser application using the statistical approach
  # unit: kg N-NH3 yr-1
  
  crop_fert_prod_N = downscale_crop_fert_product_app_rates(main_param, param, fert_product, nutrient, manure_method)
  crop_fert_prod_NH3 = crop_fert_prod_N
  
  yrs = paste0('X', seq(1987,2017))
  crop_fert_prod_NH3[, yrs] = sapply(yrs, function(x) round(crop_fert_prod_N[, x] / 1000 * muni_EF, 1))
  
  return(crop_fert_prod_NH3)
  rm(list=c('crop_fert_prod_N','yrs'))
}


loop_statistical_muni_fert_NH3 = function(fert_product, nutrient = 'N', manure_method = 'Method I') {
  # for a given fertiliser product (e.g., Urea) compute Nh3 emissions from all crops!
  # unit: kg N-NH3 yr-1
  
  standard_params <- get_standard_params_list(main_param = 'Crops')
  muni = get_activity_data('LULCC','Admin','Municipality')
  EF_prod = select_statistical_muni_fert_product_EF_NH3(fert_product, muni)[, paste0('EF_', fert_product)]
    
  for (i in 1:nrow(standard_params)) {
    
    main_param = standard_params[i, 'Main_crop']
    param = standard_params[i, 'Crop']
    
    if (param == 'Extensive_pasture') {
      next
    }
    crop_fert_prod_NH3 = compute_statistical_muni_crop_fert_NH3(main_param, param, fert_product, EF_prod)
    export_file(module = 'Nutrients', 
                file = crop_fert_prod_NH3, 
                filename = param, 
                folder = 'Gas_N_emissions', 
                subfolder = 'NH3', 
                subfolderX2 = 'Inorganic_fertiliser',
                subfolderX3 = fert_product, 
                subfolderX4 = main_param)
  }
  rm(list=c('standard_params','muni','EF_prod','main_param','param','crop_fert_prod_NH3'))
}


loop_all_fert_prod_NH3 = function(nutrient = 'N', manure_method = 'Method I') {
  
  fert_prod = c('AAN','CAN','AS','N_solutions','Other_Nstraight', 'Urea','AP','NK_compound','NPK_compound','Other_NP')
  sapply(fert_prod, function(x) loop_statistical_muni_fert_NH3(x))
}



compute_total_crop_fert_prod_NH3 = function(nutrient = 'N', manure_method = 'Method I') {
  
  standard_params <- get_standard_params_list(main_param = 'Crops')
  
  for (i in 1:nrow(standard_params)) {
    
    main_param = standard_params[i, 'Main_crop']
    param = standard_params[i, 'Crop']
    
    if (param == 'Extensive_pasture') {
      next
    }
    
    yrs = paste0('X', seq(1987,2017))
    store_crop_nh3 = get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
    store_crop_nh3[, yrs] = sapply(yrs, function(x) store_crop_nh3[,x] = 0)
    
    fert_prod = c('AAN','CAN','AS','N_solutions','Other_Nstraight', 'Urea','AP','NK_compound','NPK_compound','Other_NP')
    
    for (fert in fert_prod) {
      
      crop_fert_prod_nh3 = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gas_N_emissions', subfolder = 'NH3', subfolderX2 = 'Inorganic_fertiliser', subfolderX3 = fert, subfolderX4 = main_param, pattern = param)
      store_crop_nh3[, yrs] = sapply(yrs, function(x) round(store_crop_nh3[,x] + crop_fert_prod_nh3[, x], 1))
    }
    # export totals
    export_file(module = 'Nutrients', 
                file = store_crop_nh3, 
                filename = param, 
                folder = 'Gas_N_emissions', 
                subfolder = 'NH3', 
                subfolderX2 = 'Inorganic_fertiliser',
                subfolderX3 = 'Total', 
                subfolderX4 = main_param)
  }
}
