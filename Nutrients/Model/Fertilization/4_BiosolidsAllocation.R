source('./Main/Global_functions.R')
source('./Nutrients/Model/Fertilization/3_CropManureAllocation.R')
source('./Nutrients/Model/Fertilization/2_Biosolids.R')


# SLUDGE NUTRIENT DISTRIBUTION PRINCIPLES
#* Because there aren't any specifications on biosolid crop application in Portugal, Serra et al 2019 crop assumptions are here applied
#* The algorithm used to allocate biosolid nutrient to crops is based on the relative area fraction in arable land


get_sludge_distribution_crops = function() {
  # alternative function to get_standard_params_list(main_param = 'Crops')
  
  params = get_standard_params_list(main_param = 'Crops')
  params = subset(params, Main_crop != 'Horticulture' & Main_crop != 'Industry_crops' & Crop != 'Extensive_pasture')
  
  return(params)
}


compute_biosolid_crop_application_fraction = function(main_param, param) {
  
  arable_application_area = compute_arable_land_sludge_application()
  crop_area = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = main_param, pattern = param)
  
  yrs = paste0('X', seq(1987,2017))
  FRAC_crop_app = crop_area
  FRAC_crop_app[, yrs] = sapply(yrs, function(x) round( crop_area[, x] / arable_application_area[, x], 2))
  FRAC_crop_app = data_cleaning(FRAC_crop_app)
  
  return(FRAC_crop_app)
  rm(list=c('arable_application_area', 'crop_area','yrs'))
}


d =   biosolid_nutrient_muni = compute_updated_distributed_sludge_nutrient_content('N')
View(d)
compute_biosolid_crop_application = function(nutrient, main_param, param) {
  
  FRAC_crop_app = compute_biosolid_crop_application_fraction(main_param, param)
  biosolid_nutrient_muni = compute_updated_distributed_sludge_nutrient_content(nutrient)

  yrs = paste0('X', seq(1987,2017))
  crop_biosolid_nutrient_muni = biosolid_nutrient_muni
  crop_biosolid_nutrient_muni[, yrs] = sapply(yrs, function(x) round( biosolid_nutrient_muni[, x] * 1000 * FRAC_crop_app[,x] , 1 ))
  
  return(crop_biosolid_nutrient_muni)
  rm(list=c('FRAC_crop_app','biosolid_nutrient_muni','yrs'))
}



method1_crop_nutrientReq_minus_Biosolid = function(nutrient, main_param, param) {
  
  biosolid_crop_app = compute_biosolid_crop_application(nutrient, main_param, param)
  rem_nutrient_req = calc1_crop_nutrientReq_minus_Man(nutrient, main_param, param)[[1]]
  
  biosolid_crop_surplus = biosolid_crop_app
  yrs = paste0('X', seq(1987,2017))
  biosolid_crop_surplus[, yrs] = sapply(yrs, function(x) biosolid_crop_surplus[, x] = 0)
  
  for (i in 1:nrow(rem_nutrient_req)) {
    
    for (j in 4:ncol(rem_nutrient_req)) {
      
      if ( rem_nutrient_req[i,j] < biosolid_crop_app[i,j] ) {
        
        biosolid_crop_surplus[i,j] = round (  biosolid_crop_app[i,j] - rem_nutrient_req[i,j], 1)
        rem_nutrient_req[i,j] = 0
      }
      
      else if ( rem_nutrient_req[i,j] == biosolid_crop_app[i,j] ) {
        
        biosolid_crop_surplus[i,j] = 0
        rem_nutrient_req[i,j] = 0
      }
      else {
        
        rem_nutrient_req[i,j] = round ( rem_nutrient_req[i,j] - biosolid_crop_app[i,j] , 1)
        biosolid_crop_surplus[i,j] = 0
      }
    }
  }
  
  return(list(rem_nutrient_req = rem_nutrient_req, biosolid_crop_surplus = biosolid_crop_surplus ))
  rm(list=c('biosolid_crop_app','yrs'))
}


compute_biosolid_crop_application_rate = function(nutrient, main_param, param) {
  
  man_nutrient_req = calc1_crop_nutrientReq_minus_Man(nutrient, main_param, param)[[1]]
  rem_nutrient_req = method1_crop_nutrientReq_minus_Biosolid(nutrient, main_param, param)[[1]]
  area = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = main_param, pattern = param)

  yrs = paste0('X', seq(1987,2017))
  man_nutrient_req[, yrs] = sapply(yrs, function(x) round( (man_nutrient_req[, x] - rem_nutrient_req[, x] ) / area [, x] , 1))
  man_nutrient_req = data_cleaning(man_nutrient_req)
  
  return(man_nutrient_req)
  rm(list=c('man_nutrient_req','rem_nutrient_req','area','yrs'))
}

d = compute_biosolid_crop_application_rate('N','Cereals','Irrigated_maize')
View(d)
allocate_biosolid_crop = function(nutrient) {
  
  biosolid_crops = get_sludge_distribution_crops()
  
  # store main dataframe to update biosolid_surplus after crop application
  yrs = paste0('X', seq(1987,2017))
  main_biosolid_surplus = get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  main_biosolid_surplus[, yrs] = sapply(yrs, function(x) main_biosolid_surplus[,x] <- 0)
  
  for (i in 1:nrow(biosolid_crops)) {
    
    main_param = biosolid_crops[i, 'Main_crop']
    param = biosolid_crops[i, 'Crop']
    
    crop_rem_nutrient = method1_crop_nutrientReq_minus_Biosolid(nutrient, main_param, param)[[1]]
    biosolid_surplus = method1_crop_nutrientReq_minus_Biosolid(nutrient, main_param, param)[[2]]
    crop_biosolid_rate = 
    
    # update biosolid surplus
    main_biosolid_surplus[, yrs] = sapply(yrs, function(x) round(main_biosolid_surplus[, x] + biosolid_surplus[, x], 1))
  }
  
}

