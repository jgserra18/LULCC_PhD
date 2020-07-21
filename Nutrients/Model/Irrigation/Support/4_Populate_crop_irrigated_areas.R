source('./Nutrients/Model/Irrigation/Support/4_Populate_crop_irrigated_areas.R')


#* crop irrigated areas per irrigation method are interpolated for other years (Pseudocode):
#* 
#* if there is crop area in this municipality and also in 2009
#*     use X2009_FRACTIOn
#* else if there is crop area but not in 2009
#*     use X2009_FRACTION per agrari                     an region

#* check if the total irrigated crop areas are not higher than crop areas
#* if yes, pass to the next step
#* else correct the total irrigated crop area

## set time period --------------------------------------------------------------------------------------


yrs = paste0('X',seq(1987,2017))



# disaggregate AR fracs ---------------------------------------------------------------------------------

disaggregate_AR_FRAC_irrigation = function(var, main_param, param, irrig_method) {
  
  if (var == 'Area' & missing(irrig_method)==TRUE) {
    AR_FRAC = get_activity_data(module = 'Nutrients',folder = 'Correct_data_Municipality',subfolder = 'Irrigation',subfolderX2 = 'Postprocessing_X2009',subfolderX3 = 'AR_Fraction_crop_areas',subfolderX4 = main_param, pattern = param)   
  }
  else if (var == 'Methods' & missing(irrig_method)==FALSE){
    AR_FRAC = get_activity_data(module = 'Nutrients',folder = 'Correct_data_Municipality',subfolder = 'Irrigation',subfolderX2 = 'Postprocessing_X2009',subfolderX3 = 'AR_Fraction_crop_methods', subfolderX4 = irrig_method, subfolderX5 = main_param, pattern = param)   
  }

  names(AR_FRAC)[1] <- 'agrarian_region_id'
  
  # spatially disaggregate 
  disagg_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Spatial_disaggregation')
  AR_FRAC <- plyr::join(x = disagg_df, y = AR_FRAC, by = 'agrarian_region_id')
  AR_FRAC <- AR_FRAC[, -c(seq(4,8))]
  AR_FRAC = AR_FRAC[, c('Muni_ID','ID','Muni','X2009')]  

  return(AR_FRAC)
}




# 1 - interpolate total crop irrigated areas for the other years using 2009 fractions ------------------------

compute_interpolated_total_crop_irrig_areas = function(main_param, param, tot_irrigated_areas_df) {
  # itnerpolates total crop irrigated areas for all years using 2009 %crop-fraction vs total irrigated area
  # these are not corrected, however
  # unit: hectares
  
  
  FRAC_area_muni = get_activity_data(module = 'Nutrients',folder = 'Correct_data_Municipality',subfolder = 'Irrigation',subfolderX2 = 'Postprocessing_X2009',subfolderX3 = 'Muni_Fraction_crop_areas',subfolderX4 = main_param, pattern = param)   
  
  total_irrig_crop = tot_irrigated_areas_df
  total_irrig_crop[, yrs] = sapply(yrs, function(x) round(tot_irrigated_areas_df[,x] * FRAC_area_muni[, 'X2009'], 0))
  
  return(total_irrig_crop)
  rm(list='FRAC_area_muni')
}


# 2 - correct the interpolated total crop irrigated areas for the other years based on two conditions ----------------------
# maize and potato area is assumed to be the area from Correct_data_Municipality -------------------------------------------

correct_interpolated_total_crop_irrig_areas = function(main_param, param, tot_irrigated_areas_df) {
  # corrects the modelled crop irrigated areas based on the total acreage of that crop 
  # unit: hectares
  
  if (param == 'Maize' | param == 'Potato') {
    
    new_param = ifelse(param == 'Maize' | param == 'Potato', paste0('Irrigated_', tolower(param)), param)
    crop_irrig_area = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = main_param, pattern = new_param)
  }
  else {
    
    # get estimated total crop irrigated for all years
    crop_irrig_area = compute_interpolated_total_crop_irrig_areas(main_param, param, tot_irrigated_areas_df)
    
    # get total crop acreage (rainfed + irrigated)
    total_crop_area = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = main_param, pattern = param)

    # correct crop_irrig_area based on 2 conditions
    # * crop_irrig_area must not be higher than total_crop_area
    # * for rice only: if total_crop_area > 0 and total_irrig_rice == 0, use  AR fractions
    
    for (c in 4:ncol(total_crop_area)) {
      
      for (r in 1:nrow(total_crop_area)) {
        
        if (total_crop_area[r,c] < crop_irrig_area[r,c]) {
          
          print(paste0('Swaping ', crop_irrig_area[r,c], 'for ', total_crop_area[r,c]))
          crop_irrig_area[r,c] = total_crop_area[r,c]

        }
        else if (param == 'Rice' & (total_crop_area[r,c] >0 & crop_irrig_area[r,c] ==0 )) {
          print('Correcting rice.')
          AR_FRAC_irrig_crop = disaggregate_AR_FRAC_irrigation('Areas',main_param,param)
          crop_irrig_area[r,c] = AR_FRAC_irrig_crop[r, 'X2009'] * total_crop_area[r,c]
        }
      }
    }
  }

  return(crop_irrig_area)
  rm(list=c('total_crop_area','AR_FRAC_irrig_crop'))
}


loop_correct_interpolated_total_crop_irrig_areas = function() {
  # unit: hectares
  
  irrig_id = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Irrigation', subfolderX2 = 'Portugal_statistics', pattern = 'Crop_ids')
  total_irrigated_areas = compute_adjusted_irrigated_areas()

  for (i in 1:nrow(irrig_id)) {
    
    main_param = irrig_id[i, 'Main_crop']
    param = irrig_id[i, 'Crop']
    param = ifelse(param == 'Other_industry','Tomato',param)
    
    tot_crop_irrig = correct_interpolated_total_crop_irrig_areas(main_param, param, total_irrigated_areas)
    export_file(module = 'Nutrients', 
                folder = 'Activity_data', 
                subfolder = 'Correct_data_Municipality', 
                subfolderX2 = 'Irrigation', 
                subfolderX3 = 'Crop_irrigated_areas',
                subfolderX4 = 'Total',
                subfolderX5 = main_param, 
                filename = param, 
                file = tot_crop_irrig)
  }
}


# 3 - disaggregated total crop irrigated areas per the different methods ----------------------------------------------


compute_crop_irrigated_area_per_method = function(main_param, param, irrig_method) {
  # disaggregates the total crop irrigated areas 
   
  total_crop_irrig  = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Irrigation', subfolderX2 = 'Crop_irrigated_areas', subfolderX3 = 'Total', subfolderX4 = main_param, pattern = param)
  FRAC_crop_irrig_method = get_activity_data(module = 'Nutrients',folder = 'Correct_data_Municipality',subfolder = 'Irrigation',subfolderX2 = 'Postprocessing_X2009',subfolderX3 = 'Muni_Fraction_crop_methods', subfolderX4 = irrig_method, subfolderX5 = main_param, pattern = param)   
  
  crop_irrig_method = total_crop_irrig 
  crop_irrig_method[, yrs] = sapply(yrs, function(x) round(total_crop_irrig[,x] * FRAC_crop_irrig_method[, 'X2009'], 1))
  
  return(crop_irrig_method)
}


swap_crop_irrigated_area_per_method_X2009 = function(main_param, param, irrig_method, crop_irrig_method_df) {
  # swaps the data of 2009 with the statistical data
  # unit: hectares
  
  
  crop_irrig_method_X2009 = get_activity_data(module = 'Nutrients',folder = 'Correct_data_Municipality',subfolder = 'Irrigation',subfolderX2 = 'Preprocessing_X2009',subfolderX3 = 'Irrigated_areas', subfolderX4 = irrig_method, subfolderX5 = main_param, pattern = param)   
  crop_irrig_method_df[, 'X2009'] = crop_irrig_method_X2009[, 'X2009']
  
  return(crop_irrig_method_df)
}


loop_crop_irrigated_area_per_method = function() {
  
  
  irrig_methods = set_irrigation_methods_INE_codes()[, -2]
  irrig_id = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Irrigation', subfolderX2 = 'Portugal_statistics', pattern = 'Crop_ids')
  
  for (i in 1:nrow(irrig_id)) {
    
    main_param = irrig_id[i, 'Main_crop']
    param = irrig_id[i, 'Crop']
    param = ifelse(param == 'Other_industry','Tomato',param)
    
    for (method in irrig_methods) {
      
      crop_irrig_method = compute_crop_irrigated_area_per_method(main_param, param, method)
      crop_irrig_method  = swap_crop_irrigated_area_per_method_X2009(main_param, param, method, crop_irrig_method)
      
      export_file(module = 'Nutrients', 
                  folder = 'Activity_data', 
                  subfolder = 'Correct_data_Municipality', 
                  subfolderX2 = 'Irrigation', 
                  subfolderX3 = 'Correct_irrigated_areas_method',
                  subfolderX4 = method,
                  subfolderX5 = main_param, 
                  filename = param, 
                  file = crop_irrig_method)
    }
  }
  rm(list=c('irrig_methods','irrig_id','crop_irrig_method'))
}


compute_total_adjusted_irrigated_area = function() {
  # computes the new total irrigated areas per irrigation method and their total
  # unit: hectares
  
  
  # store all df
  tot_irrig <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  tot_irrig[, yrs] = sapply(yrs, function(x) tot_irrig[,x] = 0)
  
  irrig_methods = set_irrigation_methods_INE_codes()[, -2]
  irrig_id = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Irrigation', subfolderX2 = 'Portugal_statistics', pattern = 'Crop_ids')
  
  for (method in irrig_methods) {
    
    tot_method <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
    tot_method[, yrs] = sapply(yrs, function(x) tot_method[,x] = 0)
    
    for (i in 1:nrow(irrig_id)) {
      
      main_param = irrig_id[i, 'Main_crop']
      param = irrig_id[i, 'Crop']
      param = ifelse(param == 'Other_industry','Tomato',param)
      
      crop_irrig_method = get_activity_data(module = 'Nutrients',folder = 'Correct_data_Municipality',subfolder = 'Irrigation',subfolderX2 = 'Correct_irrigated_areas_method',subfolderX3 = method, subfolderX4 = main_param, pattern = param)   
      
      # add to tot_method and tot_irrig
      tot_irrig[, yrs] = sapply(yrs, function(x) tot_irrig[,x] + crop_irrig_method[,x])
      tot_method[, yrs] = sapply(yrs, function(x) tot_method[,x] + crop_irrig_method[,x])
    }
    export_file(module = 'Nutrients', 
                folder = 'Activity_data', 
                subfolder = 'Correct_data_Municipality', 
                subfolderX2 = 'Irrigation', 
                subfolderX3 = 'Correct_irrigated_areas_method',
                subfolderX4 = 'Total',
                subfolderX5 = 'Total', 
                filename = method, 
                file = tot_method)
  }
  export_file(module = 'Nutrients', 
              folder = 'Activity_data', 
              subfolder = 'Correct_data_Municipality', 
              subfolderX2 = 'Irrigation', 
              subfolderX3 = 'Correct_irrigated_areas_method',
              subfolderX4 = 'Total',
              subfolderX5 = 'Total', 
              filename = 'Total', 
              file = tot_irrig)
}




