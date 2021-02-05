source('./Main/General_GIS.R')

yrs = paste0('X',seq(1995,2017))
r_tmp = raster(crs=CRS('+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs '), 
               res=c(3440,4480),
               nrows=127, ncols=100, 
               xmn=2634637, xmx=2978637, ymn=1728785, ymx=2297745)

# CALENDARS FOR IRRIGATION DEMAND ---------------------------------------------------------------------

cropIrrigatedAreas_municipality = function(main_param, param, yr, method = NULL) {
  #' @param main_param main crop
  #' @param param crop
  #' @param yr year as 'X1999'
  #' @param method by default NULL, related to irrigation methods (drip, furrows,gun, microsprink, other_gravity, pivot, sprinkler)
  #' @description 1 if crop ia exists, otherwise 0
  
  subX2_mod = ifelse(missing(method)==F, 'Correct_irrigated_areas_method','Crop_irrigated_areas') # subfolderX2 modifier 
  subX3_mod = ifelse(missing(method)==F, method,'Total') # subfolderX3 modifier
  
  crop_ia = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Irrigation',subfolderX2 = subX2_mod, subfolderX3 = subX3_mod, subfolderX4 = main_param, pattern = param)
  crop_ia[, yrs] = ifelse(crop_ia[, yrs]>1, 1, 0)
  crop_ia = crop_ia[, c('Muni_ID', yr)]
  names(crop_ia)[1] = 'Admin_id'
  
  return(crop_ia)
}

get_monthly_cropKc = function(param, month) {
  #' @param param crop
  #' @param month integer 1-12
  #' @description gets the kc value for a given crop andmonth
  #' @usage get_monthly_cropKc('Wheat',1)
  
  if (!is.numeric(month)==T) { stop('Month is 1-12, do not include the X.')}
  
  calendar = get_activity_data(module = 'Water', folder = 'Other_params', subfolder = 'Tabled',pattern = 'calendar')
  calendar = calendar[which(calendar$Crop==param), paste0('M',month)]
  return(calendar)
}

rasterize_monthly_cropKC = function(main_param, param, month, yr) {
  #' @param main_param main crop
  #' @param param crop
  #' @param month integer 1-12
  #' @param yr year as 'X1999'
  #' @description rasterizes the monthly crop kc for a given crop and year
  
  ia_exists = cropIrrigatedAreas_municipality(main_param, param, yr)
  month_kc = get_monthly_cropKc(param, month)
  ia_exists[, yr] = ia_exists[, yr] * month_kc
  
  crop_month_kc = fasterize_admin_statistical(module = 'LULCC', admin_res = 'Municipality',file = ia_exists, name_id = 'Admin_id', name_field = yr, spatial_res = 500)
  crop_month_kc = resample(crop_month_kc, r_tmp, 'ngb')
  
  return(crop_month_kc)
  rm(list=c('ia_exists','month_kc'))
}



# CALENDARS FOR IRRIGATION WITHDRAWAL ------------------------------------------------

set_irrigMethods_efficiency = function(method) {
  #' @source  Leão, P., 2011. Metodologia Para a Estimativa De Portugal Mecar - Methodology To Estimate the Irrigation Water Consumption in.
  #' @description returns irrigation efficiency (I_eff)
  
  irrig_methods = c('drip','furrows','gun','microsprink','other_gravity','pivot','sprinkler')
  irrig_effs = c(0.9, 0.7, 0.7, 0.85, 0.8, 0.85, 0.75)
  return(irrig_effs[which(irrig_methods==method)])
}

rasterize_cropIrrigAreasMethods = function(main_param, param, yr, method) {
  #' @param main_param main crop
  #' @param param crop
  #' @param yr year as 'X1999'
  #' @param method by default NULL, related to irrigation methods (drip, furrows,gun, microsprink, other_gravity, pivot, sprinkler)
  #' @description 1 if crop ia exists, otherwise 0
  #' @return NULL if no method is used for a given crop; otherwise return spatially explicit irrigation efficiency (0-1)
  #' @usage rasterize_cropIrrigAreasMethods('Cereals','Maize','X1999','drip')
  
  I_eff = set_irrigMethods_efficiency(method) # irrigation efficiency
  ia_exists = cropIrrigatedAreas_municipality(main_param, param, yr, method)
  
  if (sum(ia_exists[,yr])==0) { 
    
  }
  else {
    
    ia_exists[, yr] = ia_exists[, yr] * I_eff
    I_eff = fasterize_admin_statistical(module = 'LULCC', admin_res = 'Municipality',file = ia_exists, name_id = 'Admin_id', name_field = yr, spatial_res = 500)
    I_eff = resample(I_eff, r_tmp, 'ngb')
    I_eff[I_eff==0] = NA
    
    return(I_eff)
    rm(list='ia_exists')
  }
}



