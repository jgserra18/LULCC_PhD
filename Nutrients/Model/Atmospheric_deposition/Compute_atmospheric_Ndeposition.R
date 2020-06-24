source('./Main/Global_functions.R')
source('./Main/General_GIS.R')
source('./Main/Data_operations.R')



# EMEP ATMOSPHERIC N DEPOSITION 2000-2017 --------------------------------------------------------------------------------------------


compute_EMEP_total_annual_atmospheric_Ndeposition <- function(year) {
  
  atmN_rdn <- get_activity_data(module = 'Nutrients', subfolder = 'Nutrient_params', subfolderX2 = 'N', subfolderX3 = 'Atmospheric_deposition', pattern = paste0(year,'_WDEP_RDN'))
  atmN_oxn <- get_activity_data(module = 'Nutrients', subfolder = 'Nutrient_params', subfolderX2 = 'N', subfolderX3 = 'Atmospheric_deposition', pattern = paste0(year,'_WDEP_OXN'))
  
  atmN_tot <- atmN_rdn + atmN_oxn
  atmN_tot <- atmN_tot / 100 # convert to kg N ha-1 yr-1
  
  return(atmN_tot)
  rm(list=c('atmN_rdn','atmN_oxn'))
}



loop_atmospheric_Ndeposition <- function() {
  
  yrs <- seq(2000,2017)
  
  for (i in yrs) {
    
    atmN <- compute_EMEP_total_annual_atmospheric_Ndeposition(i)
    export_file(module = 'Nutrients', 
                file = atmN, 
                filename = paste0('AtmN_',i), 
                folder = 'Atmospheric_deposition', 
                subfolder = 'N', 
                subfolderX2 = 'EMEP')
  }
}



# SCALE ATMOSPHERIC DEPOSITION ----------------------------------------------------
#* baseline: year 2000


set_atmN_scaling_params = function(manure_surplus_fills_nutDemand = F, manure_method = 'Method 1', nutrient = 'N') {
  # set the params to scale atmospheric deposition 
  
  if (manure_surplus_fills_nutDemand == TRUE) { folder_div = 'With_ManSurplus'} else { folder_div = 'Without_ManSurplus'}
  
  baseline_atmN = get_activity_data(module = 'Nutrients', mainfolder = 'Output', subfolder = 'Atmospheric_deposition', subfolderX2 = 'N', subfolderX3 = 'EMEP', pattern = '_2000')
  baseline_NH3 = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gas_N_emissions', subfolder = 'NH3', subfolderX2 = 'Total', subfolderX3 = manure_method, subfolderX4 = folder_div, subfolderX5 = 'Total', pattern = 'Total')
  baseline_NH3 = compute_temporal_sumIF_admin_df(admin = 'NUTS3', merge_df = baseline_NH3)[, c('Admin_id','X2000')]
  
  return(list(baseline_atmn = baseline_atmN, baselinhe_nh3 = baseline_NH3))
}



scale_atmospheric_Ndeposition = function(year, atmn_params, manure_surplus_fills_nutDemand = F, manure_method = 'Method 1', nutrient = 'N') {
  # scales atmospheric deposition from EMEP (2000-2017) using the year 2000 as baseline
  # scaling is done as the ratio of tot NH3 emisisons for a year before 2000 and the yar 2000
  # @NUTS3 level
  # unit: kg N ha-1 yr-1
  
  if (manure_surplus_fills_nutDemand == TRUE) { folder_div = 'With_ManSurplus'} else { folder_div = 'Without_ManSurplus'}
  
  yr = gsub('X', '', year)
  
  if (yr >= 2000) {
    atmN_yr = get_activity_data(module = 'Nutrients', mainfolder = 'Output', subfolder = 'Atmospheric_deposition', subfolderX2 = 'N', subfolderX3 = 'EMEP', pattern = paste0('_', yr))
  }
  
  else if (yr > 1986 & yr < 2000) {
    
    # set baseline params --------------------------------------
    baseline_atmN = atmn_params[[1]]
    tot_baseline_nh3 = atmn_params[[2]]

    # calculate total agricultural NH3 emissions at the NUTS3 level
    # ALTERNATIVE IMPLEMENT ANOTHER FUNCTION HERE -------------------
    tot_nh3 = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gas_N_emissions', subfolder = 'NH3', subfolderX2 = 'Total', subfolderX3 = manure_method, subfolderX4 = folder_div, subfolderX5 = 'Total', pattern = 'Total')
    tot_nh3 = compute_temporal_sumIF_admin_df(admin = 'NUTS3', merge_df = tot_nh3)
    
    # compute scaling NH3 factor fro atmospheric deposition for a given year
    # tot_NH3 /tot_NH3_baseline
    scaling_factor = tot_nh3
    scaling_factor[, year] = round ( scaling_factor[, year] / tot_baseline_nh3[, 'X2000'] , 3)
    
    # merge with NUTS3 and rasterize it onto a grid
    nuts3_shp =  get_activity_data(module = 'LULCC', folder = 'Admin', pattern = 'NUTS3')
    nuts3_shp = merge(nuts3_shp, scaling_factor[, c('Admin_id', year)])
    
    r_template <- raster(ext=extent(baseline_atmN), crs=crs(baseline_atmN), res = res(baseline_atmN))
    r_scaling_factor <- fasterize::fasterize(sf = nuts3_shp, raster = r_template, field = year)
    
    # scale 2000 data and recalculate atmospheric deposition to a given year
    atmN_yr = baseline_atmN * r_scaling_factor
  }
  
  else {
    stop('Wrong timeline.')
  }
  
  return(atmN_yr)
  rm(list=c('tot_nh3','sum_yr_nh3'))
}



compute_ALL_atmospheric_deposition = function(manure_surplus_fills_nutDemand = F, manure_method = 'Method 1', nutrient = 'N') {
  # this exports the EMEP atmospheric deposition 2000-2017 and for the scaled deposition frrom 1987-1999
  # ofolders are exported based on manure_surplus_fills_nutDemand and manure_method, which heavily influenced total NH3 emissions
  
  if (manure_surplus_fills_nutDemand == TRUE) { folder_div = 'With_ManSurplus'} else { folder_div = 'Without_ManSurplus'}
  
  atmN_params = set_atmN_scaling_params(manure_surplus_fills_nutDemand, manure_method, nutrient)
  
  yrs = paste0('X', seq(1987,2017))
  
  for (yr in yrs) {
    
    yr_atmn = scale_atmospheric_Ndeposition(yr, atmN_params, manure_surplus_fills_nutDemand, manure_method, nutrient)
    yr_atmn = resample_to_CLC(module = 'LULCC', yr_atmn, mask_CLC = T, spatial_res = 'Native')

    export_file(module = 'Nutrients', 
                file = yr_atmn, 
                filename = paste0('AtmN_',yr), 
                folder = 'Atmospheric_deposition', 
                subfolder = 'N', 
                subfolderX2 = manure_method, 
                subfolderX3 = folder_div)
  }
  rm(list=c('atmN_params','yr_atmn'))
}




