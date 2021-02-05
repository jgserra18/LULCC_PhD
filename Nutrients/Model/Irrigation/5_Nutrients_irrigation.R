source('./Nutrients/Model/Irrigation/3_Irrigation_source.R')
source('./Main/General_GIS.R')
source('./Nutrients/Model/GIS_computations/LULCC_agriculture/Build_CroplandGrassland.R')

yrs = paste0('X',seq(1995,2017))
nyrs = seq(1995,2017)


stack_all_no3 = function(source) {
  #' @param source either "gw" or "sw"
  #' @description stacks all sources of nitrate
  #' @return a raster stack with all nitrate source maps for the given period (yrs)
  
  stack_path = paste0('./Nutrients/Output/Irrigation/Nitrate_modelling/', toupper(source), '/Spatial_prediction/')
  stack_path = list.files(path = stack_path, full.names = T, pattern = '.tif')
  
  if (source == 'sw') {
    
    idx_0 = nyrs[1] - 1987 + 1
    stack_path = stack_path[idx_0:length(stack_path)]
  }
  
  if (length(stack_path) != length(yrs)) {
    stop('Number of files differs.')
  }
  else {
    no3_stack = stack(lapply(stack_path, raster))
  }
  
  return(no3_stack)
}




# average municipality values -------------------------


compute_average_NO3 = function(source, write=F) {
  #' @param source either "gw" or "sw"
  #' @description calculates the average NO3, for the specified source, at the municipality level
  #' @note format conversion between store and muni is ensured (ie, different Muni_ID)
  #' @return returns the average NO3 concentration  
  
  store = get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  muni = get_activity_data(module = 'LULCC', folder = 'Admin', pattern = 'Municipality')
  no3_src = stack_all_no3(source)
  
  ctr = 0
  for (yr in yrs) {
    ctr = ctr + 1
    muni[, yr] = round(exactextractr::exact_extract(x = no3_src[[ctr]] , y = muni, fun='mean'), 1)
  }

  muni = as.data.frame(muni)
  muni = muni[, -3] # remove geometry col
  muni$Admin_id = as.character(muni$Admin_id)
 #muni[which(nchar(muni[,1])<4), 1] = paste0('0', muni[which(nchar(muni[,1])<4), 1] ) # correct Muni_ID without 0s
  names(muni)[1] ='Muni_ID'
  
  store = plyr::join(store, muni[, c('Muni_ID', yrs)], 'Muni_ID')
  
  if (write==TRUE) {
    export_file(module = 'Nutrients', 
                folder = 'Irrigation', 
                subfolder = 'Nitrate_modelling', 
                subfolderX2 = toupper(source), 
                subfolderX3 = 'Average_municipality',
                file = store, 
                filename = 'Avg_municipality')
  }
  return(store)
  rm(list=c('muni','muni_src'))
}



#' @note DGADR ARE DISABLED!!!!
compute_irrigation_N_source = function(source, IWW='dynamic', write=F) {
  #' @param source either "gw" or "sw"
  #' @param IWW either static (DGADR) or dynamic (GlobWat)
  #' @description calculates the total irrigation N from a given source
  #' if gw, must converted mg NO3 L-1 to mg N-NO3 L-1 
  #' Irrigation N = FRAC_irrig_source x Irrig_req x NO3_src
  #' @return calculates total irrigation N from a given source (unit: kg N yr-1)

  sub_mod = ifelse(IWW=='dynamic','GlobWat','DGADR')
  
  irrig_src = get_irrigation_source(source)
  no3_src = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Irrigation', subfolder = 'Nitrate_modelling', subfolderX2 = toupper(source), subfolderX3 = 'Average_municipality', pattern = 'Avg_municipality')
  irrig_req = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Irrigation', subfolder = 'Irrigation_requirements', subfolderX2 = sub_mod, subfolderX3 = 'Total', subfolderX4 = 'Total', pattern = 'Total')
  

  conv_fct = 0.001 # conversion factor
  irrig_N = no3_src
  
  if (source == 'gw') {
    
    irrig_N[, yrs] = sapply(yrs, function(x) round(irrig_src[,4]/100 * no3_src[,x] * (11.3/50) * irrig_req[,x] * conv_fct, 1))
  }
  else {
    
    irrig_N[, yrs] = sapply(yrs, function(x) round(irrig_src[,4]/100 * no3_src[,x] * irrig_req[,x] * conv_fct, 1))
  }
  
  irrig_N = irrig_N[, c('Muni_ID','ID','Muni', yrs)]
  
  if (write==TRUE) {
    export_file(module = 'Nutrients', 
                folder = 'Irrigation', 
                subfolder = 'Irrigation_N', 
                subfolderX2 = toupper(source), 
                subfolderX3 = 'Average_municipality',
                file = irrig_N, 
                filename = 'Avg_municipality')
  }
  
  return(irrig_N)
  rm(list=c('irrig_src','no3_src','irrig_req'))
}
#sapply(c('sw','gw'), function(x) compute_irrigation_N_source(x,'dynamic',T))

compute_total_irrigation_N = function(ref_area='ABS', write=F) {
  #' @param ref_area whether the result is given in absolume values ("ABS"), per UAA ("UAA") or per TIA ("TIA")
  #' @description sums irrigation N from gorundwater and surface water
  #' @return returns @description 
  
  sw_irrigN = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Irrigation', subfolder = 'Irrigation_N', subfolderX2 = toupper('sw'), subfolderX3 = 'Average_municipality', pattern = 'Avg_municipality')
  gw_irrigN = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Irrigation', subfolder = 'Irrigation_N', subfolderX2 = toupper('gw'), subfolderX3 = 'Average_municipality', pattern = 'Avg_municipality')
  
  tot_irrigN = gw_irrigN
  tot_irrigN[, yrs] = sapply(yrs, function(x) round(sw_irrigN[,x] + gw_irrigN[,x], 1))
  
  if (ref_area == 'TIA') {
    tia = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Irrigation', subfolderX2 = 'Correct_irrigated_areas_method', subfolderX3 = 'Total', subfolderX4 = 'Total', pattern = 'Total')
    tia = tia[,c('Muni_ID','ID','Muni', yrs)]
  
    tot_irrigN[, yrs] = sapply(yrs, function(x) round(tot_irrigN[,x]/tia[,x], 1)) 
    tot_irrigN = data_cleaning(tot_irrigN)
  }
  else if (ref_area == 'UAA') {
    uaa = get_activity_data(module = 'Nutrients', mainfolder = 'Output', subfolder = 'Reference_area', subfolderX2 = 'UAA', pattern = 'UAA')
    uaa = uaa[,c('Muni_ID','ID','Muni', yrs)]
    
    tot_irrigN[, yrs] = sapply(yrs, function(x) round(tot_irrigN[,x]/uaa[,x], 1)) 
    tot_irrigN = data_cleaning(tot_irrigN)
  }
  
  if (write==TRUE) {
    export_file(module = 'Nutrients', 
                folder = 'Irrigation', 
                subfolder = 'Irrigation_N', 
                subfolderX2 = 'Total', 
                subfolderX3 = 'Average_municipality',
                file = tot_irrigN, 
                filename = paste0('Avg_municipality_',ref_area))
  }
  
  return(tot_irrigN)
  rm(list=c('sw_irrigN','gw_irrigN'))
}

#sapply(c('ABS','UAA','TIA'), function(x) compute_total_irrigation_N(ref_area = x,write = T))




