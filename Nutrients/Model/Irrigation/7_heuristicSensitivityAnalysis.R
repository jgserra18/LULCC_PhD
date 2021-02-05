source('./Nutrients/Model/Irrigation/5_Nutrients_irrigation.R')


# INDIVIDUAL HEURISTIC SA FOR ALL N_IRRIG PARAMS ------------------------------------------------------------------

modify_irrigation_N_params = function(param, param_modifier = 0.25, IWW = 'dynamic') {
  #' @param param either gw_src (for nitrate concentration), no3_gw, no3_sw, IWR or All (no3_gw and sw and IWR)
  #' @param IWW either dynamic (Glbowat) or static (DGADR)
  #' @note when changing gw_src, the complementary fraction for sw is the inverse
  #' @param param_modifier integer to modify the baseline vars
  #' @description modifies the baseline vars to calculate Nirrig; basis for the heuristic sensitivity analysis
  #' @usage modify_irrigation_N_params("no3_gw", 0.25)
  #' @usage modify_irrigation_N_params("All", 0.25)
  #' @details IGNORE GW_SRC !!!!!!!!!!!!!!!!!!!!!!!
  
  sub_mod = ifelse(IWW=='dynamic','GlobWat','DGADR')
  
  gw_src = get_irrigation_source('gw') # irrig_rc
  no3_gw = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Irrigation', subfolder = 'Nitrate_modelling', subfolderX2 = toupper('gw'), subfolderX3 = 'Average_municipality', pattern = 'Avg_municipality')
  no3_sw = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Irrigation', subfolder = 'Nitrate_modelling', subfolderX2 = toupper('sw'), subfolderX3 = 'Average_municipality', pattern = 'Avg_municipality')
  
  irrig_req = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Irrigation', subfolder = 'Irrigation_requirements', subfolderX2 = sub_mod, subfolderX3 = 'Total', subfolderX4 = 'Total', pattern = 'Total')
  irrig_req_gw = irrig_req_sw = irrig_req
  irrig_req_sw[, yrs] = sapply(yrs, function(x) round(irrig_req[,x] * (100-gw_src[, 4])/100, 1)) # IWR from sw
  irrig_req_gw[, yrs] = sapply(yrs, function(x) round(irrig_req[,x]  * gw_src[, 4]/100, 1)) # IWR from gw
  
  # check if years are correct
  if (!length(yrs)==23) { stop('Wrong years! Irrigation is only 1995-2017.') }
  
  # set the number param modifier 
  modifier = 1 + param_modifier
  
  if (param == 'no3_gw') { 
    
    no3_gw[, yrs] = sapply(yrs, function(x) round(no3_gw[,x] * modifier, 1))  #  gw conc
  } 
  else if (param == 'no3_sw') {
    
    no3_sw[, yrs] = sapply(yrs, function(x) round(no3_sw[,x] * modifier, 1))  #  sw conc
  }
  else if (param == 'IWR_gw') { 
    
    irrig_req_gw[, yrs] = sapply(yrs, function(x) round(irrig_req_gw[,x] * modifier, 1)) # IWR
  }  
  else if (param == 'IWR_sw') {
    
    irrig_req_sw[, yrs] = sapply(yrs, function(x) round(irrig_req_sw[,x] * modifier, 1)) # IWR
  }
  else if (param == 'All') {
    # change no3_gw, no3_sw and IWR
    
    irrig_req_gw[, yrs] = sapply(yrs, function(x) round(irrig_req_gw[,x] * modifier, 1)) # IWR from gw
    irrig_req_sw[, yrs] = sapply(yrs, function(x) round(irrig_req_sw[,x] * modifier, 1)) # IWR from gw
    no3_gw[, yrs] = sapply(yrs, function(x) round(no3_gw[,x] * modifier, 1))  #  gw conc
    no3_sw[, yrs] = sapply(yrs, function(x) round(no3_sw[,x] * modifier, 1))  #  sw conc
  }
  else { 
    stop('Wrong params!')
  }
  
  return(list(gw_src = gw_src, no3_sw = no3_sw, no3_gw = no3_gw, IWR_sw = irrig_req_sw, IWR_gw = irrig_req_gw))
}


heuristicSA_total_irrigation_N = function(param, param_modifier = 0.25, write=F, IWW = 'dynamic') {
  #' @param param either gw_src (for nitrate concentration), no3_gw, no3_sw, IWR or All (no3_gw and sw and IWR)
  #' @note when changing gw_src, the complementary fraction for sw is the inverse
  #' @param param_modifier integer to modify the baseline vars
  #' @description calls 5_Nutrients_irrigation and applies a heuristic SA to all main params
  #' @return calculates the range (upper and lower) of total irrigation N from a given source (unit: kg N yr-1) according to the param specified and param_modifier
  
  
  # load all Nirrig vars
  Nirrig_vars = modify_irrigation_N_params(param, param_modifier, IWW)
#  gw_src = Nirrig_vars[[1]] not needed anymore
  no3_sw = Nirrig_vars[[2]]
  no3_gw = Nirrig_vars[[3]]
  irri_req_sw = Nirrig_vars[[4]]
  irri_req_gw = Nirrig_vars[[5]]
  conv_fct = 0.001 # conversion factor
  irrig_N = irri_req_gw
  
  irrig_N[, yrs] = sapply(yrs, function(x) round(
    #N_irrig_total = IWR * conv_fct * [ (gw_src * gw_conc * 11.3/50) + ( (1-gw_src) * sw_conc) ]
    (irri_req_gw[,x] * conv_fct  * no3_gw[,x] * 11.3/50) + (irri_req_sw[,x] * conv_fct  * no3_sw[,x])
  , 1)
  )
  
  irrig_N = irrig_N[, c('Muni_ID','ID','Muni', yrs)]
  name_modifier = paste0('Mod',as.character(1+param_modifier),'_',param,'_municipality')
  
  if (write==TRUE) {
    export_file(module = 'Nutrients', 
                folder = 'Irrigation', 
                subfolder = 'Irrigation_N', 
                subfolderX2 = 'Total', 
                subfolderX3 = 'Average_municipality', 
                subfolderX4 = 'Heuristic_SA',
                file = irrig_N, 
                filename = name_modifier)
  }
  
  return(irrig_N)
  rm(list=c('irrig_src','no3_src','irrig_req'))
}


loops_heuristicSA_irrigation_N = function(write=T, param_modifier = 0.25) {
  #' @description loops each param, for each source and only then for the total Nirrig, the range of Nirrig according to the param_modifier
  
  all_params = c('no3_gw','no3_sw','IWR_gw','IWR_sw','All')
  #  loop for total Nirrig
  sapply(all_params, function(x) heuristicSA_total_irrigation_N(param = x, param_modifier = param_modifier, write = write)) 
}
loops_heuristicSA_irrigation_N(param_modifier = 0.25)
loops_heuristicSA_irrigation_N(param_modifier = -0.25)
