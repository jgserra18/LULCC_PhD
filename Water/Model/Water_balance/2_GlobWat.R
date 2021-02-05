source('./Main/Global_functions.R')



export_monthlyParams = function(file, var) {
  #' @param file must be a list with the monthly files for the ismulation period
  #' @param var varname to rename the file
  #' @importFrom export_file()
  
  if (!class(file)=='list') { stop('Must be a list.')}
  file = stack(file)
  names(file) =  seq.Date(from = as.Date("1995/1/1"), to = as.Date("2017/12/1"), by = "month")
  export_file(module = 'Water', file = file, filename = var, folder = 'GlobWat', subfolder = 'Monthly')
}


numberMonths = function() {
  
  files = list.files(path = './Water/Activity_data/Climatic_params/Monthly/Prec/', full.names = T)
  return(length(files))
}


load_monthlyVars = function(var, month_ctr) {
  #' @param var either ET0 or Prec
  #' @param month_ctr number that counts them monthly analysis
  #' @description returns the speceified param for a given year 

  if (!(var == 'ET0'| var == 'Prec')) { stop('var is either ET0 or Prec') }
  files = list.files(path = paste0('./Water/Activity_data/Climatic_params/Monthly/', var), full.names = T)
  files = files[month_ctr]
  files = raster(files)
  
  return(files)
}

update_ET_rain  = function(SM_t_1, ET_rain, Smax, Seav) {
  # ET_rain is the rainfall-depend evapotranspiration
  # SM_max is the maximum soil moisture
  # SM_0 is the initial SM_0 or S_eanv
  
  # function updates daily ET0 based on soil moisture
  # unit: mm/day
  
  # if ((SM_t_1>=SM_0) & (SM_max >= SM_t_1)) { --> conditionA and conditionB
  # ET0  = ET0  -- < cond1
  # else if (SM_t_1 < SM_0) 
  # ET0  = ET0  = SM_t_1 / SM_0 --> cond2
  
  print('Updating ET0')

  ET_rain = overlay(SM_t_1, Seav, Smax, ET_rain,
                    fun = function(a,b,c,d) ifelse(a < b, d * a / b, d))
  
  return(ET_rain)
}

update_DeepPerc  = function(SM_t_1, Smax, Seav, Rmax) {
  #' @param SM_t_1 soil moisture at t=0
  #' @param Smax maximum Soil moisture
  #' @param Seav easily available soil moisture (50% of Smax)
  #' @param Rmax maximum perc rate (mm yr-1)
  
  #if (SM_t_1 >= SM_0 & SM_max >= SM_t_1) {
  # R  = R_max * (SM_t_1 - SM_0) / (SM_max - SM_0)
  #} else if (SM_t_1<SM_0) {
  # R  = 0
  #}
  
  print('Updating deep percolation')
  R = overlay(SM_t_1, Seav, Smax, Rmax, 
              fun=function(a,b,c,d) ifelse(a<b, 0, d * (a-b)/(c-b)))
  return(R)
}

update_runoff  = function(SM_t_1, Prec, ET_rain, Smax) {
  #' @param SM_t_1 soil moisture at t=0
  #' @param prec precipitation
  #' @param ET_rain rainfed ET0
  #if (WB < SM_max) {
  #   R0  = 0
  #} else if (WB >= SM_max) {
  #  R0  = WB - SM_max
  # }
  
  print('Updating runoff')
  WB  = SM_t_1 + Prec - ET_rain 
  R0 = overlay(WB, Smax, fun=function(a,b) ifelse(a<b, 0, a-b))
  
  return(R0)
}

update_WB  = function(SM_t_1, Prec, ET_rain, R0, R) {
  #' @param SM_t_1 soil moisture at t=0
  #' @param prec precipitation
  #' @param ET_rain rainfed ET0
  #' @param R0 runoff
  #' @param R deep percolation
  
  # WB = SM_t
  WB  = SM_t_1 + Prec - ET_rain - R0 - R
  return(WB)
}

update_SoilMoisture  = function(WB_T, Smax) {
  # SM_t_1 - soil moisture at t - 1
  # WB_T - Water Balance (EQ 14) in time = t
  
  # if (WB_t < SM_max)  --> condition
  # SM  = WB_T --< cond1
  # else
  # SM  = SM_max --> cond2
  
  print('Updating soil moisture')
  
  SM = overlay(WB_T, Smax, 
               fun  = function(a,b) ifelse(a < b, a, b))
  SM[SM<0] = 0 #trying this
  return(SM)
}


compute_monthlyGlobWat = function(apply_SMmax_corr=F) {
  #' @param apply_SMmax_corr apply GlobWat soil correction factor of 0.7
  #' @unit  mm month-1
  
  # load conditional params 
  #SM = get_activity_data(module = 'Water', folder = 'Other_params', subfolder = 'Spatial', subfolderX2 = 'Conditions', pattern = 'S_initial')
  Rmax = get_activity_data(module = 'Water', folder = 'Other_params', subfolder = 'Spatial', subfolderX2 = 'Conditions', pattern = 'MaxPerc') # mm month-1
  Smax = get_activity_data(module = 'Water', folder = 'Other_params', subfolder = 'Spatial', subfolderX2 = 'Conditions', pattern = 'Smax') # mm 
  if (apply_SMmax_corr==T) { Smax = Smax * 0.7 }
  Seav = Smax * 0.5
  SM = Seav
  
  # prepare param storers
  new_et_rain <- list()
  new_R <- list()
  new_R0 <- list()
  new_SM <- list()
  new_WB <- list()
  
  nmonths = numberMonths()
  month_ctr = 0
  yr_ctr = 1995 # initial year
 
  for (i in 1:nmonths) {
 # for (i in 1:5) {
      
    yr_ctr = ifelse(month_ctr>12, yr_ctr + 1, yr_ctr) # update year
    month_ctr = ifelse(month_ctr>12, 1, month_ctr + 1) # update month 
    
    print(paste0('Timestep ', i, ' .................................'))
    # load prec and et0
    prec = load_monthlyVars(var = 'Prec', month_ctr = i)
    et0 = load_monthlyVars(var = 'ET0', month_ctr = i)
    
    # load annual kc map to calculate rainfed ET0
    lu_kc = get_activity_data(module = 'Water', folder = 'Other_params', subfolder = 'Spatial', subfolderX2 = 'LU_kc', pattern = as.character(yr_ctr))
    # update ETrain (rainfed ET0, ie, maximum ET0)
    ETrain = et0 * lu_kc
    ETrain = update_ET_rain(SM_t_1 = SM, ET_rain = ETrain, Smax = Smax, Seav = Seav)
    # calculate deep perc (R), runoff (R0) and WB
    R = update_DeepPerc(SM_t_1 = SM, Smax = Smax, Seav = Seav, Rmax = Rmax)
    R0 = update_runoff(SM_t_1 = SM, Prec = prec, ET_rain = ETrain, Smax = Smax)
    WB = update_WB(SM_t_1 = SM, Prec = prec, ET_rain = ETrain, R0 = R0, R = R)
    #  update soil moisture for timestep = t
    SM = update_SoilMoisture(WB_T = WB, Smax = Smax)
    
    # append updated data and convert to raster brick
    new_et_rain <- append(new_et_rain, ETrain)
    new_R <- append(new_R, R)
    new_R0 <- append(new_R0, R0)
    new_SM <- append(new_SM, SM)
    new_WB <- append(new_WB, WB)

  }
  
  print('Writing ...........................')
  export_monthlyParams(file = new_et_rain, var = 'ETrain')
  export_monthlyParams(file = new_R, var = 'DeepPerc')
  export_monthlyParams(file = new_R0, var = 'Runoff')
  export_monthlyParams(file = new_SM, var = 'SoilMoisture')
  export_monthlyParams(file = new_WB, var = 'WaterBalance')
  
  # clear memory
  rm(list=c('lu_kc','ETrain','R','R0','WB','SM','new_et_rain','new_R','new_R0','new_SM','new_WB','prec','et0'))
}



compute_annualGlobWatVars = function(stack_var, varname, foldername) {
  #' @param stack_var stack raster with monthly data for 1995-2017
  #' @param varname varname; exported file name
  #' @param foldername either GlobWat or Crop_irrirgation_requirements
  #' @unit  mm yr-1
  
  
  if (!nlayers(stack_var)==276) { stop('stack var must be for monthly data, 1995-2017.')}
  nmonths = numberMonths()
  month_ctr = 0
  yr_ctr = 1995 # initial year
  
  store_all = list()
  store_annual = list()

  for (i in 1:nmonths) {

    month_ctr = month_ctr + 1 # update month 
  
    store_annual[[month_ctr]] = stack_var[[i]]
    
    if (month_ctr==12) {
      
      store_annual = stack(store_annual)
      store_annual = stackApply(store_annual, indices = 1, fun = sum) # compute annual var
      
      store_all = append(store_all, store_annual ) # append 
      month_ctr = 0 # reset month
      yr_ctr = yr_ctr + 1 #update year
      store_annual = list() # reset list
    }
  }
  store_all = stack(store_all) # stack annual params for 1995-2017
  names(store_all) = as.character(seq(1995,2017))
  
  if (varname == 'Prec' | varname == 'ET0') {
    export_file(module = 'Water', file = store_all, filename = varname, folder = 'Activity_data', subfolder = foldername, subfolderX2 = 'Annual')
  }
  else {
    export_file(module = 'Water', file = store_all, filename = varname, folder = foldername, subfolder = 'Annual')
  }

  rm(list=c('monthly_var','store_annual', 'store_all'))
}

loop_annualGlobWatVars = function() {
  
  #vars = c('Prec','ET0', 'Runoff','DeepPerc','ETrain')
  vars = c('Runoff','DeepPerc','ETrain')
  
  for (var in vars) {
    
    if (var == 'Prec'| var=='ET0') {
      monthly_var = list.files(path = paste0('./Water/Activity_data/Climatic_params/Monthly/',var,'/'), full.names=T)
      monthly_var = stack(lapply(monthly_var, raster))
      names(monthly_var) = seq.Date(from = as.Date("1995/1/1"), to = as.Date("2017/12/1"), by = "month")
      compute_annualGlobWatVars(stack_var = monthly_var, varname = var, foldername = 'Climatic_params')
    }
    else {
      monthly_var = get_activity_data(module = 'Water', mainfolder = 'Output', folder = 'GlobWat', subfolder = 'Monthly', pattern = var)
      compute_annualGlobWatVars(stack_var = monthly_var, varname = var, foldername = 'GlobWat')
    }
  }
}


