source('./Water/Model/Water_balance/2_CropCalendars.R')
source('./Water/Model/Water_balance/2_GlobWat.R')

library(doParallel)
library(dplyr)

timeperiod = paste0('X', seq(1995,2017))

# CROP CALENDARS r -----------------------------------

dates = seq.Date(from = as.Date("1995/1/1"), to = as.Date("2017/12/1"), by = "month")
calendar_df = data.frame(date=dates,
                year = as.numeric(substr(dates, 1,4)),
                month  = as.numeric(substr(dates, 6,7)),
                i = 1:length(dates))


get_crop_monthCalendar = function(param) {
  #' @param crop
  #' @description finds the irrigation crop months
  
  calendar = get_activity_data(module = 'Water', folder = 'Other_params', subfolder = 'Tabled',pattern = 'calendar')
  calendar = calendar[which(calendar$Crop==param),]
  months =  which((calendar[,paste0('M',seq(1,12))] > 0)==T)
  
  return(months)  
}

find_seq_cropMonthCalendar = function(param, calendar_df) {
  #' @description find the ids i=1:nmonths where crops are actually irrigated
  #' @description this reduces loop time

  crop_months = get_crop_monthCalendar(param)
  crop_seq_i = calendar_df[which(calendar_df$month %in% crop_months ==T), ]
  
  return(crop_seq_i)  
}




# IRRIGATION DEMAND ------------------------------------------------------------

compute_monthlyCropIrrDemand = function(main_param, param, parallel=F) {
  #' @param main_param main crop
  #' @param param crop
  #' @param parallel user has an option to parallize (about 1/3 of the runtime)
  #' @description calculates monthly crop irrigation demand for 1995-2017
  #' @unit  mm month-1
  
  crop_irrig_timeperiod = find_seq_cropMonthCalendar(param, calendar_df) # df
  
  ETrain = get_activity_data(module = 'Water', mainfolder = 'Output', folder = 'GlobWat', subfolder = 'Monthly', pattern = 'ETrain') # monthly ETrain for 1995-2017
  store_crop_irr = list()

  if (parallel==F) {
    
    for (i in 1:nrow(crop_irrig_timeperiod)) {
      
      iter = crop_irrig_timeperiod[i, 'i']
      month_ctr = crop_irrig_timeperiod[i, 'month']
      yr_ctr = crop_irrig_timeperiod[i, 'year']
      
      print(paste0('Timestep ', i, ' .................................'))
      et0 = load_monthlyVars(var = 'ET0', month_ctr = iter)
      crop_kc = rasterize_monthly_cropKC(main_param, param, month_ctr, paste0('X',yr_ctr))
      crop_irr = (et0 * crop_kc) - ETrain[[iter]] # irrigatio requirements
      crop_irr[crop_irr<0] = 0 # where crop does not exist
      store_crop_irr[[iter]] = crop_irr
    }
  }
  else {

    cl = makePSOCKcluster(3)
    registerDoParallel(cl)
    store_crop_irr = foreach(i=1:nrow(crop_irrig_timeperiod),
                     .export = ls(globalenv()),
                    #.export=c('load_monthlyVars','rasterize_monthly_cropKC','get_activity_data',
                     #         'get_mainfolder_sub','identify_read_fileclass','fasterize_admin_statistical', 'r_tmp',
                      #        'calendar_df', 'main_param','param', 'ETrain','cropIrrigatedAreas_municipality','yrs', 'get_monthly_cropKc'),
                    .packages = c('raster')) %dopar% {
                      
                      iter = crop_irrig_timeperiod[i, 'i']
                      month_ctr = crop_irrig_timeperiod[i, 'month']
                      yr_ctr = crop_irrig_timeperiod[i, 'year']
                      
                      et0 = load_monthlyVars(var = 'ET0', month_ctr = iter)
                      crop_kc = rasterize_monthly_cropKC(main_param, param, month_ctr, paste0('X',yr_ctr))
                      crop_irr = (et0 * crop_kc) - ETrain[[iter]] # irrigatio requirements
                      crop_irr[crop_irr<0] = 0 # where crop does not exist
                      return(crop_irr)
                    }
    stopCluster(cl) 
  }
  
  names(store_crop_irr) = paste0(crop_irrig_timeperiod$year, '-', crop_irrig_timeperiod$month)
  
  return(store_crop_irr)
  rm(list=c('ETrain','et0','crop_kc','crop_irr'))
}


compute_annualCropIrrDemand = function(monthIrrDemand, param) {
  #' @param monthIrrDemand monthly stack of crop irr demand for 1995-2017
  #' @description computes annual Irr demand for 1995-2017
  #' @unit  mm yr-1
  
  print('Coomputing annual demand ....')
  annualIrr = lapply(as.character(seq(1995,2017)), function (x) { sum(monthIrrDemand[[ which(grepl(x, names(monthIrrDemand))==T)]]) }) # compute annual vals 
  annualIrr = stack(annualIrr)
  names(annualIrr) = as.character(seq(1995,2017))
  
  export_file(module = 'Water', file = annualIrr, filename = param, folder = 'Crop_irrigation_requirements', subfolder = 'Annual')
  rm(list='annualIrr')
}



loop_monthlyAnnualCropIrrDemand = function() {
  #' @description computes monthly and then total crop irrigation demand
  #' loops each crop
  #' @unit monthly data: mm month-1
  #' @unit annual data: mm yr-1
  
  irrig_id = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Irrigation', subfolderX2 = 'Portugal_statistics', pattern = 'Crop_ids')
 # irrig_id = irrig_id[seq(9, 10), ]
  
  for (i in 1:nrow(irrig_id)) {
    
    main_param = irrig_id[i, 'Main_crop']
    param = irrig_id[i, 'Crop']
    param = ifelse(param == 'Other_industry','Tomato',param)
    crop_IWD = compute_monthlyCropIrrDemand(main_param, param, parallel = T)
    crop_IWD = stack(crop_IWD)
    export_file(module = 'Water', file = crop_IWD, filename = param, folder = 'Crop_irrigation_requirements', subfolder = 'Monthly')
    rm(list='crop_IWD')
    gc()
    # export annual var
    monthly_crop_irr = get_activity_data(module = 'Water', mainfolder = 'Output', folder = 'Crop_irrigation_requirements', subfolder = 'Monthly', pattern = paste0(param,'.grd'))
    compute_annualCropIrrDemand(monthIrrDemand = monthly_crop_irr, param = param)
  }
  rm(list=c('crop_IWD','monthly_crop_irr'))
}




# IRRIGATION WITHDRAWALS -------------------------------


compute_cropIrrigWithdrawalsMethods_annual = function(main_param, param, yr, method) {
  #' @param main_param main crop
  #' @param param crop
  #' @param yr year as 'X1999'
  #' @param method by default NULL, related to irrigation methods (drip, furrows,gun, microsprink, other_gravity, pivot, sprinkler)
  #' @description calculates crop irr withdrawal for a given method = irrig demand /I_eff ;; if method does not exist, returns NULL
  #' @unit mm yr-1
  #' @usage rasterize_cropIrrigAreasMethods('Cereals','Maize','X1999','drip')
  
  I_eff = rasterize_cropIrrigAreasMethods(main_param, param, yr, method) # I_eff
  
  # check if method exists for crop
  if (is.null(I_eff)==T) { # or -999
    return(NULL)
  }
  else {
    cropIrrDem = get_activity_data(module = 'Water', mainfolder = 'Output', folder = 'Crop_irrigation_requirements', subfolder = 'Annual', pattern = paste0(param,'.grd')) # annual stack
    cropIrrDem = cropIrrDem[[which(names(cropIrrDem)==yr)]] # get data for "yr" year
    cropIrrWith = cropIrrDem / I_eff #withdrawal = Demand / I_eff
    
    if (param == 'Rice') { cropIrrWith = cropIrrWith + 200  } # add 20 cm water layer to rice 
    
    return(cropIrrWith)
    rm(list=c('I_eff','cropIrrDem'))
  }
}



compute_cropIrrigWithdrawalsMethods_timeperiod = function(main_param, param, method) {
  #' @param main_param main crop
  #' @param param crop
  #' @param method by default NULL, related to irrigation methods (drip, furrows,gun, microsprink, other_gravity, pivot, sprinkler)
  #' @description calculates crop irr withdrawal for a given method = irrig demand /I_eff for all PERIOD;; if method does not exist, returns NULL
  #' @unit mm yr-1
  #' @usage rasterize_cropIrrigAreasMethods('Cereals','Maize','drip')
  
  cropIrrigWith = lapply(timeperiod, function(x) {
    print(paste0('Computing irrigation withdrawals for ', param, ' for ', x))
    compute_cropIrrigWithdrawalsMethods_annual(main_param, param, x, method)
  }) 
  names(cropIrrigWith) = timeperiod
  if (is.null(unlist(cropIrrigWith))==F) { return( cropIrrigWith )} # if not null, return cropIrrigWith
}

get_crop_irrigMethods = function(main_param, param) {
  #' @description find the appropriate irrigation methods for each crop
  
  filepath = paste0('./Water/Output/Crop_irrigation_withdrawals/Annual/',main_param,'/',param,'/')
  crop_methods = list.files(path = filepath, pattern = 'gri')
  crop_methods = gsub(pattern = paste0(param,'_'), replacement = '' ,x = crop_methods)
  crop_methods = gsub(pattern = '.gri', replacement = '' ,x = crop_methods)
  
  return(crop_methods)     
}

loop_cropIrrigWithdrawals_methods = function() {
  #' @description loops for each crop and year, the irrigation water withdrawals
  
  irrig_id = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Irrigation', subfolderX2 = 'Portugal_statistics', pattern = 'Crop_ids')
  irrig_method = c('drip','furrows','gun','microsprink','other_gravity','pivot','sprinkler')
#  irrig_id = irrig_id[seq(9, 10), ]
  
  for (i in 1:nrow(irrig_id)) {
    
    main_param = irrig_id[i, 'Main_crop']
    param = irrig_id[i, 'Crop']
    param = ifelse(param == 'Other_industry','Tomato',param)
    irrig_method = get_crop_irrigMethods(main_param, param)
    
    for (method in irrig_method) {
      
      irrWith = compute_cropIrrigWithdrawalsMethods_timeperiod(main_param, param, method) # compute crop irrig withdrawals for the timeperiod
      
      irrWith_ids= lapply(1:length(irrWith), function(x) is.null(irrWith[[x]])==F) # remove null yrs
      irrWith = irrWith[which(irrWith_ids==T)]
     
      if (is.null(irrWith)==F) { 
        filename_mod = paste0(param,'_',method)
        export_file(module = 'Water', file = stack(irrWith), filename = filename_mod, folder = 'Crop_irrigation_withdrawals', subfolder = 'Annual', subfolderX2 = main_param, subfolderX3 = param)
      }
    }
  }
  rm(list=c('irrig_id','irrig_methods','main_param','param','irrWith'))
}




# average crop IWW at the municipality scale


check_outputValidity = function(main_param, param, method, avg_cropIWW_df, yrs_ctr=NULL) {
  #' @param avg_cropIWW_df the df avg_cropIWW from avgCropIrrgWithdrawls_methods_municipality()
  #' @description checks the validity of the results; if crop IA > 0, then crop IWW > 0 always
  #' if this condition is not met (ie, length check condition > 0), stop
  if (missing(yrs_ctr)==T) { yrs_ctr = timeperiod} 
  
  for (yr in yrs_ctr) {
    
    check_condition = get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
    crop_ia = cropIrrigatedAreas_municipality(main_param, param, yr, method)

    check_condition[, yr]= sapply(yr, function(x) {
      if (crop_ia[,x]==1 & avg_cropIWW_df[,x]==0) { return(1) } else { return(0) }  # if crop IA exists but no IWR, add 1, else 0 
    } )
    
    # check condition; sum of check_condition[,yr] must always be 0
    if (sum(check_condition[,yr])!=0) { 
      stop('An error was found')
    } 
    else {
      print('No error was found.')
      avg_cropIWW_df[,yr] = avg_cropIWW_df[,yr] * crop_ia[,yr] # clean munis with IWW but without IA
    }
  }
  
  return(avg_cropIWW_df)
}




avgCropIrrgWithdrawls_methods_municipality = function() {
  #' @description computes the crop irrigation withdrawals for 1995-2017 at the municipality level
  #' also checks the validity of these estimates after extracting the raster data
  #' @unit m3 ha-1 yr-1
  
  irrig_id = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Irrigation', subfolderX2 = 'Portugal_statistics', pattern = 'Crop_ids')
  irrig_id = irrig_id[seq(9, 10), ]
  for (i in 1:nrow(irrig_id)) {
    
    main_param = irrig_id[i, 'Main_crop']
    param = irrig_id[i, 'Crop']
    param = ifelse(param == 'Other_industry','Tomato',param)
    
    irrig_method = get_crop_irrigMethods(main_param, param)
    
    for (method in irrig_method) {
      
      avg_cropIWW = get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 

      irrWith = get_activity_data(module = 'Water', mainfolder = 'Output', folder = 'Crop_irrigation_withdrawals', subfolder = 'Annual', subfolderX2 = main_param, subfolderX3 = param, pattern = method)
      muni =  get_activity_data('LULCC', folder = 'Admin', pattern = 'Municipality') # load muni shp 
      
      store_yrs = c()
      for (i in 1:nlayers(irrWith)){
        yr = names(irrWith)[i]
        store_yrs = c(store_yrs,yr)
        muni[, yr] = round(exactextractr::exact_extract(irrWith[[i]], muni, 'mean') * 10, 0)
      }
      
      # pre process to Muni_IDE
      names(avg_cropIWW)[1] = 'Admin_id'
      avg_cropIWW = plyr::join(x = avg_cropIWW, y = as.data.frame(muni[, c('Admin_id',store_yrs)]), by='Admin_id')
      avg_cropIWW = as.data.frame(avg_cropIWW)
      avg_cropIWW$geometry= NULL
      avg_cropIWW[is.na(avg_cropIWW)] = 0 
      
      avg_cropIWW = check_outputValidity(main_param, param, method,avg_cropIWW, store_yrs) # check validity!
      gc()
      # export
      filename_mod = paste0(param,'_',method)
      export_file(module = 'Water', file = avg_cropIWW, filename = filename_mod, folder = 'Crop_irrigation_withdrawals', subfolder = 'Avg_municipality', subfolderX2 = main_param, subfolderX3 = param)
    }
  }
 rm(list=c('avg_cropIWW','irrWith')) 
}


