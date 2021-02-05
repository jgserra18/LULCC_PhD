source('./Water/Model/Water_balance/2_GlobWat.R')


compute_sumPrec = function(year, var_name) {
  
  if (var_name  == 'Rain_JFM') {
    j =raster(paste0('./Water/Activity_data/Climatic_params/Monthly/Prec/', year,'-01-01', '.tif'))
    f =raster(paste0('./Water/Activity_data/Climatic_params/Monthly/Prec/', year,'-02-01', '.tif'))
    m =raster(paste0('./Water/Activity_data/Climatic_params/Monthly/Prec/', year,'-03-01', '.tif'))
    p = sum(j, f, m)
    rm(list=c('j','f','m'))
  }
  else if (var_name =='Rain_AMJ') {
    j =raster(paste0('./Water/Activity_data/Climatic_params/Monthly/Prec/', year,'-04-01', '.tif'))
    f =raster(paste0('./Water/Activity_data/Climatic_params/Monthly/Prec/', year,'-05-01', '.tif'))
    m =raster(paste0('./Water/Activity_data/Climatic_params/Monthly/Prec/', year,'-06-01', '.tif'))
    p = sum(j, f, m)
    rm(list=c('j','f','m'))
  }
  else if (var_name=='Rain_JA') {
    j =raster(paste0('./Water/Activity_data/Climatic_params/Monthly/Prec/', year,'-07-01', '.tif'))
    f =raster(paste0('./Water/Activity_data/Climatic_params/Monthly/Prec/', year,'-08-01', '.tif'))
    p = sum(j, f)
    rm(list=c('j','f'))
  }
  return(p)
}


compute_tavg = function(year, var_name) {
  
  if (var_name  == 'TempJF') {
    f1 =raster(paste0('./Water/Activity_data/Climatic_params/Monthly/tmin//', year,'-01-01', '.tif'))
    f2 =raster(paste0('./Water/Activity_data/Climatic_params/Monthly/tmax//', year,'-01-01', '.tif'))
    e1 =raster(paste0('./Water/Activity_data/Climatic_params/Monthly/tmin//', year,'-02-01', '.tif'))
    e2 =raster(paste0('./Water/Activity_data/Climatic_params/Monthly/tmax//', year,'-02-01', '.tif'))
    
    p = mean(f1,f2,e1,e2)
    rm(list=c('f1','f2','e1','e2'))
  }
  else if (var_name  == 'TempMA') {
    f1 =raster(paste0('./Water/Activity_data/Climatic_params/Monthly/tmin//', year,'-03-01', '.tif'))
    f2 =raster(paste0('./Water/Activity_data/Climatic_params/Monthly/tmax//', year,'-03-01', '.tif'))
    e1 =raster(paste0('./Water/Activity_data/Climatic_params/Monthly/tmin//', year,'-04-01', '.tif'))
    e2 =raster(paste0('./Water/Activity_data/Climatic_params/Monthly/tmax//', year,'-04-01', '.tif'))
    
    p = mean(f1,f2,e1,e2)
    rm(list=c('f1','f2','e1','e2'))
  }
  else if (var_name  == 'TempMJ') {
    f1 =raster(paste0('./Water/Activity_data/Climatic_params/Monthly/tmin//', year,'-05-01', '.tif'))
    f2 =raster(paste0('./Water/Activity_data/Climatic_params/Monthly/tmax//', year,'-05-01', '.tif'))
    e1 =raster(paste0('./Water/Activity_data/Climatic_params/Monthly/tmin//', year,'-06-01', '.tif'))
    e2 =raster(paste0('./Water/Activity_data/Climatic_params/Monthly/tmax//', year,'-06-01', '.tif'))
    
    p = mean(f1,f2,e1,e2)
    rm(list=c('f1','f2','e1','e2'))
  }
  else if (var_name  == 'TempJA') {
    f1 =raster(paste0('./Water/Activity_data/Climatic_params/Monthly/tmin//', year,'-07-01', '.tif'))
    f2 =raster(paste0('./Water/Activity_data/Climatic_params/Monthly/tmax//', year,'-07-01', '.tif'))
    e1 =raster(paste0('./Water/Activity_data/Climatic_params/Monthly/tmin//', year,'-08-01', '.tif'))
    e2 =raster(paste0('./Water/Activity_data/Climatic_params/Monthly/tmax//', year,'-08-01', '.tif'))
    
    p = mean(f1,f2,e1,e2)
    rm(list=c('f1','f2','e1','e2'))
  }
  return(p)
}


load_climVars = function(year) {
  #' @source Dellar et al 2019 https://doi.org/10.1016/j.envsoft.2019.104562 
  #' @description loads the required monthly vars according to src
  
  
  Rain_JFM = compute_sumPrec(year, 'Rain_JFM')
  Rain_AMJ = compute_sumPrec(year, 'Rain_AMJ')
  
  TempJF =compute_tavg(year, 'TempJF')
  TempMA = compute_tavg(year, 'TempMA')
  TempMJ = compute_tavg(year, 'TempMJ')
  TempJA = compute_tavg(year, 'TempJA')
  Rain_JA = compute_sumPrec(year, 'Rain_JA')

  all = stack(Rain_JFM, Rain_AMJ, TempJF, TempMA, TempMJ, TempJA, Rain_JA)
  names(all) = c('Rain_JFM','Rain_AMJ','TempJF','TempMA','TempMJ','TempJA','Rain_JA')
  return(all)
}

# load static vars

tmp =  load_TerraClim_monthlyVars(var = 'prcp', start = paste0('2000','-01-01'), end = paste0('2000', '-01-01'))
r_template = raster(ext=extent(tmp), crs=proj4string(tmp), res=res(tmp))

reg = raster('./Nutrients/Activity_data/General_params/Crops/Grass_yield/ENV_yields.tif')
reg[reg<5000] = 1.2554504 # south
reg[reg>=5000] = 0 # atlantic
reg = resample(reg, r_template, 'ngb')
reg = mask(crop(reg, extent(tmp)), tmp)

altitude = raster('./LULCC/Activity_data/Environmental_params/500m/Elevation.tif')
altitude = resample(altitude, r_template, 'ngb')

p = read.csv('./Yield_Intensive_pasture.csv')

cuts = (reg>=0) * 2 # cut 3 times
NF = (reg>=0) * 135 # kg N ha-1 yr-1
f_legume = (reg>=0) * 5 # %; baddeley et al 2014





loop_intensivePasture_dmYield = function() {
  #' @description calculates intensive pasture DM yield according to 
  #' @source Dellar et al 2019 https://doi.org/10.1016/j.envsoft.2019.104562 
  #' @unit kg DM ha-1 yr-1
  
  yrs = as.character(seq(2005,2017))
  sapply(yrs, function(x) {
    climvars = load_climVars(as.character(x))
    prodDM = p[1,2] + 
      reg + p[4,2] * climvars[['Rain_JFM']] + 
      p[5,2]*climvars[['Rain_AMJ']] + p[6,2] * climvars[['Rain_JA']] + 
      p[7,2]*climvars[['TempJF']] + p[8,2]*climvars[['TempMA']] + 
      p[9,2] * climvars[['TempMJ']] +  p[10,2]*climvars[['TempJA']] + 
      p[11,2]*climvars[['Rain_JFM']]^2 + p[12,2] * climvars[['Rain_AMJ']]^2 +
      p[13,2] * climvars[['Rain_JA']]^2 + p[14,2] * climvars[['TempMJ']]^2 + 
      p[15,2]*climvars[['TempJA']]^2 + p[16,2]*altitude +
      p[17,2] * cuts + p[18,2] * f_legume + 
      p[19,2] * NF + p[20,2]*altitude^2 + 
      p[21,2] * cuts^2 + p[22,2]*f_legume^2 + 
      p[23,2]*NF^2 + p[24,2]*NF*climvars[['Rain_JA']] + 
      p[25,2] * NF * cuts
    prodDM = prodDM * 1000 # kg DM ha-1 yr-1
    prodDM[prodDM<0] = 0
    
    export_file(module = 'Nutrients', file = prodDM, 
                folder = 'Activity_data', subfolder = 'General_params', 
                subfolderX2 = 'Crops', subfolderX3 = 'Grass_yield', 
                subfolderX4 = 'Intensive_pasture', subfolderX5 = 'Spatially_explicit', filename = as.character(x))
    
  })
}


compute_agrarianRegion_yields = function() {
  #' @unit kg DM ha-1 yr-1
  
  yrs = as.character(seq(1987,2017))
  ag = get_activity_data(module = 'LULCC', folder = 'Admin', pattern = 'Agrarian')
 
  ag[, paste0('X',yrs)] = sapply(yrs, function(x) {
    climvars = load_climVars(x)
    ag[, names(climvars)] = sapply(names(climvars), function(x) exactextractr::exact_extract(climvars[[x]], ag, 'mean'))
    ag$Altitude = exactextractr::exact_extract(altitude, ag, 'mean')
    ag$Cuts = exactextractr::exact_extract(cuts, ag, 'mean')
    ag$NF = exactextractr::exact_extract(NF, ag, 'mean')
    ag$f_legume = exactextractr::exact_extract(f_legume, ag, 'mean')
    ag$reg = exactextractr::exact_extract(reg, ag, 'mean')
    ag$reg = ifelse(ag$reg>0.62, 1.25545, 0)
    ag[, paste0('X',x)] = p[1,2] + 
      ag$reg + p[4,2] * ag$Rain_JFM + 
      p[5,2]*ag$Rain_AMJ + p[6,2] * ag$Rain_JA + 
      p[7,2]*ag$TempJF + p[8,2]*ag$TempMA + 
      p[9,2] * ag$TempMJ +  p[10,2] * ag$TempJA + 
      p[11,2]*ag$Rain_JFM^2 + p[12,2] * ag$Rain_AMJ^2 +
      p[13,2] *ag$Rain_JA^2 + p[14,2] * ag$TempMJ^2 + 
      p[15,2]*ag$TempJA^2 + p[16,2]*ag$Altitude +
      p[17,2] * ag$Cuts + p[18,2] * ag$f_legume + 
      p[19,2] * ag$NF + p[20,2]*ag$Altitude^2 + 
      p[21,2] * ag$Cuts^2 + p[22,2]*ag$f_legume^2 + 
      p[23,2]*ag$NF^2 + p[24,2]*ag$NF*ag$Rain_JA + 
      p[25,2] * ag$NF * ag$Cuts
  })
  df = as.data.frame(ag)
  df$geometry = NULL
  df = df[, c('Admin_name','Admin_id','Muni',paste0('X', yrs))]
  export_file(module = 'Nutrients', file = df, 
              folder = 'Activity_data', subfolder = 'General_params', 
              subfolderX2 = 'Crops', subfolderX3 = 'Grass_yield', 
              subfolderX4 = 'Intensive_pasture', subfolderX5 = 'Tabled', filename = 'Intensive_pasture_Ag')
  
}



            