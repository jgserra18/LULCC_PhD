source('./Main/Global_functions.R')
source('./Main/General_GIS.R')

## ----------------------- LIBRARIES --------------------- 

library(lulcc)
library(dplyr)





## ----------------------- Exploratory Variables Organization --------------------- 


general_rstack_func = function(folder_path) {
  # general function to stack rasters from a given folder path
  
  l_files = list.files(folder_path, full.names = TRUE)
  
  require(doParallel)
  require(doRNG)
  cl = registerDoParallel(cores=2)
  getDoParWorkers()
  
  
  rs_func = function(x) {
    r_file = raster(x)
    if (res(r_file)[1] != 100) {
      r_file = resample_to_CLC(module = 'LULCC',r_file, TRUE, spatial_res)
    }
    return(r_file)
  }
  
  store = mclapply(l_files, FUN = rs_func)
  store = stack(store)
  return(store)
  doParallel::stopImplicitCluster()
  rm(list=c('l_files', 'r_file'))
}



## ----------------------- Exploratory Variables Organization

# PREPARE STATIC DATA -----------------------------------------------------------



get_output_statistical_data = function(spatial_res) {
  # creates a RasterStack from all statistical data from Statistics Portugal at subregional level
  
  out_stat = get_folderpath(module = 'LULCC', 'Output', 'Exploratory_variables', 'Statistical_data')
  stat_files = list.files(path = out_stat, pattern = as.character(spatial_res), full.names = TRUE)
  stat_files = list.files(path = stat_files, full.names = TRUE)
  store_list = vector('list', length = length(stat_files))
  
  ctr = 0
  for (i in stat_files) {
    
    ctr = ctr + 1
    r_file = raster(i)
    
    store_list[[ctr]] = r_file
  }
  
  store_list = stack(store_list)
  
  return(store_list)
  rm(list=c('out_stat', 'stat_files', 'r_file'))
}



correct_Params_Resolution = function(param, spatial_res) {
  
  spatial_res =  ifelse(spatial_res=='Native', 'Native', paste0(spatial_res, 'm'))
  
  climatic_native = get_dir_files(module = 'LULCC',folder = 'Activity_data', param_pattern = param, subfolder = spatial_res)
  
  for (i in climatic_native) {
    
    r_file = raster(i)
    clc = get_activity_data(module = 'LULCC',folder = 'CLC', pattern = '1990', subfolder =spatial_res)
    r_file = resample(r_file, clc)
  # r_file = resample_to_CLC(raster_file = r_file, spatial_res ='1000m')
    tifoptions=c('COMPRESS=DEFLATE', 'PREDICTOR=2', 'ZLEVEL=6')
    
    if (compareRaster(r_file, clc)==TRUE) {
      writeRaster(r_file, i, options=tifoptions, overwrite = TRUE)
    } else {
      print('Wrong spatial resolution.')
    }
  }
}


set_waterbodies_mask = function(spatial_res='500') {
  
  clc = stack(lapply(list.files(path = './LULCC/Activity_data/CLC/500m/', full.names = T), raster))
  clc = clc==512
  wbodies = calc(clc, fun=sum)
  wbodies = wbodies>0
  wbodies[wbodies==0] = NA
  
  return(wbodies)
  rm(list='clc')
}

stack_environmental_data = function(spatial_res) {
  # creates a rasterstack with climatic and environmental parameters
  # resamples those data to a 100x100m grid using the ngb algorithm
  
  store = list()
  folders = c('Environmental_params', 'Climatic_params')
  ctr = 0
  
  res = ifelse(spatial_res=='Native','Native', paste0(spatial_res,'m'))
  
  for (i in folders) {
    
    l_files = get_dir_files(module = 'LULCC', folder = 'Activity_data', param_pattern =  i, subfolder = res)
    folder_path = get_folderpath(module = 'LULCC', 'Activity_data', i)
    
    for (j in l_files) {
      
      print(paste0('Working with ', j))

      r_file = raster(j)
      # check spatial resolution
    #  if (res(r_file)[1] != 100) {
    #    r_file = resample_to_CLC(r_file, TRUE)
    #  }
      
      ctr = ctr + 1
      store[[ctr]] = r_file
      
      filename = gsub(paste0(folder_path, '/'), '', j)
      names(store)[[ctr]] = filename
    }
  }
  
  store = stack(store)
  return(store)
  rm(list=c('store', 'folders', 'folder_path', 'l_files', 'r_file'))
}




stack_MRB_data = function(spatial_res) {
  # stacks MRB data (e.g., distance to roads, distance to rivers)
  # resamples to CLC spatial res and extent
  # more importantly, it reclassifies and masks the MRB raster to the CLC data
  
  store = list()
  ctr_store = 0
  
  folder_path = get_folderpath(module = 'LULCC', main_folder = 'Output', folder = 'Exploratory_variables', subfolder = 'MRB')
  folder_path = list.files(path = folder_path, pattern = spatial_res, full.names = TRUE)
  l_files = list.files(folder_path,  full.names=TRUE)
  filename = gsub('.tif','',list.files(folder_path))
    
  ctr = 0
    
    for (j in l_files) {
      
      print(j)
      ctr = ctr + 1
      r_file = raster(l_files[[ctr]])
      store[[ctr]] = r_file
      names(store)[[ctr]] = filename[[ctr]]
    }
  
  store = stack(store)
  return(store)
  rm(list=c('folders', 'folder_path', 'l_files', 'shp_file', 'r_file'))
}


export_MRB_data = function(spatial_res) {
  
  r_stack = stack_MRB_data(spatial_res)
  names(r_stack) = gsub('.tif', '', names(r_stack))
  
  for (i in 1:nlayers(r_stack)) {
    r_file = r_stack[[i]]
    export_file(module = 'LULCC',file = r_file, folder = 'Exploratory_variables', filename = names(r_stack)[[i]], 
                subfolder = 'MRB', subfolderX2 = spatial_res)
    
  }
}


aggregate_stack_ExpVarRaster = function(spatial_res, write=F) {
  # static params
  # stack file names start by Static_
  
  mrb = stack_MRB_data(spatial_res)
  environ = stack_environmental_data(spatial_res)
  wbody = set_waterbodies_mask(spatial_res='500')
 
  # stat = get_output_statistical_data(spatial_res)
  #st = stack(mrb, environ)
  st = stack(stack(mrb, environ), wbody)
  #st = (st - cellStats(st, 'min')) / (cellStats(st, 'max')-cellStats(st, 'min'))
  
  if (write==TRUE) {
    # export vars names
    df = data.frame(ID = paste0('EF_', seq(12, nlayers(st)+11)),
                    ID_name = names(st))
    export_file(module = 'LULCC', folder = 'Activity_data', subfolder = 'Exp_vars', subfolderX2 = 'Vars_list', file = df, filename = 'Static_vars')
  }
 # names(st) = paste0('EF_', seq(12, nlayers(st)+11)) # start at 12 ebcause dynamic exp vars (7 LU_crop classes + 4 climatic params)
 names(st) = paste0('ef', seq(1, nlayers(st))) # start at 12 ebcause dynamic exp vars (7 LU_crop classes + 4 climatic params)
  
  return(st)
  rm(list=c('mrb', 'environ', 'stat'))
}


# PREPARE DYNAMIC DATA ---------------------------------------------------------

aggregate_stack_dynamic_LU_ExpRaster = function(spatial_res='500', write=F) {
  # dynamic crop_LU exp vars
  # names are Dynamic_ctr_X
  # where ctr is the number of LUs assessed
  # and X is the year to be used
  
  LUs = c('Fruit_trees','Olive_groves','Pastures','Permanently_irrigated','Rice','Vineyards', 'Non_irrigated')
  store = list()  # to store
  ctr = 1
  
  for (LU in LUs) {
    
    r_LU = stack(paste0('./LULCC/Output/Exploratory_variables/Dynamic_stack/', LU, '.tif.tif'))
    r_LU = r_LU[[3:31]] # subset because the stacks are for the period 1987-2018
    r_LU = unstack(r_LU)
    names(r_LU) = paste0('EF_',ctr,'_', seq(0,length(r_LU)-1, 1))
    store = append(store, r_LU)
    ctr = ctr + 1
  }

  store = stack(store)
  
  # TODO:: WRONG
  if (write==TRUE) {
    # export vars names
    df = data.frame(ID = unlist(lapply(1:7, function(x) paste0('EF_', x,'_', seq(0, 30)))),
                    ID_name = unlist(lapply(LUs, function(x) paste0(x,'_', seq(1987,2017)))))
    export_file(module = 'LULCC', folder = 'Activity_data', subfolder = 'Exp_vars', subfolderX2 = 'Vars_list', file = df, filename = 'Dynamic_vars_LU')
  }
  
  return(store)
  rm(list=c('LUs','r_LU','ctr'))
}


aggregate_stack_dynamic_climatic_ExpRaster = function(spatial_res='500', write=F) {
  # aggregates into one stack the different stacks for meteoclimatic params
  # starts at 8
  
  params = c('PREC','TEMP_MAX','TEMP_MIN','TEMP_AVG')
  store = list()  # to store
  ctr = 1
#  ctr = 8 # start at 8
  for (param in params) {
    r_clim = stack(paste0('./LULCC/Output/Exploratory_variables/Dynamic_stack/', param, '.tif.tif'))
    r_clim = r_clim[[3:31]] # subset because the stacks are for the period 1987-2018
    r_clim = unstack(r_clim)
    names(r_clim) = paste0('EF_',ctr,'_', seq(0,length(r_clim)-1, 1))
    store = append(store, r_clim)
    ctr = ctr + 1
  }
  
  store = stack(store)
  
  # TODO:: WRONG
  if (write==TRUE) {
    # export vars names
    df = data.frame(ID = unlist(lapply(8:11, function(x) paste0('EF_', x,'_', seq(0, 30)))),
                    ID_name = unlist(lapply(params, function(x) paste0(x,'_', seq(1987,2017)))))
    export_file(module = 'LULCC', folder = 'Activity_data', subfolder = 'Exp_vars', subfolderX2 = 'Vars_list', file = df, filename = 'Dynamic_vars_climatic')
  }
  
  return(store)
  rm(list=c('LUs','r_LU','ctr'))
}

aggregate_all_ExpRaster = function(spatial_res = '500') {
  
  if (as.numeric(spatial_res) != 500) {
    stop('Only interested in doing this at 500m.')
  }
  else {
    
    static = aggregate_stack_ExpVarRaster('500')
    #dynamic_lu = aggregate_stack_dynamic_LU_ExpRaster(spatial_res)
  #  dynamic_clim = aggregate_stack_dynamic_climatic_ExpRaster(spatial_res)
    
   # st = stack(dynamic_clim, static)
    
    return(static)
    rm(list=c('static','dynamic_clim','dynamic_lu'))
  }
}


compile_ExpVars_df = function() {
  
  static = read.csv('./LULCC/Activity_data/Exp_vars/Vars_list/Static_vars.csv')
  static$type = 'Static'
  dynamic_clim  =  read.csv('./LULCC/Activity_data/Exp_vars/Vars_list/Dynamic_vars_climatic.csv')
  dynamic_clim$type = 'Dynamic'
  dynamic_lu  =  read.csv('./LULCC/Activity_data/Exp_vars/Vars_list/Dynamic_vars_LU.csv')
  dynamic_lu$type = 'Dynamic'
  
  all = rbind(dynamic_lu, dynamic_clim, static)
  export_file(module = 'LULCC', folder = 'Activity_data', subfolder = 'Exp_vars', subfolderX2 = 'Vars_list', file = all, filename = 'All_vars')
  
  return(all)
}



feed_ExpVarRasterList = function(admin, admin_id, spatial_res='500') {
  
  st_expVar = stack(aggregate_all_ExpRaster(spatial_res))

  if (missing(admin)==TRUE | missing(admin_id)==TRUE) {
      st_expVar = ExpVarRasterList(x = st_expVar, dynamic=F)
  }
  else {
    st_expVar = general_RasterCrop_admin(module = 'LULCC',r_file = st_expVar, admin = admin, admin_id = admin_id)
    st_expVar = ExpVarRasterList(x = st_expVar)
  
  }
  return(st_expVar)
}


