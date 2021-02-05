source('./LULCC/Model/LULC/LULCC_modelling.R')
source('./LULCC/Model/LULC/lulcc_allocate_modification.R')


require(doParallel)



export_LULC_OrderHierarchy = function(unrestricted, file, admin_reg, admin_id, model_lc, spatial_res) {
  # exports the output FOM file for a given simulation to its adequate folder
  # EXAMPLE: fine_tune_orderedModel(spatial_res ='1000', iter = 1, model_lc = 'glm', param = param)
  
  print('Exporting ....')
  order_path = create_activityData_folders(module = 'LULCC', folder = 'Activity_data', subfolder = 'CLUES_params',subfolderX2 =  'Order')
  
  # does the model include any LU restriction?
  order_path = ifelse(unrestricted == TRUE,
    create_new_directory(order_path, 'Unrestricted'),
    create_new_directory(order_path, 'Restricted'))
  
  # create dir for admin_reg and admin_id
  if (missing(admin_reg)==TRUE && missing(admin_id)==TRUE) {
    order_path = create_new_directory(path = order_path, new_dir = 'PT')
  }
  else if (admin_reg=='PT' & admin_id == 'PT') {
    order_path = create_new_directory(path = order_path, new_dir = 'PT')
  }
  else if (missing(admin_id)==TRUE) {
    order_path = create_new_directory(path = order_path, new_dir = 'PT')
  }
  else {
    order_path = create_new_directory(path = order_path, new_dir = admin_reg)
    order_path = create_new_directory(path = order_path, new_dir = admin_id)
  }
  # create dir for spatial res
  order_path = create_new_directory(path = order_path, new_dir = paste0(spatial_res, 'm'))
  
  file_name = correct_filename_iter(path = order_path, filename = model_lc)
  write.csv(x = file, file = file_name)
  rm(list=c('order_path', 'file_name'))
}
admin='NUTS2'
admin_id = 15
spatial_res = '500'
model_lc = 'glm'
iter = 1



fine_tune_orderedModel('NUTS2',15,'500',1,'glm')
fine_tune_orderedModel = function(admin, admin_id, spatial_res, iter, model_lc, param, rs = TRUE) {
  # fine tune LULC ordered model land use order
  # admin reg is the administrative unit (e.g., NUTS2, NVZ)
  # admin_id specifies a string (e.g., Tejo) or an integer (e.g., 15)
  # model_lc represents the specified model to be run (rpart, rf, glm)
  # param is the aggregated dataset containing CLC LULCC, Exp Vars and the training partition
  

  cl = makePSOCKcluster(3)
  registerDoParallel(cl)

  if (missing(param)==TRUE) {
    if ((missing(admin)==TRUE && missing(admin_id)==TRUE) | (admin=='PT' & admin_id=='PT')) {
      param = set_LULCC_params(spatial_res = spatial_res)
    } else {
      param = set_LULCC_params(admin = admin, admin_id = admin_id, spatial_res = spatial_res)
    }
  } 

  print(paste0('====Computing ', model_lc))
  glm_model = compute_LULCC_models(params = param, model = model_lc)
  print(paste0('==== Computing demand'))
  dmd = compute_LULCC_demand(param = param)
  
  if (admin == 'NUTS2' & admin_id == 11) {
    lu_ids = c(2, 1, 4, 212, 321, 221, 231, 211,   3, 512, 222, 223)
  } else {
    lu_ids = c(3,212, 321,223,211,213,231,2,4,222,1,512) # random as this will be shuffled
  }
  
  #rules = read.csv('./LULCC/Activity_data/CLC/CLC_allow.csv', header = F)
  
  # store fom per timestepe
  store = data.frame(order_lc=NA, t_10=NA, t_16=NA, t_22=NA,t_28=NA)
  times = c(10,16,22,28)
  
  j=1
  print('Starting to fine tune elasticity')
  # fine tune model for a given agrarian region
  store = foreach(j=1:iter, #.verbose = T, 
                  # export functions from "lulcc_allocate_modification.R"
                   .combine = rbind,
                   .packages = c('raster', 'randomForest', 'lulcc'),
                   .export = c('allocate', 'export_LULC_OrderHierarchy','getExpVarRasterList_','as.data.frame.ExpVarRasterList_',
                               'ThreeMapComparison','FigureOfMerit', 'OrderedModel','updatehist_', 'autoConvert_',
                              'update_data_frame', 'ordered_','applyDecisionRules_','applyNeighbDecisionRules_','maxtprob_','updatehist_')) %dopar% {
                     

                     n_order = sample(lu_ids, 12) # random LU ids generator
                    
                     ordered_model = OrderedModel(obs=param[[2]],ef=param[[1]],models=glm_model,time=0:28, demand=dmd, 
                                                   order=n_order)
                     ordered_model = allocate(ordered_model, stochastic=F)
                     plot(ordered_model@output)
                     for (i in 1:4) {
                       
                       clues.fom = FigureOfMerit(x=ThreeMapComparison(x=ordered_model,factors=2^(1:8),timestep=times[i]))
                       res = unlist(list(
                         clues.fom@overall[[1]],
                         clues.fom@overall[[4]],
                         clues.fom@overall[[8]]))
                       store[j, paste0('t_',times[i])] = paste(round(res, 2), collapse = ',')
                       store[j, 1] = paste(n_order, collapse = ',')
                     }
                     View(store)
                     return(store)
          }
  
  export_LULC_OrderHierarchy(unrestricted = TRUE, file = store, admin_reg = admin, admin_id = admin_id, model_lc = model_lc, spatial_res = spatial_res)
  stopCluster(cl) 
  beepr::beep(sound=3) # beep beep 
  
  if (rs == TRUE) {
    .rs.restartR() # clean memory session
  }
  gc() 
  rm(list=c('ordered_model', 'clues.tabs', 'glm_model', 'clues.fom'))
}




loop_fineTuning_NUTS2_ordered(admin = 'NUTS2', spatial_res = '500', iter = 10, model_lc = 'glm')

loop_fineTuning_NUTS2_ordered = function(admin='NUTS2', spatial_res, iter, model_lc, rs = F) {

    
  nuts2_id = c(15,16,11,17,18)
  
  for (i in nuts2_id) {
    print(paste0('Fine tuning ==================', i))
    fine_tune_orderedModel(admin = admin, admin_id = i, spatial_res = spatial_res, iter = iter, model_lc = model_lc, rs = rs)
  }
}



compute_FomValidated_LULCC_NUTS2 = function(admin = 'NUTS2', spatial_res, model_lc) {
  
  nuts2_id = c(18,15,16,11,17)
  r_store = list()
  r_agr = list()
  r_fom = list()
  
  for (i in nuts2_id) {
    print(paste0('WORKING WITHIN ', i))
    
    param = set_LULCC_params(admin = admin, admin_id = as.integer(i), spatial_res = spatial_res)
    glm_model = compute_LULCC_models(params = param, model = model_lc)
    dmd = compute_LULCC_demand(param = param)
    fom = find_best_fom(unrestricted = T,admin = admin,admin_id = as.integer(i), spatial_res = spatial_res, model_lc = model_lc)
    order = fom[[1]]
    
    ordered_model = OrderedModel(obs=param[[2]],ef=param[[1]],models=glm_model,time=0:28, demand=dmd, 
                                  order=order)
    
    clues_model = allocate(ordered_model, stochastic = F)
    clues.tabs = ThreeMapComparison(x=clues_model,factors=2^(1:8),timestep=28)
    clues.agr = AgreementBudget(clues.tabs)
    clues.fom = FigureOfMerit(x=clues.tabs)
    
    r_fom = round(clues.fom@overall[[1]], 2)
    max_fom = round(fom[[2]], 2)
    
    if (r_fom < max_fom) {
      print('======= FoM are different!')
      print(paste0('FoM is ', r_fom, 'and max FoM is', max_fom))
      break
    }  else {
      print('====== FoMs MATCH !!! EVERYTHING OK')
      print(paste0('FoM is ', r_fom, 'and max FoM is', max_fom))
      
      out_lulc = clues_model@output
      names(out_lulc) = paste0(i,'_',model_lc, '_LULC_', seq(1990,2018))
      
      r_agr = append(r_agr, clues.agr)
      r_stor = append(r_store, out_lulc)
      r_fom = append(r_fom, clues.fom)
      #for (j in 1:nlayers(out_lulc)) {
    #    r_file = out_lulc[[j]]
    #      print('======== Exporting ....')
     #     export_file(module = 'LULCC', file = r_file, folder = 'LULC', filename = names(out_lulc)[j], 
     #                 subfolder = admin, subfolderX2 = i, subfolderX3 = spatial_res)
     # }
    }
  }
  return(list(r_store = r_store, agreement = r_agr, fom = r_fom))
}




prepare_output_validated_data = function(admin = 'NUTS2', spatial_res, model_lc) {
  
  output =  compute_FomValidated_LULCC_NUTS2(admin, spatial_res, model_lc)
  
  out_lu = output$r_store
  out_lu = do.call(raster::mosaic, out_lu)
  
  fom = output$fom
  agr = output$agreement
  
  return(list(annual_LU = out_lu, fom = fom, agr=agr))
}







find_best_fom = function(unrestricted, admin='PT', admin_id, spatial_res, model_lc) {
  # finds the best figure of merit!
    # by default it is only the first out of 8 spatial resolutions
  
    param_finder = ifelse(missing(admin_id)==TRUE,
           c(admin, paste0(spatial_res,'m')),
           c(admin, admin_id, paste0(spatial_res,'m')))
  
  #  param_finder = c(admin, admin_id, paste0(spatial_res,'m'))
    path = create_activityData_folders(module = 'LULCC', 'Activity_data', 'CLUES_params', 'Order')
    # does the model include any LU restriction?
      path = ifelse(unrestricted == TRUE,
             create_new_directory(path, 'Unrestricted'),
             create_new_directory(path, 'Restricted'))
      
    for (i in param_finder) {
      path = file.path(path, i)
    }
      
   # files with all simulations
   files = list.files(path, pattern = model_lc, full.names = TRUE)
   # find the best fom
   data = lapply(files, read.csv)
   df = do.call(rbind.data.frame, data)
   max_fom = max(df[,3])
   
   # find correspoding LULC order
   lu_order = df[which(df[,3]==max_fom),2]
   lu_order =  as.character(lu_order[[1]])
   lu_order = as.numeric(strsplit(lu_order, ',')[[1]])

   return(list(lu_order, max_fom))
   rm(list=c('param_finder','path','files','data','df'))
}


compute_annualOrdered_LULC = function(admin='PT', admin_id, spatial_res, model_lc, write) {
  # looks for the best FoM and computes annual LULC
  # annual LULC can be exported
  
  param = set_LULCC_params(admin,admin_id, spatial_res)
  glm_model = compute_LULCC_models(params = param, model = model_lc)
  dmd = compute_LULCC_demand(param = param)
  rules = get_LULCC_rules()
  order_vars = find_best_fom(unrestricted = TRUE, admin, admin_id, spatial_res, model_lc)
  order = order_vars[[1]]
  ordered_model = OrderedModel(obs=param[[2]],ef=param[[1]],models=glm_model,time=0:28, demand=dmd,
                                             order=order)
  ordered_model = allocate(ordered_model)
  clues_tabs = ThreeMapComparison(x=ordered_model,factors=2^(1:8),timestep=28)
  clues.fom = FigureOfMerit(x=clues_tabs)
  
  fom = round(clues.fom@overall[[1]], 3)
  max_fom = round(order_vars[[2]], 3)
  
  if (fom != max_fom) {
    print('======= FoM are different!')
    print(paste0('FoM is ', fom, 'and max FoM is', max_fom))
  }
  
  else {
    print('====== FoMs match.')
    print(paste0('FoM is ', fom, 'and max FoM is', max_fom))
    
    rm(list=c('clues_tabs', 'clues.fom'))
    out_lulc = ordered_model@output
    names(out_lulc) = paste0(model_lc, '_LULC_', seq(1990,2018,1))
    
    for (i in 1:nlayers(out_lulc)) {
      r_file = out_lulc[[i]]
      if (write == T) {
        print('======== Exporting ....')
        export_file(file = r_file, folder = 'LULC', filename = names(out_lulc)[i], 
                    subfolder = admin, subfolderX2 = admin_id, subfolderX3 = spatial_res)
        }
    }
    return(out_lulc)
  }
}



fnc = function() {
  
  order = c(211,213,222,1,223,212,2,321,3,244,243,242,231,221,241,512)
  order = c(212,223,243,221,222,321,231,213,512,211,242,1,3,244,241,2)
  order = c(241,321,512,213,231,212,244,1,221,223,222,242,3,211,243,2)
  order = c(222,244,512,212,223,242,243,3,211,231,221,241,1,213,321,2)
  order = c(243,3,321,221,242,212,223,244,211,213,231,2,241,222,1,512)
  order = c(243,1,242,2,241,321,3,244,221,222,211,213,223,231,212,512)
  
  rasterOptions(progress = 'window', maxmemory = 1e+15)
  nuts2_id = c(18,15,16,11,17)
  
  param = set_LULCC_params(admin = 'NUTS2', admin_id = 16, spatial_res = '500')
  glm_model = compute_LULCC_models(params = param, model = 'glm')
  dmd = compute_LULCC_demand(param = param)
  
  fom = find_best_fom(unrestricted = T,admin = 'NUTS2',admin_id = 16, spatial_res = '500',model_lc = 'glm')
  order = fom[[1]]
  
  ordered_model = OrderedModel(obs=param[[2]],ef=param[[1]],models=glm_model,time=0:28, demand=dmd, 
                                order=order)
  
  clues_model = allocate(ordered_model, stochastic = F)
  clues.tabs = ThreeMapComparison(x=clues_model,factors=2^(1:8),timestep=28)
  clues.agr = AgreementBudget(clues.tabs)
  clues.fom = FigureOfMerit(x=clues.tabs)
  
  r_fom = round(clues.fom@overall[[1]], 3)
  max_fom = round(fom[[2]], 3)
  
  if (r_fom != max_fom) {
    print('======= FoM are different!')
    print(paste0('FoM is ', r_fom, 'and max FoM is', max_fom))
  }  else {
    print('====== FoMs match.')
    print(paste0('FoM is ', r_fom, 'and max FoM is', max_fom))
  }
}
