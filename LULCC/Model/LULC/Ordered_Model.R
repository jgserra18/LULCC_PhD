source('./LULCC/Model/LULC/LULCC_modelling.R')


export_LULC_OrderHierarchy <- function(unrestricted, file, admin_reg, admin_id, model_lc, spatial_res, t) {
  # exports the output FOM file for a given simulation to its adequate folder
  # EXAMPLE: fine_tune_orderedModel(spatial_res ='1000', iter = 1, model_lc = 'glm', param = param)
  
  print('Exporting ....')
  order_path <- create_activityData_folders(module = 'LULCC', folder = 'Activity_data', subfolder = 'CLUES_params',subfolderX2 =  'Order')
  
  # does the model include any LU restriction?
  ifelse(unrestricted == TRUE,
    order_path <- create_new_directory(order_path, 'Unrestricted'),
    order_path <- create_new_directory(order_path, 'Restricted'))
  
  # create dir for admin_reg and admin_id
  if (missing(admin_reg)==TRUE && missing(admin_id)==TRUE) {
    order_path <- create_new_directory(path = order_path, new_dir = 'PT')
  }
  else if (missing(admin_id)==TRUE) {
    order_path <- create_new_directory(path = order_path, new_dir = 'PT')
  }
  else {
    order_path <- create_new_directory(path = order_path, new_dir = admin_reg)
    order_path <- create_new_directory(path = order_path, new_dir = admin_id)
  }
  # create dir for spatial res
  order_path <- create_new_directory(path = order_path, new_dir = paste0(spatial_res, 'm'))
  file_name <- correct_filename_iter(path = order_path, filename = model_lc)
  write.csv(file,  file_name, row.names = F)
  rm(list=c('order_path', 'file_name'))
}



# Ordered rules start now 20 Out 2020
# if FoMs dont match, develop a different approach (e.g., directly export everything related starting from a min FoM)

fine_tune_orderedModel <- function(admin, admin_id, spatial_res, iter, model_lc, param, rs = F, tt = c(0,10,16,22,28)) {
  # fine tune LULC ordered model land use order
  # admin reg is the administrative unit (e.g., NUTS2, NVZ)
  # admin_id specifies a string (e.g., Tejo) or an integer (e.g., 15)
  # model_lc represents the specified model to be run (rpart, rf, glm)
  # param is the aggregated dataset containing CLC LULCC, Exp Vars and the training partition
  

  cl = makePSOCKcluster(3)
  registerDoParallel(cl)
  
  if (missing(param)==TRUE) {
    if (missing(admin)==TRUE && missing(admin_id)==TRUE) {
      param <- set_LULCC_params(spatial_res = spatial_res)
    } else {
      param <- set_LULCC_params(admin = admin, admin_id = admin_id, spatial_res = spatial_res, tt = tt)
    }
  } 
  
  print(paste0('====Computing ', model_lc))
  glm_model <- compute_LULCC_models(params = param, model = model_lc)
  print(paste0('==== Computing demand'))
  dmd <- compute_LULCC_demand(param = param)

  if (admin == 'NUTS2' & admin_id == 11) {
    lu_ids = c(2, 1, 4, 212, 321, 221, 231, 211,   3, 512, 222, 223, 243,244)
  } else {
    lu_ids = c(1,2,3,4,211,212,213,221,222,223,231,321,512, 243,244) # random as this will be shuffled
  }
  
 # rules = as.matrix(read.csv('./LULCC/Activity_data/CLC/CLC_allow.csv', header = F))
  
  print('Starting to fine tune order')
  # fine tune model for a given agrarian region
  store <- foreach(j=1:iter, 
                   .export = c('param', 'glm_model', 'dmd', 'lu_ids', 't'),
                   .combine = rbind,
                   .packages = c('lulcc', 'raster', 'randomForest')) %dopar% {
                    
                    if(admin_id == 11) {  n_order = sample(lu_ids, 14) } else { n_order = sample(lu_ids, 15) } # random LU ids generator
                     
                     ordered_model <- lulcc::OrderedModel(obs=param[[2]],ef=param[[1]],models=glm_model,time=0:28, demand=dmd, 
                                                   order=n_order)
                     ordered_model <- lulcc::allocate(ordered_model, stochastic=F)
                     clues.fom <- FigureOfMerit(x=ThreeMapComparison(x=ordered_model,factors=2^(1:8),timestep=28))

                     result2 <- clues.fom@overall[[1]]
                     result4 <- clues.fom@overall[[4]]
                     result8 <- clues.fom@overall[[8]]
                     data.frame(order_lc=paste(n_order, collapse = ','),
                                fom_2=result2,
                                fom_4=result4,
                                fom_8=result8,
                                iter=j)
                   }
  print(store)
  export_LULC_OrderHierarchy(unrestricted = TRUE, file = store, admin_reg = admin, admin_id = admin_id, model_lc = model_lc, spatial_res = spatial_res, t = tt)
  stopCluster(cl) 
  
  beepr::beep(sound=3)
  # clean memory session
  if (rs == TRUE) {
    .rs.restartR()
  }

  rm(list=c('ordered_model', 'clues.tabs', 'glm_model', 'clues.fom'))
}

loop_fineTuning_NUTS2_ordered <- function(admin='NUTS2', spatial_res, iter, model_lc, rs = F, t = c(0,10,16,22,28)) {

  nuts2_id <- c(15,16,11,17,18)
  for (i in nuts2_id) {
    print(paste0('Fine tuning ==================', i))
    fine_tune_orderedModel(admin = admin, admin_id = i, spatial_res = spatial_res, iter = iter, model_lc = model_lc, rs = rs, t = t)
  }
}


find_best_fom <- function(unrestricted=T, admin='PT', admin_id, spatial_res='500', model_lc='glm') {
  # sapply(c('11','16','15','17','18'), function(x) find_best_fom(T,'NUTS2',x))
  # finds the best figure of merit!
    # by default it is only the first out of 8 spatial resolutions
  
    ifelse(missing(admin_id)==TRUE,
           param_finder <- c(admin, paste0(spatial_res,'m')),
           param_finder <- c(admin, admin_id, paste0(spatial_res,'m')))
  
  #  param_finder <- c(admin, admin_id, paste0(spatial_res,'m'))
    path <- create_activityData_folders(module = 'LULCC', 'Activity_data', 'CLUES_params', 'Order')
    # does the model include any LU restriction?
    path = ifelse(unrestricted == TRUE,
            create_new_directory(path, 'Unrestricted'),
              create_new_directory(path, 'Restricted'))
      
    for (i in param_finder) {
      path <- file.path(path, i)
    }
      
   # files with all simulations
   files <- list.files(path, pattern = model_lc, full.names = TRUE)
   # find the best fom
   data <- lapply(files, read.csv)
   df <- do.call(rbind.data.frame, data)
   max_fom <- max(df[,2])
  
   # find correspoding LULC order
   lu_order <- df[which(df[,2]==max_fom),1]
   lu_order <-  as.character(lu_order[[1]])
   lu_order <- as.numeric(strsplit(lu_order, ',')[[1]])
   
   return(list(lu_order, max_fom))
   rm(list=c('param_finder','path','files','data','df'))
}



compute_FomValidated_LULCC_NUTS2 <- function(admin = 'NUTS2', spatial_res='500', model_lc='glm', write=F) {
  # computes the FoM validated NUTS2 LULC maps and export those
  # these need to be mosaiced afterwards!
  
  nuts2_id <- c(18,15,16,11,17)
  r_store <- list()
  
  for (i in nuts2_id) {
    print(paste0('WORKING WITHIN ', i))
    
    param <- set_LULCC_params(admin = admin, admin_id =i, spatial_res = spatial_res, t=c(0,10,16,22,28))
    glm_model <- compute_LULCC_models(params = param, model = model_lc)
    dmd <- compute_LULCC_demand(param = param, t=c(0,10,16,22,28))
    fom <- find_best_fom(unrestricted = T,admin = admin,admin_id = as.character(i), spatial_res = spatial_res, model_lc = model_lc)
    n_order <- fom[[1]]
    
    ordered_model <- OrderedModel(obs=param[[2]],ef=param[[1]],models=glm_model,time=0:28, demand=dmd, 
                                  order=n_order)
    clues_model <- lulcc::allocate(ordered_model, stochastic = F)
    clues.tabs <- ThreeMapComparison(x=clues_model,factors=2^(1:8),timestep=28)
    #   clues.agr <- AgreementBudget(clues.tabs)
    clues.fom <- FigureOfMerit(x=clues.tabs)
    
    r_fom <- round(clues.fom@overall[[1]], 2)
    max_fom <- round(fom[[2]], 2)
    
    if (r_fom != max_fom) {
      print('======= FoM are different!')
      print(paste0('FoM is ', r_fom, 'and max FoM is', max_fom))
      #  break
      next 
    }  else {
      print('====== FoMs MATCH !!! EVERYTHING OK')
      print(paste0('FoM is ', r_fom, 'and max FoM is', max_fom))
      
      out_lulc <- clues_model@output
      names(out_lulc) <- paste0(i,'_',model_lc, '_LULC_', seq(1990,2018))
      
      for (j in 1:nlayers(out_lulc)) {
        r_file <- out_lulc[[j]]
        print('======== Exporting ....')
        export_file(module = 'LULCC', file = r_file, folder = 'LULC', filename = names(out_lulc)[j], 
                    subfolder = admin, subfolderX2 = i, subfolderX3 = spatial_res)
      }
    }
  }
}

include_alqueva_dam  = function() {
  # creates a raster mask (0 or 1) to mask the alqueva dam in CLUES/ORDERED models
  
  r_tmp = get_activity_data(module = 'LULCC', subfolder = 'CLC', subfolderX2 = '500', pattern = '1990')
  r_tmp[r_tmp>0] = 0
  
  dams  = get_activity_data(module = 'LULCC', subfolder = 'Physical_constraints', pattern = 'Dam_rivers')
  dams =subset(dams, codigo == 'PT07GUA1487C' | codigo == 'PT07GUA1487A' | codigo == 'PT07GUA1487C' | codigo == 'PT07GUA1487D') # alqueva
  
  dams = fasterize::fasterize(dams, r_tmp)
  dams = mask(crop(mosaic(dams, r_tmp, fun=sum), extent(r_tmp)), r_tmp)
  dams[dams==1] = 512  # waterbody lulc id
  
  return(dams)
  rm(list='r_tmp')
}


mosaic_FoM_validated_NUTS2_LULC = function(spatial_res ='500', model_lc  = 'glm', write = F) {
  # creates the mainland mosaic of all NUTS2 regions; the alqueva dam was superimoosed starting from 2004
  
  alqueva = include_alqueva_dam()
  yrs = as.character(seq(1990,2018))
  store = list()
  
  for (yr in yrs) {
    
    # mosaic NUTS2 LULC maps for a given year
    lulc_n2  = list.files(path = './LULCC/Output/LULC/NUTS2/', pattern = paste0(model_lc,'_LULC_',yr), full.names = T, recursive = T)
    lulc_n2 = lapply(lulc_n2, raster)
    lulc_n2$fun=sum
    lulc_n2 = do.call(mosaic, lulc_n2)
    lulc_n2[lulc_n2==0] = NA
    
    # include the Alqueva dam?
    if (as.numeric(yr)>=2004) {
      lulc_n2 = overlay(lulc_n2, alqueva, fun=function(x, y) { ifelse( y == 512, 512, x) } ) 
    }
    
    store = append(store, lulc_n2)
  }
  store = stack(store)
  names(store) = paste0('PT_',model_lc,'_LULC_', seq(1990,2018))

  for (i in 1:nlayers(store)) {
    r_file <- store[[i]]
    if (write == T) {
      print('======== Exporting ....')
      export_file(module = 'LULCC', file = r_file, folder = 'LULC', filename = names(store)[i], 
                  subfolder = 'PT', subfolderX2 = spatial_res)
    }
    else {
      return(store)
    }
  }
}
