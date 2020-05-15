source('./LULCC/Model/LULC/LULCC_modelling.R')



rasterOptions(progress = 'window', maxmemory = 1e+11)

param <- set_LULCC_params(spatial_res = '1000')
glm_model <- compute_LULCC_models(params = param, model = 'glm')
dmd <- compute_LULCC_demand(param = param)

rules <- get_LULCC_rules()

order <- c(211,213,222,1,223,212,2,321,3,244,243,242,231,221,241,512)
order <- c(212,223,243,221,222,321,231,213,512,211,242,1,3,244,241,2)
order <- c(241,321,512,213,231,212,244,1,221,223,222,242,3,211,243,2)
order <- c(222,244,512,212,223,242,243,3,211,231,221,241,1,213,321,2)
order <- c(243,3,321,221,242,212,223,244,211,213,231,2,241,222,1,512)
fom <- find_best_fom(unrestricted = T, spatial_res = '1000',model_lc = 'glm')
order <- fom[[1]]
order
ordered_model <- OrderedModel(obs=param[[2]],ef=param[[1]],models=glm_model,time=0:28, demand=dmd, 
                              order=order)

clues_model <- allocate(ordered_model, stochastic = F)
clues.tabs <- ThreeMapComparison(x=clues_model,factors=2^(1:8),timestep=28)
clues.agr <- AgreementBudget(clues.tabs)
clues.fom <- FigureOfMerit(x=clues.tabs)
clues.fom@overall
plot(clues.fom)

export_LULC_OrderHierarchy <- function(unrestricted, file, admin_reg, admin_id, model_lc, spatial_res) {
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
  write.csv(x = file, file = file_name)
  rm(list=c('order_path', 'file_name'))
}


fine_tune_orderedModel(spatial_res = '1000', iter = 15, model_lc = 'glm')
fine_tune_orderedModel <- function(admin, admin_id, spatial_res, iter, model_lc, param) {
  # fine tune LULC ordered model land use order
  # admin reg is the administrative unit (e.g., NUTS2, NVZ)
  # admin_id specifies a string (e.g., Tejo) or an integer (e.g., 15)
  # model_lc represents the specified model to be run (rpart, rf, glm)
  # param is the aggregated dataset containing CLC LULCC, Exp Vars and the training partition
  
  require(doParallel)
  # require(doRNG)
  cl <- registerDoParallel(cores=2)
  getDoParWorkers()
  
  if (missing(param)==TRUE) {
    if (missing(admin)==TRUE && missing(admin_id)==TRUE) {
      param <- set_LULCC_params(spatial_res = spatial_res)
    } else {
      param <- set_LULCC_params(admin = admin, admin_id = admin_id, spatial_res = spatial_res)
    }
  } 
  
  print(paste0('====Computing ', model_lc))
  glm_model <- compute_LULCC_models(params = param, model = model_lc)
  print(paste0('==== Computing demand'))
  dmd <- compute_LULCC_demand(param = param)
  
  lu_ids <- c(243,3,321,221,242,212,223,244,211,213,231,2,241,222,1,512) # random as this will be shuffled
 # lu_ids <- get_activity_data(folder = 'CLUES_param', pattern = 'ordered')[, 1]
  clues_info <- get_CLUES_info(param)
  
  # registerDoRNG(seed = 194842)
  print('Starting to fine tune elasticity')
  # fine tune model for a given agrarian region
  store <- foreach(j=1:iter, 
                   .export = c('param', 'glm_model', 'dmd', 'clues_info', 'lu_ids'),
                   .combine = rbind,
                   .packages = c('lulcc', 'raster', 'randomForest')) %dopar% {
                     
                     n_order <- sample(lu_ids, 16)
                     
                     ordered_model <- OrderedModel(obs=param[[2]],ef=param[[1]],models=glm_model,time=0:28, demand=dmd, 
                                                   order=n_order)
                     ordered_model <- allocate(ordered_model, stochastic=F)
                     clues.tabs <- ThreeMapComparison(x=ordered_model,factors=2^(1:8),timestep=28)
                     clues.fom <- FigureOfMerit(x=clues.tabs)
                     result2 <- clues.fom@overall[[1]]
                     result4 <- clues.fom@overall[[4]]
                     result8 <- clues.fom@overall[[8]]
                     data.frame(order_lc=paste(n_order, collapse = ','),
                                fom_2=result2,
                                fom_4=result4,
                                fom_8=result8,
                                iter=j)
                   }
  export_LULC_OrderHierarchy(unrestricted = TRUE, file = store, admin_reg = admin, admin_id = admin_id, model_lc = model_lc, spatial_res = spatial_res)
  beepr::beep(sound=3)
  # clean memory session
  .rs.restartR()
  
  rm(list=c('ordered_model', 'clues.tabs', 'glm_model', 'clues.fom'))
  doParallel::stopImplicitCluster()
}

loop_fineTuning_Ordered <- function(admin, admin_id, iter, model_lc, param) {
  
  res <- c('200','500','1000')
  for (i in res) {
    print(i)
    fine_tune_orderedModel(admin = admin, admin_id = admin_id, spatial_res = i, iter = iter, model_lc = model_lc, param = param)
  }
}
fine_tune_orderedModel(admin='PT',spatial_res = '1000', iter = 25, model_lc = 'glm')







find_best_fom <- function(unrestricted, admin='PT', admin_id, spatial_res, model_lc) {
  # finds the best figure of merit!
    # by default it is only the first out of 8 spatial resolutions
  
    ifelse(missing(admin_id)==TRUE,
           param_finder <- c(admin, paste0(spatial_res,'m')),
           param_finder <- c(admin, admin_id, paste0(spatial_res,'m')))
  
  #  param_finder <- c(admin, admin_id, paste0(spatial_res,'m'))
    path <- create_activityData_folders(module = 'LULCC', 'Activity_data', 'CLUES_params', 'Order')
    # does the model include any LU restriction?
      ifelse(unrestricted == TRUE,
             path <- create_new_directory(path, 'Unrestricted'),
             path <- create_new_directory(path, 'Restricted'))
      
    for (i in param_finder) {
      path <- file.path(path, i)
    }
      
   # files with all simulations
   files <- list.files(path, pattern = model_lc, full.names = TRUE)
   # find the best fom
   data <- lapply(files, read.csv)
   df <- do.call(rbind.data.frame, data)
   max_fom <- max(df[,3])
   
   # find correspoding LULC order
   lu_order <- df[which(df[,3]==max_fom),2]
   lu_order <-  as.character(lu_order[[1]])
   lu_order <- as.numeric(strsplit(lu_order, ',')[[1]])

   return(list(lu_order, max_fom))
   rm(list=c('param_finder','path','files','data','df'))
}


compute_annualOrdered_LULC(spatial_res = '1000', model_lc = 'glm', write = T)
compute_annualOrdered_LULC <- function(admin='PT', admin_id, spatial_res, model_lc, write) {
  # looks for the best FoM and computes annual LULC
  # annual LULC can be exported
  
  param <- set_LULCC_params(admin,admin_id, spatial_res)
  glm_model <- compute_LULCC_models(params = param, model = model_lc)
  dmd <- compute_LULCC_demand(param = param)
  rules <- get_LULCC_rules()
  order_vars <- find_best_fom(unrestricted = TRUE, admin, admin_id, spatial_res, model_lc)
  order <- order_vars[[1]]
  ordered_model <- OrderedModel(obs=param[[2]],ef=param[[1]],models=glm_model,time=0:28, demand=dmd,
                                             order=order)
  ordered_model <- allocate(ordered_model)
  clues_tabs <- ThreeMapComparison(x=ordered_model,factors=2^(1:8),timestep=28)
  clues.fom <- FigureOfMerit(x=clues_tabs)
  
  fom <- round(clues.fom@overall[[1]], 3)
  max_fom <- round(order_vars[[2]], 3)
  
  if (fom != max_fom) {
    print('======= FoM are different!')
    print(paste0('FoM is ', fom, 'and max FoM is', max_fom))
  }
  
  else {
    print('====== FoMs match.')
    print(paste0('FoM is ', fom, 'and max FoM is', max_fom))
    
    rm(list=c('clues_tabs', 'clues.fom'))
    out_lulc <- ordered_model@output
    names(out_lulc) <- paste0(model_lc, '_LULC_', seq(1990,2018,1))
    
    for (i in 1:nlayers(out_lulc)) {
      r_file <- out_lulc[[i]]
      if (write == T) {
        print('======== Exporting ....')
        export_file(file = r_file, folder = 'LULC', filename = names(out_lulc)[i], 
                    subfolder = admin, subfolderX2 = admin_id, subfolderX3 = spatial_res)
        }
    }
    return(out_lulc)
  }
}

compute_annualOrdered_LULC('NVZ','Tejo','200', 'glm', T)
