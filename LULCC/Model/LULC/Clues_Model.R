source('./Model/LULC/LULCC_modelling.R')




export_LULC_elasticity <- function(unrestricted, file, admin_reg, admin_id, model_lc, spatial_res) {
  # exports the output FOM file for a given simulation to its adequate folder
  # EXAMPLE: fine_tune_orderedModel(spatial_res ='1000', iter = 1, model_lc = 'glm', param = param)
  
  print('Exporting ....')
  order_path <- create_activityData_folders('Activity_data', 'CLUES_params', 'Elasticity')
  
  # does the model include any LU restriction?
  ifelse(unrestricted == TRUE,
         order_path <- create_new_directory(order_path, 'Unrestricted'),
         order_path <- create_new_directory(order_path, 'Restricted'))
  
  # create dir for admin_reg and admin_id
  if (missing(admin_reg)==TRUE && missing(admin_id)==TRUE) {
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


param <- set_LULCC_params('NVZ','Tejo','500')
glm_model <- compute_LULCC_models(params = param, model = 'glm')
dmd <- compute_LULCC_demand(param = param)
rules <- get_LULCC_rules()
elas <- c(0.43,0.16,0.93,0.62,0.18,0.73,0.21,0.37,0.65,0.56,0.18,0,0.2,0.24,0.43,0.98)
clues_info <- get_CLUES_info(param)

clues_model <- CluesModel(obs = param[[2]], 
                          ef = param[[1]], 
                          models=glm_model, 
                          time=0:28, 
                          demand=dmd, 
                          elas=elas, 
                          rules=clues_info[[1]], 
                          params=clues_info[[2]])
clues_model <- allocate(clues_model)
clues.tabs <- ThreeMapComparison(x=clues_model,factors=2^(1:8),timestep=28)
clues.fom <- FigureOfMerit(x=clues.tabs)
clues.fom@overall[[1]]
plot(AgreementBudget(clues.tabs))

fine_tune_LULC_admin <- function(admin, admin_id, spatial_res, iter, model_lc, param) {
  
  require(doParallel)
  require(doRNG)
  registerDoParallel(cores=2)
  getDoParWorkers()
  registerDoRNG(seed = 194842)
  
  if (missing(param)==TRUE) {
    if (missing(admin)==TRUE && missing(admin_id)==TRUE) {
      param <- set_LULCC_params(spatial_res = spatial_res)
    } else {
      param <- set_LULCC_params(admin = admin, admin_id = admin_id, spatial_res = spatial_res)
    } 
  }
  glm_model <- compute_LULCC_models(params = param, model = model_lc)
  dmd <- compute_LULCC_demand(param = param)
  clues_info <- get_CLUES_info(param)
  
  print('Starting to fine tune elasticity')
  # fine tune model for a given agrarian region
  store <- foreach(j=1:iter, 
                   .export = c('param', 'glm_model', 'dmd', 'clues_info'),
                   .combine = rbind,
                   .packages = c('lulcc', 'raster', 'randomForest')) %dopar% {
                     
                     elas <- c(round(runif(15),2), 0.98)
                     clues_model <- CluesModel(obs = param[[2]], 
                                               ef = param[[1]], 
                                               models=glm_model, 
                                               time=0:28, 
                                               demand=dmd, 
                                               elas=elas, 
                                               rules=clues_info[[1]], 
                                               params=clues_info[[2]])
                     clues_model <- allocate(clues_model)
                     clues.tabs <- ThreeMapComparison(x=clues_model,factors=2^(1:8),timestep=28)
                     clues.fom <- FigureOfMerit(x=clues.tabs)
                     result2 <- clues.fom@overall[[1]]
                     result4 <- clues.fom@overall[[4]]
                     result8 <- clues.fom@overall[[8]]
                     data.frame(elasticity=paste(elas, collapse = ','),
                                fom_2=result2,
                                fom_4=result4,
                                fom_8=result8,
                                iter=j)
                   }
  export_LULC_elasticity(unrestricted = TRUE, file = store, admin_reg = admin, admin_id = admin_id, model_lc = model_lc, spatial_res = spatial_res)
  beepr::beep(sound=3)
  # clean memory session
  .rs.restartR()
  rm(list=c('elas','clues_model','clues.tabs','clues.fom','glm_model', 'dmd','param'))
  doParallel::stopImplicitCluster()
}
fine_tune_LULC_admin(admin = 'NVZ', admin_id = 'Tejo', spatial_res = '500', iter = 15, model_lc = 'rf')
