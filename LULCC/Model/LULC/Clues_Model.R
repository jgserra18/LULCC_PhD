source('./LULCC/Model/LULC/LULCC_modelling.R')




export_LULC_elasticity <- function(unrestricted, file, admin_reg, admin_id, model_lc, spatial_res) {
  # exports the output FOM file for a given simulation to its adequate folder
  # EXAMPLE: fine_tune_orderedModel(spatial_res ='1000', iter = 1, model_lc = 'glm', param = param)
  
  print('Exporting ....')
  order_path <- create_activityData_folders(module = 'LULCC', 'Activity_data', 'CLUES_params', 'Elasticity')
  
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


admin='NUTS2'
admin_id = 15
spatial_res = '500'
model_lc = 'glm'
iter = 1
t = c(0,10,16,22,28)
param <- set_LULCC_params(admin = admin, admin_id = admin_id, spatial_res = spatial_res, t = t)
glm_model <- compute_LULCC_models(params = param, model = 'glm')

dmd <- compute_LULCC_demand(param = param)

elas <- c(0.95, 0.4, 0.95, 0.10, 0.1, 0.2, 0.8, 0.5, 0.5, 0.5, 0.2, 0.2, 0.5)
clues_info <- get_CLUES_info(param)
rules = as.matrix(read.csv('./LULCC/Activity_data/CLC/CLC_allow.csv', header = F))

clues_model <- CluesModel(obs = param[[2]], 
                          ef = param[[1]], 
                          models=glm_model, 
                          time=0:28, 
                          demand=dmd, 
                          elas=elas, 
                         # rules=rules,
                          params=clues_info[[2]])
clues_model <- allocate(clues_model)
clues.tabs <- ThreeMapComparison(x=clues_model,factors=2^(1:8),timestep=28)
clues.fom <- FigureOfMerit(x=clues.tabs)
clues.fom@overall[[1]]
plot(clues.fom)
plot(AgreementBudget(clues.tabs))

plot(clues_model@output)


grid = expand.grid(urban=seq(0.9, 1, 0.1), forest = seq(0.9, 1, 0.1), wetlands = seq(0.9, 1, 0.1), 
                   heterogeneous = seq(0.1, 0.5, 0.4), Non_irrigated = seq(0.1, 0.5, 0.4), 
                   Permanently_irrigated = seq(0.1, 0.5, 0.4), Rice = seq(0.1, 0.5, 0.4),
                   Vineyards = seq(0.2, 0.8, 0.6),  Fruit_trees = seq(0.2, 0.5, 0.3), Olive_groves = seq(0.2, 0.8,0.6), 
                   Pastures = seq(0.1, 0.5, 0.4), Natural_grasslands = seq(0.1, 0.5, 0.4), Water_bodies = seq(0.8, 1, 0.2))
nrow(grid)
View(grid)
fine_tune_LULC_admin <- function(admin, admin_id, spatial_res, iter, model_lc, param) {
  
  cl = makePSOCKcluster(3)
  registerDoParallel(cl)
  
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
                     expand.grid(elas)
                     elas <- c(round(runif(15),2), 1)
                     clues_model <- CluesModel(obs = param[[2]], 
                                               ef = param[[1]], 
                                               models=glm_model, 
                                               time=0:28, 
                                               demand=dmd, 
                                               elas=elas, 
                                              # rules=clues_info[[1]], 
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
fine_tune_LULC_admin(admin = 'NVZ', admin_id = 'Tejo', spatial_res = '500', iter = 50, model_lc = 'rpart')
