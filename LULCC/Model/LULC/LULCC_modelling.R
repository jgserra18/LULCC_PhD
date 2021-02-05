source('./LULCC/Model/PreProcessing/Organize_ObsLulccRasterStack.R')
source('./LULCC/Model/PreProcessing/Organize_ExpVarRasterList.R')


require(caTools)
require(doParallel)
require('ggplot2')

## LULCC MODELLING EQUATIONS --------------------------------------------------------------------------------------



x_predict_arg = function(x_ini, x_end, vector_numbers) {
  
  if (missing(vector_numbers)==TRUE) {
    x_pred = paste0('ef', seq(x_ini, x_end), collapse = '+')
  } 
  else if (missing(x_ini)==T && missing(x_end)==T) {
    x_pred = paste0('ef', vector_numbers, collapse = '+')
  }
  else {
    x_pred = paste0('ef', seq(x_ini, x_end), collapse = '+')
    x_pred = paste0(x_pred, '+', paste0('ef',vector_numbers, collapse = '+'))
  }
  return(x_pred)
}


lulcc_x_predictors <- function() {
  
  x_predictor <- data.frame(x=c(
    x_predict_arg(1,27),
    x_predict_arg(1,27),
    x_predict_arg(2,27),
    x_predict_arg(1,27),
    x_predict_arg(1,27),
    x_predict_arg(1,27),
    x_predict_arg(1,27),
    x_predict_arg(1,27),
    x_predict_arg(1,27),
    x_predict_arg(1,27),
    x_predict_arg(1,27),
    x_predict_arg(1,27),
    x_predict_arg(1,27),
    x_predict_arg(1,27),
    x_predict_arg(1,27) # including waterbodies mask
    ))
  colnames(x_predictor) <- 'lu_x'
  return(x_predictor)
}


create_model_formula <- function(st_clc) {
  # creates the LULC modelling formula
  # LU_y ~ Lu_x
  
  clc_label <-  as.character(get_CLC_info('label'))
  lulcc_x_pred <- lulcc_x_predictors()
  
  form_df <- data.frame(lu_y =  clc_label,
                        lu_x = lulcc_x_pred)
  
  # cross-reference with the existing categories in CLC
  # and update form_df
  clc_cat <- data.frame(lu_y=st_clc@labels)
  form_df <- dplyr::left_join(clc_cat, form_df, 'lu_y')
  
  # create formula list
  store_fm <- list()
  
  for (i in 1:nrow(form_df)) {
    
    fm <- as.formula(paste0(form_df[i,1], '~', form_df[i,2]))
    store_fm <- append(store_fm, fm)
  }
  
  return(store_fm)
  rm(list=c('clc_label', 'lulcc_x_pred', 'form_df'))
}


##  PREPARE LULCC PREDICTION ----------------------------------------------------------------------------------



# ALL PREDICTIONS ARE BASED ON ONLY ONE SET OF TRAINING IDS (FROM CLC_1990)
# HENCE WHY ALL THE PREDICTIONS FOR A GIVEN ADMINISTRATIVE REGION (E.G., ALENTEJO) REQUIRES A SINGLE RUN OF SETSEED!!!
# OTHERWISE ALL THE SIMULATIONS TO REFINE THE ORDER/ELASTICITY WILL BE DIFFERENT REGARDLESS!!!!!!!

create_setSeed_folder <- function(admin='PT', admin_id, spatial_res) {
  
  file_path <- create_new_directory('./LULCC/Activity_data/', 'Set_seed')

  if (missing(admin_id)==TRUE) {
    file_path <- create_new_directory(file_path, admin)
    file_path <- create_new_directory(file_path, ifelse(spatial_res=='Native', 'Native', paste0(spatial_res, 'm')))
  } 
  else {
    file_path <- create_new_directory(file_path, admin)
    file_path <- create_new_directory(file_path, admin_id)
    file_path <- create_new_directory(file_path, ifelse(spatial_res=='Native', 'Native', paste0(spatial_res, 'm')))
  }
  return(file_path)
}

export_setSeed_func <- function(admin='PT', admin_id, spatial_res, filename, file) {
  
  if (missing(admin)==TRUE && missing(admin_id)==TRUE) {
    file_path <- create_setSeed_folder(spatial_res = spatial_res)
  }
  else {
    file_path <- create_setSeed_folder(admin = admin, admin_id = admin_id, spatial_res = spatial_res)
  }
  
  file_path <- file.path(file_path, paste0(filename, '.csv'))
  write.csv(x = file, file_path, row.names = FALSE)
  return(file_path)
}


export_partition_RNG_index <- function(x, size, admin='PT', admin_id, spatial_res) {
  # to be used only ONCE
  # all the model will be based upon this
  # the seeds vhange the data partitions and thus all the model simulations run henceforth
  # IMPORTANT: THIS MUST BE RUN FOR EVERY SUBSET (E.G., TAGUS) AND SPATIAL RESOLUTION
  
  if (missing(admin)==TRUE| missing(admin_id)==TRUE) {
    test_path <- create_setSeed_folder(spatial_res = spatial_res)
  } else {
    test_path <- create_setSeed_folder(admin = admin, admin_id = admin_id, spatial_res = spatial_res)
  }
  condition <- list.files(test_path)
  
  #check if set.seed file already exists
  if (identical(condition, character(0))==TRUE) {
    print('Exporting new set.seed for this administrative unit.')
    points <- raster::rasterToPoints(x, spatial=TRUE)
    cells <- raster::cellFromXY(x, points)
    
    set.seed(1000)
    train_ids <- sample.split(points@data[,1] , SplitRatio = size)
    train_ids <- which(train_ids==TRUE)
    train_ids_df <- data.frame(ids=train_ids)
    
    export_setSeed_func(admin = admin, admin_id = admin_id, spatial_res = spatial_res, filename = paste0('Train_ids', size*10), file = train_ids_df)
    rm(list=c('points', 'cells', 'train_ids' ,'train_ids_df'))
  }
  else {
    print('Set.seed already exported.')
  }
}

updated_partition <- function(x, size, admin='PT', admin_id, spatial_res, spatial=TRUE, ...) {
  
 # points <- raster::rasterToPoints(x, spatial=TRUE)
  #cells <- raster::cellFromXY(x, points)
 # set.seed(1000)
 # train_ids <- sample.split(points@data[,1] , SplitRatio = size)
 # train_ids <- which(train_ids==TRUE)
  
  if (missing(admin)==TRUE | missing(admin_id)==TRUE) {
    export_partition_RNG_index(x = x, size = size, spatial_res = spatial_res)
  } else {
    export_partition_RNG_index(x = x, size = size, admin = admin, admin_id = admin_id, spatial_res = spatial_res)
  }
         
  points <- raster::rasterToPoints(x, spatial=TRUE)
  cells <- raster::cellFromXY(x, points)
  
  train_ids <- create_setSeed_folder(admin, admin_id, spatial_res)
  train_ids <- read.csv(list.files(train_ids, full.names = TRUE))[,1]
  
  if (spatial == T) {
    points <- as(points, "SpatialPoints")
    
    train <- points[train_ids]
    test <- points[-train_ids]
    all <- points
  }
  else {
    train <- cells[train_ids]
    test <- cells[-train_ids]
    all <- points
  }
  out <- list(train=train, test=test, all=all)
  return(out)
}



create_CLC_partition <- function(st_clc, admin='PT', admin_id, spatial_res) {
  # creates the train and test partitions from the observed CLC_LULCC
  
 # part <- partition(x=st_clc[[1]], size=0.3, spatial=T)
  if(missing(admin)==TRUE | missing(admin_id)==TRUE) {
    part <- updated_partition(x = st_clc[[1]], size = 0.3, spatial_res = spatial_res)
  } else {
    part <- updated_partition(x = st_clc[[1]], size = 0.3, admin = admin, admin_id = admin_id, spatial_res = spatial_res)
  }
  return(part)
}



feed_getPredictiveModelInputData <- function(expVar, st_clc, admin='PT', admin_id, spatial_res, tt=c(0,10,16,22,28)) {
  # feeds  Mould's LULCC getPredictiveModelInputData function the necessary data
  # i.e., obs LULCC, Exploratory Vars and the train partition
  # output: a list where index1 is the partition data and index 2 is the train data
  
  if (missing(admin)==TRUE | missing(admin_id)==TRUE) {
    part <- create_CLC_partition(st_clc,spatial_res = spatial_res)
  } else {
    part <- create_CLC_partition(st_clc, admin, admin_id, spatial_res)
  }
  train_data <- getPredictiveModelInputData(obs=st_clc, ef=expVar, cells=part[['train']], t=tt[1])
  
  return(list(part, train_data))
}


set_LULCC_params <- function(admin='PT', admin_id, spatial_res='500', tt=c(0,10,16,22,28)) {
  # set and load LULCC params
  # output: list where
  # index 1 - ExpVar
  # index 2 - Obs_CLC 
  # index 3 - train data
  # index 4 - partition data
  
  if (missing(admin_id)==T) {
    expVar <- feed_ExpVarRasterList(admin=admin, spatial_res=spatial_res)
    st_clc <- feed_ObsLulcRasterStack(admin=admin, spatial_res=spatial_res, tt = tt)
    model_data <- feed_getPredictiveModelInputData(expVar = expVar, st_clc = st_clc, admin=admin,spatial_res = spatial_res, tt =tt)
  }
  else {
    expVar <- feed_ExpVarRasterList(admin, admin_id, spatial_res)
    st_clc <- feed_ObsLulcRasterStack(admin, admin_id, spatial_res, tt = tt)
    model_data <- feed_getPredictiveModelInputData(expVar, st_clc, admin, admin_id, spatial_res, tt = tt)
  }

  partition <- model_data[[1]]
  train_data <- model_data[[2]]
  
  store <- list(expVar, st_clc, train_data, partition)
  
  return(store)
  rm(list=c('expVar', 'st_clc', 'partition', 'train_data'))
}



##  LULCC MODELS PREDICTION ----------------------------------------------------------------------------------

.checkFormula <- function(formula, categories, labels) {
  dep <- sapply(formula, function(x) as.character(x)[2])
  
  if (any(is.na(categories), is.na(labels))) {
    stop("'categories' and 'labels' must be supplied if 'obs' is missing")
  } 
  
  if (length(categories) != length(labels)) {
    stop("'labels' must correspond to 'categories'")
  }
  
  ## if (!missing(obs)) {
  ##     categories <- obs@categories
  ##     labels <- obs@labels
  ## } else {
  ##     if (missing(categories) | missing(labels)) {
  ##         stop("'categories' and 'labels' must be supplied if 'obs' is missing")
  ##     } else {
  ##         if (length(categories) != length(labels)) {
  ##             stop("'labels' must correspond to 'categories'")
  ##         }
  ##     }
  ## }
  
  if (!all(labels %in% dep)) {
    stop("a formula must be supplied for each land use type")
  }
  
  formula <- formula[match(dep, labels)]
}


glmModels <- function(formula, family=binomial, model=FALSE, ..., obs, categories=NA, labels=NA) {
  
  
  #cl = makePSOCKcluster(3)
  #registerDoParallel(cl)
  
  glm.models <- list()
  
  if (!missing(obs)) {
    categories <- obs@categories
    labels <- obs@labels
  }
  formula <- .checkFormula(formula, categories, labels)
  
 # glm.models = foreach(i=1:length(formula), .combine=list, .multicombine=T, .export=c('train_data','family', 'formula'), .packages=c('lulcc','glm2')) %dopar% {
 #   glm.models = glm2(formula[[i]], family='binomial', control = list(maxit=100), data=train_data)
 # }
  #stopCluster(cl) 
  for (i in 1:length(formula)) {
    form <- formula[[i]]
    glm.models[[i]] <- glm2::glm2(form, family=family, model=model, control = list(maxit=100), ...)
  }
  
  out <- new("PredictiveModelList",
             models=glm.models,
             categories=categories,
             labels=labels)
}


randomForestModels <- function(formula, ..., obs, categories=NA, labels=NA) {
  
  
  rf.models <- list()
  
  if (!missing(obs)) {
    categories <- obs@categories
    labels <- obs@labels
  }
  formula <- .checkFormula(formula, categories, labels)

  
  
  for (i in 1:length(formula)) {
    form <- formula[[i]]
    rf.models[[i]] <- randomForest::randomForest(form, ...)
  }
  
  out <- new("PredictiveModelList",
             models=rf.models,
             categories=categories,
             labels=labels)
}

compute_LULCC_models <- function(params, model) {
  # predict LULCC based on the select models (glm, rpart, rf)
  # output: returns a prediction for the allocation of different LUs
  
  expVar <- params[[1]]
  st_clc <- params[[2]]
  train_data <- params[[3]]
    # correct train data NAs
  train_data[is.na(train_data)] <- 0
  
  forms <- create_model_formula(st_clc)
  
  # model conditional
  if (model=='glm') {
    glm_model <- glmModels(formula=forms, family=binomial, data=train_data, obs=st_clc)
    return(glm_model)
  }
  else if (model=='rpart') {
    rpart_model <- rpartModels(formula=forms, data=train_data, obs=st_clc)
    return(rpart_model)
  }
  else if (model=='rf') {
    rf_model <- randomForestModels(formula=forms, data=train_data, obs=st_clc)
    return(rf_model)
  }
}



# LULC MODEL PERFORMANCE ASSESSMENT AND VISUALIZATION --------------------------------------------------------------------------



compute_LULCC_prediction <- function(param, glm_model, tt = c(0,10,16,22,28)) {
  # predicts and quantifies the performance of the model
  # model is the output of compute_LULCC_models
  # output: list where
  # index 1 - predicted model (ie, to assess annual output @output)
  # index 2 - model performance
  
  # how to plot: 
  # plot(list(model_perf))
  
  test_data <- getPredictiveModelInputData(obs=param[[2]], ef=param[[1]], cells=param[[4]][["test"]], t = tt[5])
  test_data[is.na(test_data)] = 0
  # test_data = na.omit(test_data)
  
  pred_model <- PredictionList(models=glm_model, newdata=test_data)
  model_perf <- PerformanceList(pred=pred_model, measure="rch")
  
  store <- list(pred_model, model_perf)
  
  return(store)
  rm(list=c('test_data','pred_model','model_perf'))
}


compute_probability_map = function(param, LU_name) {
  # computes the probability maps for a given LU_name
  # param is the output of set_LULCC_params
  
  test_data = getPredictiveModelInputData(obs=param[[2]], ef=param[[1]], cells=param[[4]][["all"]])
  probmaps <- predict(object=glm_model,
                      newdata=test_data,
                      data.frame=TRUE)
  points <- rasterToPoints(param[[2]][[1]], spatial=TRUE)
  probmaps <- SpatialPointsDataFrame(points, probmaps)
  probmaps = st_as_sf(probmaps)
  probmaps = st_buffer(probmaps, dist=500)
  probmaps = fasterize::fasterize(probmaps, param[[2]][[1]], field=LU_name)
  
  return(probmaps)
}


ggplot_LULC_prediction = function(LULCC_prediction_output, tt = c(0,10,16,22,28)) {
  # LULCC_prediction_output is the output from compute_LULCC_prediction()
  # gets prediction performance data 
  # stores all into a df for each LU class (incl the AUC; index 2) and a ggplot (index 1)

  data = unlist(LULCC_prediction_output[[2]]@performance) #unpack all data
  store = data.frame()
  
  for (i in 1:length(data)) {
    
    df = data.frame(x=data[[i]]@x.values[[1]], y = data[[i]]@y.values[[1]], LU=LULCC_prediction_output[[1]]@labels[i], AUC = LULCC_prediction_output[[2]]@auc[i], t = paste(tt, collapse = ','))
    store = rbind(store,df)
  }
  
  plot = ggplot(store, aes(x=x,y=y)) + 
    geom_line(colour='red1') + 
    geom_abline(intercept = 0, slope = 1, colour='black') + 
    geom_label(data=store, aes(label=paste0('AUC: ', AUC)), x=0.8, y = 0.25) + 
    facet_wrap(LU~.,ncol=3) + 
    theme_test()
  
  return(list(AUC_data = store, plot_AUC = plot))
}


ggplot_LULC_prediction_all_timesteps = function(tt = c(0,10,16,22,28)) {
  
  store = data.frame()
  for (i in 1:(length(tt)-1)) {
    
    t_0 = tt[i]
    t_1 = tt[i+1]
    
    # get performance params for each LU for each timestep and store them
    param <- set_LULCC_params(spatial_res = '500', tt = c(t_0, t_1))
    glm_model <- compute_LULCC_models(params = param, model = 'glm')
    m = compute_LULCC_prediction(param, glm_model, tt= c(t_0, t_1))
    df = ggplot_LULC_prediction(LULCC_prediction_output = m, tt = c(t_0,t_1))
    
    store = rbind(store,df[[1]])
    rm(list = c('param','glm_model','m'))
  }
  # plot for each timestep (t_o, t_1)
  plot = ggplot(store, aes(x=x, y=y, colour=factor(t))) + 
    scale_colour_viridis_d() + 
    geom_line(size=1) + 
    geom_abline(intercept = 0, slope = 1, colour='black') + 
    # geom_label(data=store, aes(label=paste0('AUC: ', AUC)), x=0.8, y = 0.25) + 
    facet_wrap(LU~.,ncol=3) + 
    theme_test()
  
  return(list(all_data=store, ggplot_plot = plot))
}






##  LULCC HISTORICAL DEMAND -------------------------------------------------------------------------------------------



compute_LULCC_demand <- function(param, t = c(0,10,16,22,28)) {
  
  dmd <- approxExtrapDemand(obs=param[[2]], tout=0:28)
  return(dmd)
}


get_LULCC_elasticity <- function() {
  
  elas <- get_activity_data(module = 'LULCC',folder = 'CLUES_params', pattern = 'LULCC_elasticity')[, 'elasticity']
  return(elas)
}

get_LULCC_rules <- function() {
  
  rules <- get_activity_data(module = 'LULCC','CLUES_params', subfolder = 'Rules', pattern = 'LULCC_rules')
  rules <- as.matrix(rules)#[,-1]
  return(rules)
}


get_CLUES_info <- function(param) {
  # best clues.params
  #  clues.params <- list(jitter.f=0.0002,scale.f=0.000001,max.iter=2000,max.diff=5,ave.diff=5)
  #clues.params <- list(jitter.f=0.0001,scale.f=0.000002,max.iter=1000,max.diff=5,ave.diff=5)
  # best:   clues.params <- list(jitter.f=0.0001,scale.f=0.000005,max.iter=1000,max.diff=5,ave.diff=5)
  
  n <- length(param[[2]]@categories)
  clues.rules <- matrix(data=1, nrow=n, ncol=n, byrow=TRUE)
  clues.params <- list(jitter.f=0.0001,scale.f=0.01,max.iter=250,max.diff=1,ave.diff=1)
 # default: clues.params <- list(jitter.f=0.0002,scale.f=0.000001,max.iter=1000,max.diff=50,ave.diff=50)
  
  return(list(clues.rules, clues.params))
}


