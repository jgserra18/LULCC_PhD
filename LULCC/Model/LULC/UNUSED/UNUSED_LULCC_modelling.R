source('./LULCC/Model/PreProcessing/Organize_ObsLulccRasterStack.R')
source('./LULCC/Model/PreProcessing/Organize_ExpVarRasterList.R')


require(caTools)


## LULCC MODELLING EQUATIONS --------------------------------------------------------------------------------------


x_predict_arg = function(x_ini, x_end, vector_numbers) {
  
  if (missing(vector_numbers)==TRUE) {
    x_pred = paste0('EF_', seq(x_ini, x_end), collapse = '+')
  } 
  else if (missing(x_ini)==T && missing(x_end)==T) {
    x_pred = paste0('EF_', vector_numbers, collapse = '+')
  }
  else {
    x_pred = paste0('EF_', seq(x_ini, x_end), collapse = '+')
    x_pred = paste0(x_pred, '+', paste0('EF_',vector_numbers, collapse = '+'))
  }
  return(x_pred)
}


lulcc_x_predictors = function() {
  # currently ignores the statistical data (e.g., NO_agriholdings_1999)
  
  x_predictor = data.frame(x=c(
    x_predict_arg(5, 31), #urban
    x_predict_arg(5, 31), # forest
    x_predict_arg(5, 31), # wetlands
    x_predict_arg(vector_numbers = c(seq(5, 31), 3)), #hetrero
    x_predict_arg(vector_numbers = c(seq(5, 31), 7)), # non_irrig 
    x_predict_arg(vector_numbers = c(seq(5, 31), 4)), #perma_irrig
    x_predict_arg(vector_numbers = c(seq(5, 31), 5)), #rice
    x_predict_arg(vector_numbers = c(seq(5, 31), 6)), #vineyards
    x_predict_arg(vector_numbers = c(seq(5, 31), 1)), #fruit_trees
    x_predict_arg(vector_numbers = c(seq(5, 31), 2)), #olives
    x_predict_arg(vector_numbers = c(seq(5, 31), 3)), # pastures
    x_predict_arg(vector_numbers = c(seq(5, 31), 3)), #natural grass
    x_predict_arg(5, 31) #waterbody
    ))
  colnames(x_predictor) = 'lu_x'
  return(x_predictor)
}



lulcc_x_predictors = function() {
  # currently ignores the statistical data (e.g., NO_agriholdings_1999)
  
  x_predictor = data.frame(x=c(
    x_predict_arg(5, 31), #urban 
    x_predict_arg(5, 31), # forest
    x_predict_arg(5, 31), # wetlands
    x_predict_arg(1,31), #hetrero
    x_predict_arg(1,31), # non_irrig 
    x_predict_arg(1,31), #perma_irrig
    x_predict_arg(1,31),#rice
    x_predict_arg(1,31), #vineyards
    x_predict_arg(1,31),#fruit_trees
    x_predict_arg(1,31), #olives
    x_predict_arg(1,31), # pastures
    x_predict_arg(1,31), #natural grass
    x_predict_arg(1,31) #waterbody
  ))
  colnames(x_predictor) = 'lu_x'
  return(x_predictor)
}


st_clc = param[[2]]

create_model_formula = function(st_clc) {
  # creates the LULC modelling formula
  # LU_y ~ Lu_x
  
  clc_label =  as.character(get_CLC_info('label'))
  lulcc_x_pred = lulcc_x_predictors()
  
  form_df = data.frame(lu_y =  clc_label,
                        lu_x = lulcc_x_pred)
  
  # cross-reference with the existing categories in CLC
  # and update form_df
  clc_cat = data.frame(lu_y=st_clc@labels)
  form_df = dplyr::left_join(clc_cat, form_df, 'lu_y')
  
  # create formula list
  store_fm = list()
  
  for (i in 1:nrow(form_df)) {
    
    fm = as.formula(paste0(form_df[i,1], '~', form_df[i,2]))
    store_fm = append(store_fm, fm)
  }
  
  return(store_fm)
  rm(list=c('clc_label', 'lulcc_x_pred', 'form_df'))
}



##  PREPARE LULCC PREDICTION ----------------------------------------------------------------------------------



# ALL PREDICTIONS ARE BASED ON ONLY ONE SET OF TRAINING IDS (FROM CLC_1990)
# HENCE WHY ALL THE PREDICTIONS FOR A GIVEN ADMINISTRATIVE REGION (E.G., ALENTEJO) REQUIRES A SINGLE RUN OF SETSEED!!!
# OTHERWISE ALL THE SIMULATIONS TO REFINE THE ORDER/ELASTICITY WILL BE DIFFERENT REGARDLESS!!!!!!!

create_setSeed_folder = function(admin='PT', admin_id, spatial_res) {
  
  file_path = create_new_directory('./LULCC/Activity_data/', 'Set_seed')

  if (missing(admin_id)==TRUE) {
    file_path = create_new_directory(file_path, admin)
    file_path = create_new_directory(file_path, ifelse(spatial_res=='Native', 'Native', paste0(spatial_res, 'm')))
  } 
  else {
    file_path = create_new_directory(file_path, admin)
    file_path = create_new_directory(file_path, admin_id)
    file_path = create_new_directory(file_path, ifelse(spatial_res=='Native', 'Native', paste0(spatial_res, 'm')))
  }
  return(file_path)
}


export_setSeed_func = function(admin='PT', admin_id, spatial_res, filename, file) {
  
  if (missing(admin)==TRUE && missing(admin_id)==TRUE) {
    file_path = create_setSeed_folder(spatial_res = spatial_res)
  }
  else {
    file_path = create_setSeed_folder(admin = admin, admin_id = admin_id, spatial_res = spatial_res)
  }
  
  file_path = file.path(file_path, paste0(filename, '.csv'))
  write.csv(x = file, file_path, row.names = FALSE)
  return(file_path)
}


export_partition_RNG_index = function(x, size, admin='PT', admin_id, spatial_res) {
  # to be used only ONCE
  # all the model will be based upon this
  # the seeds vhange the data partitions and thus all the model simulations run henceforth
  # IMPORTANT: THIS MUST BE RUN FOR EVERY SUBSET (E.G., TAGUS) AND SPATIAL RESOLUTION
  
  if (missing(admin)==TRUE && missing(admin_id)==TRUE) {
    test_path = create_setSeed_folder(spatial_res = spatial_res)
  } else {
    test_path = create_setSeed_folder(admin = admin, admin_id = admin_id, spatial_res = spatial_res)
  }
  condition = list.files(test_path)
  
  #check if set.seed file already exists
  if (identical(condition, character(0))==TRUE) {
    print('Exporting new set.seed for this administrative unit.')
    points = raster::rasterToPoints(x, spatial=TRUE)
    cells = raster::cellFromXY(x, points)
    
    set.seed(1000)
    train_ids = sample.split(points@data[,1] , SplitRatio = size)
    train_ids = which(train_ids==TRUE)
    train_ids_df = data.frame(ids=train_ids)
    
    export_setSeed_func(admin = admin, admin_id = admin_id, spatial_res = spatial_res, filename = paste0('Train_ids', size*10), file = train_ids_df)
    rm(list=c('points', 'cells', 'train_ids' ,'train_ids_df'))
  }
  else {
    print('Set.seed already exported.')
  }
}


updated_partition = function(x, size, admin='PT', admin_id, spatial_res, spatial=TRUE, ...) {
  
 # points = raster::rasterToPoints(x, spatial=TRUE)
  #cells = raster::cellFromXY(x, points)
 # set.seed(1000)
 # train_ids = sample.split(points@data[,1] , SplitRatio = size)
 # train_ids = which(train_ids==TRUE)
  
  if (missing(admin)==TRUE & missing(admin_id)==TRUE) {
    export_partition_RNG_index(x = x, size = size, spatial_res = spatial_res)
  } else {
    export_partition_RNG_index(x = x, size = size, admin = admin, admin_id = admin_id, spatial_res = spatial_res)
  }
         
  points = raster::rasterToPoints(x, spatial=TRUE)
  cells = raster::cellFromXY(x, points)
  
  train_ids = create_setSeed_folder(admin, admin_id, spatial_res)
  train_ids = read.csv(list.files(train_ids, full.names = TRUE))[,1]
  
  if (spatial == T) {
    points = as(points, "SpatialPoints")
    
    train = points[train_ids]
    test = points[-train_ids]
    all = points
  }
  else {
    train = cells[train_ids]
    test = cells[-train_ids]
    all = points
  }
  
  out = list(train=train, test=test, all=all)
  return(out)
}


create_CLC_partition = function(st_clc, admin='PT', admin_id, spatial_res) {
  # creates the train and test partitions from the observed CLC_LULCC
  
 # part = partition(x=st_clc[[1]], size=0.3, spatial=T)
  if(missing(admin)==TRUE && missing(admin_id)==TRUE) {
    part = updated_partition(x = st_clc[[1]], size = 0.3, spatial_res = spatial_res)
  } else {
    part = updated_partition(x = st_clc[[1]], size = 0.3, admin = admin, admin_id = admin_id, spatial_res = spatial_res)
  }
  return(part)
}



feed_getPredictiveModelInputData = function(expVar, st_clc, admin='PT', admin_id, spatial_res) {
  # feeds  Mould's LULCC getPredictiveModelInputData function the necessary data
  # i.e., obs LULCC, Exploratory Vars and the train partition
  # output: a list where index1 is the partition data and index 2 is the train data
  
  if (missing(admin)==TRUE && missing(admin_id)==TRUE) {
    part = create_CLC_partition(st_clc,spatial_res = spatial_res)
  } else {
    part = create_CLC_partition(st_clc, admin, admin_id, spatial_res)
  }
  train_data = getPredictiveModelInputData(obs=st_clc, ef=expVar, cells=part[['train']],t=0)
  
  return(list(part, train_data))
}

set_LULCC_params = function(admin, admin_id, spatial_res) {
  # set and load LULCC params
  # output: list where
  # index 1 - ExpVar
  # index 2 - Obs_CLC 
  # index 3 - train data
  # index 4 - partition data
  
  print('Reading exp vars.')
  expVar = feed_ExpVarRasterList(admin, admin_id, spatial_res)
  print('Reading obs vars.')
  st_clc = feed_ObsLulcRasterStack(admin, admin_id, spatial_res)
  
  model_data = feed_getPredictiveModelInputData(expVar, st_clc, admin, admin_id, spatial_res)
  partition = model_data[[1]]
  train_data = model_data[[2]]
  
  store = list(expVar, st_clc, train_data, partition)
  
  return(store)
  rm(list=c('expVar', 'st_clc', 'partition', 'train_data'))
}

##  LULCC MODELS PREDICTION ----------------------------------------------------------------------------------

.checkFormula = function(formula, categories, labels) {
  dep = sapply(formula, function(x) as.character(x)[2])
  
  if (any(is.na(categories), is.na(labels))) {
    stop("'categories' and 'labels' must be supplied if 'obs' is missing")
  } 
  
  if (length(categories) != length(labels)) {
    stop("'labels' must correspond to 'categories'")
  }
  
  ## if (!missing(obs)) {
  ##     categories = obs@categories
  ##     labels = obs@labels
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
  
  formula = formula[match(dep, labels)]
}




glmModels = function(formula, family=binomial, model=FALSE, ..., obs, categories=NA, labels=NA) {
  
  glm.models = list()
  
  if (!missing(obs)) {
    categories = obs@categories
    labels = obs@labels
  }
  formula = .checkFormula(formula, categories, labels)
  
  for (i in 1:length(formula)) {
    form = formula[[i]]
    glm.models[[i]] = glm2::glm2(form, family=family, model=model, control = list(maxit=100), ...)
  }
  
  out = new("PredictiveModelList",
             models=glm.models,
             categories=categories,
             labels=labels)
}

compute_LULCC_models = function(params, model) {
  # predict LULCC based on the select models (glm, rpart, rf)
  # output: returns a prediction for the allocation of different LUs
  
  expVar = params[[1]]
  st_clc = params[[2]]
  train_data = params[[3]]
    # correct train data NAs
  train_data[is.na(train_data)] = 0
  
  forms = create_model_formula(st_clc)
  
  # model conditional
  if (model=='glm') {
    glm_model = glmModels(formula=forms, family=binomial, data=train_data, obs=st_clc)
    return(glm_model)
  }
  else if (model=='rpart') {
    rpart_model = rpartModels(formula=forms, data=train_data, obs=st_clc)
    return(rpart_model)
  }
  else if (model=='rf') {
    rf_model = randomForestModels(formula=forms, data=train_data, obs=st_clc)
    return(rf_model)
  }
}



compute_LULCC_prediction = function(param, glm_model) {
  # predicts and quantifies the performance of the model
  # model is the output of compute_LULCC_models
  # output: list where
  # index 1 - predicted model (ie, to assess annual output @output)
  # index 2 - model performance
  
  # how to plot: 
  # plot(list(pred_model))
  
  test_data = getPredictiveModelInputData(obs=d[[2]], ef=d[[1]], cells=d[[4]][["test"]], t=0)
  test_data = na.omit(test_data)
  
  pred_model = PredictionList(models=dd, newdata=test_data)
  model_perf = PerformanceList(pred=pred_model, measure="rch")

  store = list(pred_model, model_perf)
 # model_perf@performance[[1]]@x.name
  return(store)
  rm(list=c('test_data','pred_model','model_perf'))
}


##  LULCC HISTORICAL DEMAND -------------------------------------------------------------------------------------------


compute_LULCC_demand = function(param) {
  # period: 1987-2018
  
 # dmd = approxExtrapDemand(obs=param[[2]], tout=0:28)
  dmd = approxExtrapDemand(obs=param[[2]], tout=0:28)
  
  return(dmd)
}

get_LULCC_elasticity = function() {
  
  elas = get_activity_data(module = 'LULCC',folder = 'CLUES_params', pattern = 'LULCC_elasticity')[, 'elasticity']
  return(elas)
}

get_LULCC_rules = function() {
  
  rules = get_activity_data(module = 'LULCC','CLUES_params', subfolder = 'Rules', pattern = 'LULCC_rules')
  rules = as.matrix(rules)#[,-1]
  return(rules)
}


get_CLUES_info = function(param) {
  # best clues.params
  #  clues.params = list(jitter.f=0.0002,scale.f=0.000001,max.iter=2000,max.diff=5,ave.diff=5)
  #clues.params = list(jitter.f=0.0001,scale.f=0.000002,max.iter=1000,max.diff=5,ave.diff=5)
  # best:   clues.params = list(jitter.f=0.0001,scale.f=0.000005,max.iter=1000,max.diff=5,ave.diff=5)
  
  n = length(param[[2]]@categories)
  clues.rules = matrix(data=1, nrow=n, ncol=n, byrow=TRUE)
  clues.params = list(jitter.f=0.0001,scale.f=0.000005,max.iter=1000,max.diff=5,ave.diff=5)
 # default: clues.params = list(jitter.f=0.0002,scale.f=0.000001,max.iter=1000,max.diff=50,ave.diff=50)
  
  return(list(clues.rules, clues.params))
}


