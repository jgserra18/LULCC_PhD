source('./Main/Global_functions.R')
source('./Main/General_GIS.R')

## ----------------------- LIBRARIES --------------------- 


library(lulcc)
library(dplyr)


## ----------------------- CLC_LULCC PreProcessing --------------------- 



reclassify_CLC = function() {
  # reclassifies (specified) non-agricultural land uses from CLC
  # note: waterbodies, except for inland waterbodies, were set to NA
  
  clc_rcl = get_activity_data(module = 'LULCC', 'CLC', 'specified')
  return(clc_rcl)
}



stack_all_CLC = function(spatial_res, t=c(0,10,16,22,28), write=F) {
  # creates a stack with the properly reclassified CLC versions for all years
  # returns a RasterStack
  # note: to be used in the LULCC framework
  
  clc_rcl = reclassify_CLC()
  
 #clc_yr <- c('1990', '2000', '2006', '2012', '2018')
  clc_yr = as.character(t + 1990)
  clc_stack = list()
  
  for (i in clc_yr) {
    print(paste0('Stacking CLC from ', i))
    
  #  spatial_res = ifelse(spatial_res=='Native', 'Native', paste0(spatial_res, 'm'))
    clc = get_activity_data(module = 'LULCC', folder = 'CLC', pattern = i, subfolder = spatial_res)
    clc = reclassify(clc, rcl = as.matrix(clc_rcl[, -4]))
    clc_stack = append(clc_stack, clc)
  }
  clc_stack = stack(clc_stack)

  if (write==TRUE) {
    export_file(module = 'LULCC',file = clc_stack, folder = 'Stacked_CLC', filename = 'Stacked_CLC_LU')
  }
  
  return(clc_stack)
  rm(list=c('clc_rcl', 'clc_yr', 'clc'))
}


get_CLC_info = function(label_or_cat) {
  # get CLC categories for further application within the LULCC framework
  # label_or_cat can be either label or category
  
  clc_cat = get_activity_data(module = 'LULCC','CLC', 'CLC_LULCC_label')
  
  if (label_or_cat=='label') {  clc_cat = clc_cat[, 3]  } else { clc_cat = clc_cat[, 1] }
  
  return(clc_cat)
}


CLC_df = function() {

  # create pre-defined CLC cat/label dataframe
  clc_cat = get_CLC_info('category')
  clc_label = as.character(get_CLC_info('label'))
  clc_df = data.frame(cat = clc_cat, label = clc_label)
  
  # merge with a given CLC map irrespective of spatial resolution and boundary
  #stclc_cats = unique(st_clc[[1]])
  #l_unique = lapply(stclc_cats, unique)
  #max_length = lapply(l_unique, length)
  #unique_cats = l_unique[[which.max(max_length)]]
  
  #new_cat = data.frame(cat=unique_cats)
  #new_cat = left_join(new_cat, clc_df, 'cat')
  return(clc_df)
  #return(new_cat)
  #rm(list=c('clc_cat', 'clc_label', 'clc_df', 'stclc_cats', 'l_unique', 'max_length'))
}


feed_ObsLulcRasterStack = function(admin, admin_id, spatial_res='500', tt = c(0,10,16,22,28)) {
  # feed the necessary input data to Mould's LULCC framework "ObsLulcRasterStack" function
  # i.e., CLC categories and labels and the CLC stack
 
  st_clc = stack_all_CLC(spatial_res = spatial_res, t = tt, write = F)
  
  if (missing(admin)==TRUE | missing(admin_id)==TRUE) {
    
    #clc_df = CLC_df(st_clc)
    clc_df = CLC_df()
    
    lu = ObsLulcRasterStack(x=st_clc,
                             categories=clc_df[,1],
                             labels = as.character(clc_df[, 2]),
                             t=tt)
  }
  else {
    st_clc = general_RasterCrop_admin(module = 'LULCC',r_file = st_clc, admin = admin, admin_id = as.numeric(admin_id))
    #clc_df = CLC_df(st_clc)
    clc_df = CLC_df()
    
    if (admin == 'NUTS2' & admin_id == 11) {
      
      cats = getValues(st_clc[[1]])
      cats = unique(cats)
      cats = cats[order(cats)]
      cats = cats[-which(is.na(cats)==T)]
      
      df = data.frame(cat = cats)
      clc_df = plyr::join(df, clc_df, 'cat')
    }
    lu = ObsLulcRasterStack(x=st_clc,
                             categories=clc_df[,1],
                             labels = as.character(clc_df[, 2]),
                              t=tt)
    
  }
  
  return(lu)
  rm(list=c('clc_cat', 'clc_label', 'st_clc'))
}

