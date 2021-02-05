source('./Main/Global_functions.R')

library(doParallel)


compute_aggregated_LU_classes = function(year, spatial_res='500') {
  # 1 - aggregates each LU class at the specified resolution
  # 2 - computes the fraction of each LU within a cellgrid
  # important souce:
  # https://gis.stackexchange.com/questions/297908/calculating-block-statistics-from-land-cover-raster-using-r
  
  
  
  # reclassify LU according to agricultural LU + other simplified types
  clc_rcl <- get_activity_data(module = 'LULCC', 'CLC', 'specified')
  clc <- get_activity_data(module = 'LULCC', folder = 'CLC', pattern = year, subfolder = 'Native')
  clc <- reclassify(clc, rcl = as.matrix(clc_rcl[, -4]))

  # get LU ids
  ids = na.omit(unique(getValues(clc), na.rm=T))
  
  # PT template file
  pt = read_sf('./LULCC/Activity_data/Admin/Municipality.shp')
  
  # prepare to calculate aggregated LU classes (fraction of total cell)
  cl <- makePSOCKcluster(2)
  registerDoParallel(cl)
  
  # specify res
  res = res(clc)*5/100
  
  st_LU_agg = foreach(i=1:length(ids), .packages = 'raster') %dopar% {
    
    clc_id = aggregate(clc, fact=res, fun=function(vals, na.rm=T) {
      sum(vals==ids[i], na.rm=na.rm)/length(vals)
      }
    )
    clc_id  = mask(crop(clc_id, extent(pt)), pt)
    names(clc_id) = paste0('LU_', ids[i])
    # tifoptions=c('COMPRESS=DEFLATE', 'PREDICTOR=2', 'ZLEVEL=6')
    #writeRaster(clc_id, paste0('./clc_',ids[i],'.tif'), options=tifoptions, overwrite = TRUE)
    return(clc_id)
    rm(list='clc_id')
  }
  stopCluster(cl) 
  st_LU_agg = stack(st_LU_agg)
  
  
  return(st_LU_agg)
  rm(list=c('clc_rcl','clc','pt','res','ids'))
}



loop_LU_aggregation = function(spatial_res = '500') {
  
  yrs = c('1990','2000','2006','2012','2018')

  for (yr in yrs) {
    
    st_LU_agg = compute_aggregated_LU_classes(year = yr, spatial_res = spatial_res)
    
    for (i in 1:nlayers(st_LU_agg)) {
      
      export_file(module = 'LULCC', folder = 'Activity_data', subfolder = 'LU_aggregation', 
                  subfolderX2 = yr, file = st_LU_agg[[i]], filename = names(st_LU_agg)[[i]])
    }
  }
}
loop_LU_aggregation()

files = list.files(path = './LULCC/Activity_data/LU_aggregation/', recursive = T, pattern = '212', full.names = T)
files = stack(lapply(files, raster))

require('tmap')

tm_shape(files) + 
  tm_raster(breaks=seq(0,1,0.1), palette = 'Blues')
