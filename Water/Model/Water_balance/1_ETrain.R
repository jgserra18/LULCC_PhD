source('./Main/Global_functions.R')


r_tmp = raster(crs=CRS('+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs '), 
               res=c(3440,4480),
               nrows=127, ncols=100, 
               xmn=2634637, xmx=2978637, ymn=1728785, ymx=2297745)

#' @note this calculates the modified RAINFED land use kcs from GlobWat
#' @note these differ from the source given in get_LU_kc !

get_LU_kc = function(annual_LU) {
  #' @param annual_LU annual LU map
  #' @description calculates the rainfall dependent evapotranspirationkc
  LU_kc = get_activity_data(module = 'Water',folder = 'Other_params', subfolder = 'Tabled', pattern = 'LU_kc')
  msk = get_activity_data(module = 'LULCC', folder = 'Admin', pattern = 'Municipality')
  store = list()
  store = lapply(1:nrow(LU_kc), function(x) {
    print(x)
    LU_id = LU_kc[x, 1]
    LU_kc = LU_kc[x, 3]
    new_LU_kc = (annual_LU==LU_id) * LU_kc
    store[[i]] = new_LU_kc
  })
  
  store$fun = sum
  store = do.call(mosaic, store)
  store[store==0] = 1 # for rivers
  store = mask(crop(store, extent(msk)), msk)
  
  return(store)
  rm(list=c('msk','new_LU_kc'))
}



#' @note agriculture LU_kc must be set to 0
#' @notecheck this if weird values
compute_LU_kc = function() {
  #' @description exports the rainfall dependent evapotranspiration kc for 1995-2017
  #' @note resamples Lu_kc according to data from TerraClimate, thus calculting the dominant land use kc

  yrs = as.character(seq(1995,2017))
  lapply(seq_along(yrs), function(x) {
    print(yrs[x])
    LU = get_activity_data(module = 'LULCC', mainfolder = 'Output', folder = 'LULC', subfolder = 'PT', subfolderX2 = '500', pattern = yrs[x])
    LU = get_LU_kc(annual_LU = LU)
    # resampling 
    LU =  resample(LU, r_tmp, 'ngb')
    export_file(module = 'Water', folder = 'Activity_data', subfolder = 'Other_params',  subfolderX2 = 'Spatial', subfolderX3 = 'LU_kc', file = LU, filename = paste0('LU_kc_', yrs[x]))
  })
}
compute_LU_kc()

