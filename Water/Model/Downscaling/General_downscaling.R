if (Sys.getenv("JAVA_HOME")!="") {
  Sys.setenv(JAVA_HOME="")
}
library(rJava)


#library(devtools)
#install_github(c("SantanderMetGroup/loadeR.java",
 #                  "SantanderMetGroup/climate4R.UDG",
  #                 "SantanderMetGroup/loadeR",
   #               # "SantanderMetGroup/transformeR",
    #               "SantanderMetGroup/visualizeR",
     #              "SantanderMetGroup/downscaleR"))

car = list('loadeR','transformeR','downscaleR','visualizeR','RColorBrewer')
lapply(car, require, character.only=T)

require('raster')
require('ncdf4')
library(climate4R.UDG)
lon = c(-9.5,-6); lat = c(36,42.2); seas = 1:12


#dataInventory(dataset = eobs) see params!
#C4R.vocabulary()
#C4R.vocabulary()


# small functions -----------------------------------------------------------------------------

compute_annualVals = function(grid, operation) {
  #' @param grid grid to work
  #' @param operation currently only sum or mean
  #' @description spatiotemporal aggregation
  
  if (operation == 'sum') {
    f = function(x) sum(x)
  } 
  else if (operation == 'mean') {
    f = function(x) mean(x)
  }
  else { stop('Only mean and sum.') }
  
  grid = aggregateGrid(grid, aggr.y = list(FUN= function(x) f(x)))
  return(grid)
}

export_gridToBrick = function(grid) {
  #' @param grid grid to work
  #' @description converts grid to a raster brick!
  grid = transformeR::grid2sp(grid)
  grid= brick(grid)
  grid
}


plot_grid = function(grid) {
  #' @param grid grid to work
  #' @description grid plot
  
  spatialPlot(climatology(grid), backdrop.theme = 'countries',
              col.regions=colorRampPalette(rev(brewer.pal(n=9,'RdYlBu'))))
}




# processing ------------------------------------------------


dirobs = '.\\Water\\Activity_data\\Raw' # directory with the main data


create_loadGridData = function(var, ncml_file, timestep = 1958:2018) {
  #' @param var tmax, pet, tmin, ppt
  #' @param nfcm_file eg, CDX_obs.ncml
  #' @description creates loadGridData to a given param, doing any dictionary modification
  
  makeAggregatedDataset(source.dir = dirobs,  recursive = T,ncml.file =ncml_file , pattern = var)
  if (var == 'ppt') { var_mod = 'prr' } else if (var == 'tmax') { var_mod = 'tasmax'} else if (var == 'tmin') { var_mod = 'tasmin'}

  # update dictionary
  dic = paste0('dicEOBS_',var,'.dic')
  file.create(dic)
  writeLines(c('identifier,short_name, time_step,lower_time_bound,upper_time_bound,cell_method,offset,scale,deaccum,derived,interface',
               paste0(var_mod,',',var,',24h,0,24,max,0,1,0,0,')), dic)
  
  SUvar = loadGridData(dataset = ncml_file, var = var_mod, season=1:12, lonLim = lon, latLim = lat, years = timestep,
                       dictionary=dic)
  SUvar
}

set_timestep = 1990:2000

tmax_hist = create_loadGridData(var = 'tmax','tmax_hist.ncml', timestep = set_timestep)
tmin_hist = create_loadGridData(var = 'tmin','tmin_hist.ncml', timestep = set_timestep)
pr_hist =  create_loadGridData(var = 'ppt','ppt_hist.ncml', timestep = set_timestep)
tmin_temp = aggregateGrid(tmin_hist, aggr.y = list(FUN= function(x) sum(x)))

ref_grid <- getGrid(tmin_temp) # The reference grid of the CRU data is retained in order to interpolate the RCM data afterwards:
ref_mask = gridArithmetics(tmin_hist, 0, operator='*')





