source('./Main/Global_functions.R')

#' @source http://www.climatologylab.org/terraclimate.html
#' @params pet, ppt, q, tmax, tmin 


pt = get_activity_data(module = 'LULCC', folder = 'Admin', pattern = 'Agrarian')
pt = st_transform(pt, CRS('+proj=longlat +ellps=WGS84 +no_defs')) # long lat !

# define params and yrs
params = c('pet', 'ppt', 'q', 'tmax', 'tmin')
yrs = seq(1958, 2018)

# create dir to export raw data
sapply(params, function(x) dir.create(path = paste0('./Water/Activity_data/Raw/', x), recursive = T))


nc_postprocessing = function(nc_filepath, var) {
  #' @description postprocesses the global nc file to portugal!
  #' @param nc_filepath filepath of the nc file
  #' @param var see params
  
  # open and scrape nc data
  # TODO: is this necessary? 
  nc = nc_open(nc_filepath)
  varname<-names(nc[['var']])
  varunits <- ncatt_get(nc,varname,"units")[2]
  lon<-ncvar_get(nc,"lon")
  lat<-ncvar_get(nc,"lat", verbose = F)
  time<- ncvar_get(nc, "time")
  tunits <- ncatt_get(nc, "time", "units")[[2]]
  dlname <- ncatt_get(nc, varname,"long_name")[[2]]
  nc_close(nc)
  
  # open as brick, mask according to portugal
  rbrck = brick(nc_filepath)
  rbrck = mask(crop(rbrck, extent(pt)), pt)
  writeRaster(rbrck, filename = nc_filepath,
              format="CDF", varname=var, varunit=varunits$value, longname=dlname,
              xname="lon", yname="lat", zname="time", zunit=tunits, overwrite=TRUE)
  rm(list=c('nc','rbrck'))
}




for (var in params) {
  for (yr in yrs) {
    print(paste0('Downloading ', var, '::::', yr))
    url = paste0('https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_', var, '_',yr,'.nc')
    filepath = paste0('./Water/Activity_data/Raw/', var, '/', var,'_',yr,'.nc')
    utils::download.file(url = url, destfile = filepath, mod='wb')
    
    # post process nc file
    nc_postprocessing(nc_filepath = filepath, var = var)
  }
}
