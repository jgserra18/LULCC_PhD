source('./Main/Global_functions.R')
source('./Main/General_GIS.R')


require('dplyr')
require('gstat')

# calculate sum, avg for each station
# apply grid
# downscale using elevation, latitude, longitude


yrs  = seq(1987,2017)

meteoclim_db = get_activity_data(module = 'LULCC', folder = 'Meteoclimatic_vars', pattern = 'params')


meteoclim_db = meteoclim_db %>%
  group_by(GRID_NO, year, LATITUDE, LONGITUDE, ALTITUDE) %>%
  arrange(year) %>%
  summarize(TEMP_AVG = mean(TEMPERATURE_AVG),
            PREC = sum(PRECIPITATION),
            ET_0 = sum(ET0),
            TEMP_MIN  = mean(TEMPERATURE_MIN),
            TEMP_MAX = mean(TEMPERATURE_MAX))


d_coast = raster('./LULCC/Output/Exploratory_variables/MRB/500m/Distance_ocean.tif')
elev = raster('./LULCC/Activity_data/Environmental_params/500m/Elevation.tif')
lon = init(elev, 'x')
lat = init(elev, 'y')
downscaling_params = stack(elev, lon, lat, d_coast)
names(downscaling_params) = c('elev','lon','lat', 'coast')


predict_and_downscale_meteoclim_params  = function(meteoclim_db, downscaling_params) {
  
  params = c('TEMP_AVG','PREC','TEMP_MIN','TEMP_MAX')
  
  # template raster 25 x 25 km
  r_tmp = get_activity_data(module = 'LULCC', folder = 'CLC', subfolder = '500', pattern = '1990')
  r_tmp = raster(crs=crs(r_tmp), ext=extent(r_tmp), res=25000)
  
  # pt shapefile
  pt = read_sf('./LULCC/Activity_data/Admin/NUTS2.shp')
  
  param_ctr = 8 # start at 8 due to the existence of other 7 dynamic params

  for (param in params) {
    
    store = list()
    
    yr_ctr = 0
    
    for (yr in yrs) {
      
      yr_ctr = yr_ctr + 1

      # subset and convert to spatial points
      sb = subset(meteoclim_db, year == yr)
      sp = SpatialPoints(sb[, 4:3], proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs'))
      SPdf <- SpatialPointsDataFrame(sp, sb)
      SPdf <- spTransform(SPdf, CRSobj = CRS('+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'))
      
      # apply idw
      fm = as.formula(paste(param,'~ 1'))
      gs <- gstat(formula =fm , locations=SPdf, nmax=5, set=list(idp=2))
      r_idw <- interpolate(r_tmp, gs)
      r_idw = mask(crop(r_idw, extent(pt)), pt)
      
      # downscale using elevation, x and y
      ds_param = spatialEco::raster.downscale(x = downscaling_params, y = r_idw, p = 0.25, scatter = T)
      plot(ds_param$downscale)
      store = append(store, ds_param$downscale)
      names(store)[yr_ctr] = paste0('EF_',param_ctr,'_',yr_ctr-1)
      
    }
    # export this stack
    store = stack(store)
    export_file(module = 'LULCC', folder = 'Exploratory_variables', subfolder = 'Dynamic_stack', file = store, filename = paste0(param,'.tif'))
    
    param_ctr = param_ctr + 1
  }
  rm(list=c('r_tmp','pt','store','sb','SPdf','fm','gs','r_idw','ds_param'))
  gc()
}

predict_and_downscale_meteoclim_params(meteoclim_db, downscaling_params)

