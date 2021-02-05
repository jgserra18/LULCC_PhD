source('./Main/Global_functions.R')

# scraping framework not in use  ---------------------------------------------------------------------------
#library(rgee)
## @source https://developers.google.com/earth-engine/datasets/catalog/NASA_FLDAS_NOAH01_C_GL_M_V001 
# @references https://r-spatial.github.io/rgee/reference/ee_imagecollection_to_local.html
# @references https://github.com/r-spatial/rgee 
#ee_Initialize()
#scrape_FLDAS_climaticParams = function() {
  #' @description scrapes, using rgee, data for evapotranspiration (mm s-1), precipitation (mm s-1) and soil moisture 10-40 cm  (%)
  # from https://developers.google.com/earth-engine/datasets/catalog/NASA_FLDAS_NOAH01_C_GL_M_V001#bands
  #vars = c('Evap_tavg','Rainf_f_tavg','SoilMoi10_40cm_tavg', 'Tair_f_tavg', 'Qs_tavg','Qs_tavg') # evapotranspiration, precipitation, soil moisture
  #param = c('Evapotranspiration','Precipitation','SoilMoisture', 'Temperature', 'Subsurface_runoff','Surface_runoff')
  #collection = ee$ImageCollection('NASA/FLDAS/NOAH01/C/GL/M/V001')
  #ee_crs <- collection$first()$projection()$getInfo()$crs
  #geometry <- ee$Geometry$Rectangle(coords = c(-11, 34, 45, 72), proj = ee_crs, geodesic = FALSE)
 #for (i in 1:4) {
    #st_var = collection$select(vars[i])
    #st_var = st_var$filterDate(start = '1987-01-01', opt_end = '2018-01-01')
    # https://r-spatial.github.io/rgee/reference/ee_imagecollection_to_local.html
    #file_path = paste0('./Water/Activity_data/Climatic_params/Monthly/', param[i])
    #dir.create(file_path)
    #ee_imagecollection_to_local(ic = st_var,
     #                           region = geometry,
      #                          dsn = file.path(file_path,'/'), 
       #                         lazy=T, 
        #                        maxPixels = 1e20)
  #}
  #rm(list=c('collection','ee_crs','geometry','st_var','vars','param'))
#S}


# climateR scraping ----------------------------------------------------------------------------------

#remotes::install_github("mikejohnson51/AOI", dependencies = T) # suggested!
#remotes::install_github("mikejohnson51/climateR", dependencies = T)


library(AOI)
library(climateR)
library(raster)

pt = aoi_get(country = "Portugal") # define area of study
#yrs = seq(1995,2017) # define modelling timeperiod
yrs = seq(1987,2017) # define modelling timeperiod



get_monthYears = function(year) {
  
  start = paste0(year,'-01-01')
  end = paste0(year, '-12-01')
  return(list(start, end))
}



load_TerraClim_monthlyVars = function(var, start, end=NULL) {
  #' @param var either pet, prcp, q, soilm, tmin and tmax
  #' @param start startDate Yr-Month-day
  #' @param end endData Yr-Month-day
  #' @description gets terraclim vars and projects to LAEA
  #' @usage load_TerraClim_monthlyVars('prcp','1995-01-01','1995-12-01')
  #' 
  TC_var = getTerraClim(pt, param = var, startDate =start, endDate = end)
  # project and mask to PT
  TC_var = projectRaster(TC_var[[1]], 
                         crs = CRS('+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs '))
  msk = get_activity_data(module = 'LULCC', folder = 'Admin', pattern = 'Municipality')
  TC_var = mask(crop(TC_var, extent(msk)), msk)
  
  return(TC_var)
  rm(list='msk')
}


scrape_initial_soilMoisture = function() {
  #' @description scrapes the initial soil moisture for t - 1 (ie, December 1994)
  
  SM_t_1 = load_TerraClim_monthlyVars('soilm', '1994-12-01')
  export_file(module = 'Water', folder = 'Activity_data', subfolder = 'Other_params', subfolderX2 = 'Spatial', subfolderX3 = 'Conditions',
              filename =  'S_initial', file = SM_t_1)
  rm(list='SM_t_1')
}

scrape_TerraClim_monthlyVar = function(var) {
  #' @param var either pet, prcp, q, soilm
  
  if (var=='pet') { folder_name ='ET0' } else if (var == 'prcp') { folder_name='Prec' } else { folder_name = var}
  print('Scraping data from TerraClimate .........')
  export_var = load_TerraClim_monthlyVars(var, start = '1995-01-01', end = '2017-12-01')
  all_months = seq.Date(from = as.Date("1995/1/1"), to = as.Date("2017/12/1"), by = "month")

  sapply(1:nlayers(export_var), function(x)
    export_file(module = 'Water', folder = 'Activity_data', subfolder = 'Climatic_params', subfolderX2 = 'Monthly', subfolderX3 = folder_name,
                filename = all_months[x], file = export_var[[x]])
  )

  rm(list=c('folder_name','export_var'))
}

loop_TerraClim_monthlyVar = function() {
  
  scrape_TerraClim_monthlyVar(var = 'prcp') # scrape monthly precipitation
  scrape_TerraClim_monthlyVar(var = 'pet') # scrape monthly ET0
  scrape_initial_soilMoisture() # scrape initial soil moisture (Dec 1994)
}

