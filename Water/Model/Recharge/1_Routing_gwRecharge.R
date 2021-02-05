source('./Main/Global_functions.R')

library(raster)
library(sf)

#' @source  https://github.com/jgserra18/PCR-GLOBWB_model 
#' @backref Sutanudjaja, E. H., van Beek, R., Wanders, N., Wada, Y., 
#' Bosmans, J. H. C., Drost, N., van der Ent, R. J., de Graaf, I. E. M., 
#' Hoch, J. M., de Jong, K., Karssenberg, D., López López, P., Peßenteiner, 
#' S., Schmitz, O., Straatsma, M. W., Vannametee, E., Wisser, D., and Bierkens, 
#' M. F. P.: PCR-GLOBWB 2: a 5???arcmin global hydrological and water resources model, 
#' Geosci. Model Dev., 11, 2429-2453, https://doi.org/10.5194/gmd-11-2429-2018, 2018.


# get gw recharge from PCR-GWB 1958-2015 and download it
#link = 'https://opendap.4tu.nl/thredds/fileServer/data2/pcrglobwb/version_2019_11_beta/example_output/global_05min_gmd_paper_output/gwRecharge_monthTot_output_1958-01-31_to_2015-12-31_zip.nc'
#download.file(url = link, destfile = file.path(tempdir(), '/gwRecharge_1978_2015.nc'), quiet = F)


# PRE-PROCESSING OF GLOBAL PCR-GWB GW RECHARGE DATA --------------------
# skip if already done 


# get the nc file 
gwRecharge = stack('c:/Users/serrm/Downloads/gwRecharge_monthTot_output_1958-01-31_to_2015-12-31_zip.nc')
#gwRecharge = raster(file.path(tempdir(), '/gwRecharge_1978_2015.nc'))

# get eu mask
eu = sf::read_sf('e:/EU_shp/NUTS2_WGS84.shp')


monthId_finder_1987 = function(gwRecharge_st) {
  #' @param gwRecharge_st the raster stack containing PCR-GWB gw recharge 1958-2015
  #' @description finds the appropriate stack id for the year 1989 onwards
  
  year = 1958; month = 0 #starting point = 1 (January 1958)

  for (i in 1:nlayers(gwRecharge_st)) {
    month = month + 1
    if (month > 12) { month = 1; year = year + 1} 
    
    print(paste0('Month: ', month, '|| Year: ', year))
    if (year == 1987) { return(i)}
  }
}

monthly_gwRecharge = function(gwRecharge_st) {
  #' @param gwRecharge_st the raster stack containing PCR-GWB gw recharge 1958-2015
  #' @description computes the annual gw recharge for the period 1987-2015
  
  start_i = monthId_finder_1987(gwRecharge_st)
  month = 1; year = 1987

  yr_ctr = 1
  annual_store = list()
  month_store = list()
  
  for (i in start_i:nlayers(gwRecharge_st)) {
    print(paste0('Month: ', month))
    # preprocess gw recharge according to Europe mask@WGS84
    r_file = gwRecharge_st[[i]]
    r_file = mask(crop(r_file, extent(eu)), eu)
    
    # if a year has passed
    if (month == 12) { 
      print(paste0('A year has passed || Year = ', year))
      month = 0; year = year + 1; yr_ctr = yr_ctr + 1 # reset month and update counters
      annual_store = append(sum(stack(month_store)), annual_store) # sum the monthly gw recharges
      month_store = list() # empty the month store
    }
    else {
      month_store = append(r_file, month_store)
    }
    
    month = month + 1 # update month
  }
  annual_store = stack(annual_store)
  names(annual_store) = as.character(seq(1987,2015))
  
  return(annual_store)
}

# write the annual gw recharge for Europe
# unit: m yr-1
#d = monthly_gwRecharge(gwRecharge)
#writeRaster(d, 'E:\\EU_gwRecharge_PCR_GWB\\EU_annual_gwRecharge.tiff', options=tifoptions, overwrite = TRUE)

# if downloaded, delete the mainfile which wont be needed anymore
#if (file.exists(file.path(tempdir(), '/gwRecharge_1978_2015.nc'))) { file.remove(file.path(tempdir(), '/gwRecharge_1978_2015.nc')) }


# PREPROCESSING FOR PT ----------------------------------------

eu_gwr = stack('e:/EU_gwRecharge_PCR_GWB/EU_annual_gwRecharge.tif') # m yr-1
eu_gwr = eu_gwr * 1000 # mm yr-1
eu_gwr
pt_laea = rgdal::readOGR('./LULCC/Activity_data/Admin/Municipality.shp')
pt_wgs84 = spTransform(pt_laea, proj4string(eu_gwr))

pt_gwr = mask(crop(eu_gwr, extent(pt_wgs84)),pt_wgs84)
names(pt_gwr) = as.character(seq(1987,2015))

# unit: mm yr-1
pt_gwr_laea = projectRaster(pt_gwr, 
                            crs = CRS('+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs'), 
                            method = 'ngb')
yr = paste0('X',seq(1987,2015))
for (i in 1:nlayers(pt_gwr_laea)) {
  
  export_file(module = 'Water', 
              file = pt_gwr_laea[[i]],
              filename = yr[i], 
              folder = 'Groundwater_recharge', 
              subfolder = 'External_dataset', 
              subfolderX2 = 'PCR_GWB')
  
}

rm(list=c('eu_gwr','pt_laea','pt_wgs84','pt_gwr','pt_gwr_laea'))
