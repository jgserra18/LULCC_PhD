source('./Main/Global_functions.R')


compute_total_annual_atmospheric_Ndeposition <- function(year) {
  
  atmN_rdn <- get_activity_data(module = 'Nutrients', subfolder = 'Nutrient_params', subfolderX2 = 'N', subfolderX3 = 'Atmospheric_deposition', pattern = paste0(year,'_WDEP_RDN'))
  atmN_oxn <- get_activity_data(module = 'Nutrients', subfolder = 'Nutrient_params', subfolderX2 = 'N', subfolderX3 = 'Atmospheric_deposition', pattern = paste0(year,'_WDEP_OXN'))
  
  atmN_tot <- atmN_rdn + atmN_oxn
  atmN_tot <- atmN_tot / 100 # convert to kg N ha-1 yr-1
  
  return(atmN_tot)
  rm(list=c('atmN_rdn','atmN_oxn'))
}

loop_atmospheric_Ndeposition <- function() {
  
  yrs <- seq(2000,2017)
  
  for (i in yrs) {
    
    atmN <- compute_total_annual_atmospheric_Ndeposition(i)
    export_file(module = 'Nutrients', 
                file = atmN, 
                filename = paste0('AtmN_',i), 
                folder = 'Atmospheric_deposition', 
                subfolder = 'N')
  }
}


atmN_files <- './Nutrients/Output/Atmospheric_deposition/N/'
atmN_files <- list.files(atmN_files, full.names = T)

atmN_files <- lapply(atmN_files, raster)
atmN_files <- stack(atmN_files)
time <- seq(2000,2017)


tm_shape(atmN_files) + 
  tm_raster(palette = c('blue1','green1','yellow1','orange1','red1'), style='cont')
