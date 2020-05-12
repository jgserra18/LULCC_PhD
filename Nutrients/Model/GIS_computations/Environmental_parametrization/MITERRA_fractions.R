source('./Main/Global_functions.R')


compute_annual_precipitation_surplus <- function(spatial_res) {
  
  yrs <- paste0('X', seq(1979,2017))
  
  for (i in yrs) {
    
    prec <- get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Environmental_params', subfolder = 'Climatic', subfolderX2 = 'Prec', subfolderX3 =spatial_res, pattern = i)
    et0 <- get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Environmental_params', subfolder = 'Climatic', subfolderX2 = 'ET0', subfolderX3 =spatial_res, pattern = i)
    prec_surplus <- prec - et0
    prec_surplus[prec_surplus<0] <- 0
    
    export_file(module = 'Nutrients', 
                file = prec_surplus, 
                filename = i, 
                folder = 'Environmental_params',
                subfolder = 'Climatic',
                subfolderX2 = 'Surplus_precipitation', 
                subfolderX3 = ifelse(spatial_res == '1', '1x1km','25x25km'))
  }
}


## RUNOFF FRACTIONS ------------------------------------------------------------------------------

compute_annual_runoff_prec_surplus <- function(spatial_res) {
  
  yrs <- paste0('X', seq(1979,2017))
  
  for (i in yrs) {
    
    prec_surplus <- get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Environmental_params', subfolder = 'Climatic', subfolderX2 = 'Surplus_precipitation', subfolderX3 =spatial_res, pattern = i)
    
    rcl_range <- c(-Inf, 50, 0.25,
                   50, 100, 0.50,
                   100, 300, 0.75,
                   300, +Inf, 1)
    prec_surplus <- reclassify(prec_surplus, rcl = rcl_range)
    
    export_file(module = 'Nutrients', 
                file = prec_surplus, 
                filename = i, 
                folder = 'MITERRA_fractions',
                subfolder = 'FRAC_runoff',
                subfolderX2 = 'Surplus_precipitation', 
                subfolderX3 = ifelse(spatial_res == '1', '1x1km','25x25km'))
  }
}


compute_runoff_slope <- function() {
  
  slope <- get_activity_data(module = 'LULCC', mainfolder = 'Activity_data',  folder = 'Environmental_params', subfolder = 'Native',  pattern = 'Slope')
  rcl_range <- c(0,3, 0,
                 3,5, 0.20,
                 5,6, 0.35,
                 6, +Inf, 0.50)
  slope <- reclassify(slope, rcl = rcl_range)
  export_file(module = 'Nutrients', 
              file = slope, 
              filename = 'Slope', 
              folder = 'MITERRA_fractions',
              subfolder = 'FRAC_runoff',
              subfolderX2 = 'Static')
}


compute_runoff_clay_FRAC <- function() {
  
  FRAC_clay <-  get_activity_data(module = 'Nutrients', mainfolder = 'Activity_data',  folder = 'Environmental_params', subfolder = 'Environmental',  pattern = 'FRAC_clay')
  
  rcl_range <- c(0, 18, 0.25,
                 18, 34, 0.75,
                 34, 60, 0.9,
                 60, +Inf, 1)
  FRAC_clay <- reclassify(FRAC_clay, rcl = rcl_range)
  export_file(module = 'Nutrients', 
              file = FRAC_clay, 
              filename = 'FRAC_clay', 
              folder = 'MITERRA_fractions',
              subfolder = 'FRAC_runoff',
              subfolderX2 = 'Static')
}


compute_runoff_DepthToRock <- function() {
  
  DR <- get_activity_data(module = 'Nutrients', mainfolder = 'Activity_data',  folder = 'Environmental_params', subfolder = 'Environmental',  pattern = 'Depth_rock')
  rcl_range <- c(0, 1, 1,
                 1, +Inf, 0.8)
  DR <- reclassify(DR, rcl = rcl_range)
  export_file(module = 'Nutrients', 
              file = DR, 
              filename = 'Depth_rock', 
              folder = 'MITERRA_fractions',
              subfolder = 'FRAC_runoff',
              subfolderX2 = 'Static')
}

## LEACHING FRACTIONS ------------------------------------------------------------------------------

compute_leaching_DepthRoots <- function() {
  
  roots <- get_activity_data(module = 'Nutrients', mainfolder = 'Activity_data',  folder = 'Environmental_params', subfolder = 'Environmental',  pattern = 'Depth_roots')
  rcl_range <- c(0,0.1, NA,
           0.1,1, 0.75,
           1,2,0.75,
           2,3, 0.75,
           3,4, 1,
           4,5, 1)
  roots <- reclassify(roots, rcl = rcl_range)
  export_file(module = 'Nutrients', 
              file = roots, 
              filename = 'Depth_roots', 
              folder = 'MITERRA_fractions',
              subfolder = 'FRAC_leaching',
              subfolderX2 = 'Static')
}

compute_annual_leaching_temperature <- function(spatial_res) {
  
  yrs <- paste0('X', seq(1979,2017))
  
  for (i in yrs) {
    
    avg_temp <- get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Environmental_params', subfolder = 'Climatic', subfolderX2 = 'Avg_temp', subfolderX3 =spatial_res, pattern = i)
    rcl_range <- c(-Inf,5, 1,
                   5, 15, 0.75,
                   15, +Inf, 0.5)
    avg_temp <- reclassify(avg_temp, rcl = rcl_range)
    export_file(module = 'Nutrients', 
                file = avg_temp, 
                filename = i, 
                folder = 'MITERRA_fractions',
                subfolder = 'FRAC_leaching',
                subfolderX2 = 'Avg_temp', 
                subfolderX3 = ifelse(spatial_res == '1', '1x1km','25x25km'))

  }
}

compute_leaching_SOC <- function() {
  
  soc <- get_activity_data(module = 'LULCC', mainfolder = 'Activity_data',  folder = 'Environmental_params', subfolder = 'Native',  pattern = 'SOC_PT')
  rcl_range <- c(0,1, 1,
                 1,2,0.9,
                 2,5,0.75,
                 5, +Inf, 0.5)
  soc <- reclassify(soc, rcl = rcl_range)
  export_file(module = 'Nutrients', 
              file = soc, 
              filename = 'SOC', 
              subfolder = 'FRAC_leaching',
              subfolderX2 = 'Static')
}


compute_leaching_soil_texture <- function() {
  
  soil_text <- get_activity_data(module = 'LULCC', mainfolder = 'Activity_data',  folder = 'Environmental_params', subfolder = 'Native',  pattern = 'SoilTexture')
  rcl_range <- c(0,0.1,NA,
                 0.1,1,1,
                 1, 2, 0.75,
                 2, 3, 0.5,
                 3, 11, 1,
                 11, 12, 0.75,
                 12, 13, 0.5)
  soil_text <- reclassify(soil_text, rcl = rcl_range)
  export_file(module = 'Nutrients', 
              file = soil_text, 
              filename = 'SoilTexture', 
              subfolder = 'FRAC_leaching',
              subfolderX2 = 'Static')
}


compute_leaching_precipitation_surplus <- function(spatial_res) {
  # computes leaching fraction regarding precipitation surplus according to soil texture
  
  yrs <- paste0('X', seq(1979,2017))
  
  rcl_soil_tex <- get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'MITERRA_fractions', subfolder = 'FRAC_leaching',  pattern = 'SoilTexture')
  
  for (i in yrs) {
    
    prec_surplus <- get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Environmental_params', subfolder = 'Climatic', subfolderX2 = 'Surplus_precipitation', subfolderX3 =spatial_res, pattern = i)
    prec_surplus <- resample(prec_surplus, rcl_soil_tex)
    
    # clay soilsl
    clay_frac <- rcl_soil_tex==0.5
    clay_frac[clay_frac==0] <- NA

    rcl_range1 <- c(-Inf, 50, 0.25,
                   50, 100, 0.75,
                   100, 300, 1,
                   300, +Inf, 0.5)
    clay_frac <- clay_frac * prec_surplus
    clay_frac <- reclassify(clay_frac, rcl_range1)

    # sandy loamy soils 
    other_frac <- (rcl_soil_tex==0.75) + (rcl_soil_tex == 1)
    other_frac[other_frac==0] <- NA
    
    rcl_range2 <- c(-Inf, 50, 0.25,
                   50, 100, 0.50,
                   100, 300, 0.75,
                   300, +Inf, 1)
    other_frac <- other_frac * prec_surplus
    other_frac <- reclassify(other_frac, rcl_range2)
    
    leaching_PS <- mosaic(other_frac, clay_frac, fun=sum)
    leaching_PS <- mask(leaching_PS, rcl_soil_tex)
    
    export_file(module = 'Nutrients', 
                file = leaching_PS, 
                filename = i, 
                folder = 'MITERRA_fractions',
                subfolder = 'FRAC_leaching',
                subfolderX2 = 'Surplus_precipitation', 
                subfolderX3 = ifelse(spatial_res == '1', '1x1km','25x25km'))
    
  }
  rm(list=c('rcl_soil_tex', 'prec_surplus', 'clay_frac', 'other_frac'))
}


