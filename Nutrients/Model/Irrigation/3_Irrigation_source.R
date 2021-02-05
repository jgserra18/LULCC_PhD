source('./Main/Global_functions.R')



get_irrigation_source_AR = function() {
  
  source_AR = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Irrigation',subfolderX2 = 'Irrigation_source', pattern = 'Irrigation_source')
  return(source_AR)
}


municipality_disaggregation_source_AR = function() {
  # spatially disaggregates irrigation water source at the AR level to the municipality scale
  # unit: % water
  
  
  source_AR = get_irrigation_source_AR()
  names(source_AR)[1] <- 'agrarian_region_id'
  
  # spatially disaggregate 
  disagg_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Spatial_disaggregation')
  source_AR <- plyr::join(x = disagg_df, y = source_AR, by = 'agrarian_region_id')
  source_AR <- source_AR[, -c(seq(4,8))]
  
  return(source_AR)
  rm(list='disagg_df')
}


get_irrigation_source = function(source) {
  #' @param source either "gw" or "surface"
  #' @description calculates the fraction of irrigation water for the specified source
  #' @return returns description
  
  source_df = municipality_disaggregation_source_AR()
  source_df = source_df[, c(1,2,3,5,6,7)]
  
  if (source == 'gw') {
    source_df = source_df[, c(1,2,3,6)]
  }
  else {
    source_df = source_df[, seq(1,5)]
    source_df[, 'FRAC_surface'] = source_df[, 'FRAC_dam'] + source_df[, 'FRAC_river']
    source_df = source_df[, -c(4,5)]
  }
  
  return(source_df)
}

