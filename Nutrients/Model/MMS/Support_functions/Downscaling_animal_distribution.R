source('./Main/Global_functions.R')
source('./Nutrients/Model/MMS/Support_functions/TimeExtrapolation_MMSparams.R')



#* RULE-BASE GRAZING FRACTION DOWNSCALING 
#* Animals can graze in intensive and/or extensive pastures


# 1 - correct grazing fractions to municipalities without pastures ----------------------------------------------------------------------------------------------


create_municipality_grazing_FRAC = function(param, pathway='Grazing') {
  
  yrs = paste0('X',seq(1987,2017))
  
  FRAC_grazing_PT = correct_share_MMS_pathway(pathway, param)
  FRAC_grazing_muni = get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  FRAC_grazing_muni[, yrs] = sapply(yrs, function(x) FRAC_grazing_muni[,x] = 0)
  
  # "downscale"
  for (c in 4:ncol(FRAC_grazing_muni)) {

    FRAC_grazing_muni[, c] = FRAC_grazing_PT[, c-1]
  }
  
  return(FRAC_grazing_muni)
}


correct_livestock_grazing_fractions = function(main_param, param) {
  # corrects animal grazing fractions according to the existence of pastures
  # unit: %Animal_pop

  perm_grass = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = 'Pastures', pattern = 'Extensive_pasture')
  temp_grass = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = 'Pastures', pattern = 'Intensive_pasture')
  
  yrs = paste0('X',seq(1987,2017))
  grass =temp_grass
  grass[, yrs] = sapply(yrs, function(x) round(temp_grass[,x] + perm_grass[,x] , 1))
  
  yrs = paste0('X',seq(1987,2017))
  FRAC_grazing = create_municipality_grazing_FRAC(param)
  
  for (r in 1:nrow(FRAC_grazing)) {
    
    for (c in 4:ncol(FRAC_grazing)) {
      
      # if there is no grassland, set FRAC_grazing to 0, else maintain
      if (grass[r,c] == 0) {
        
        FRAC_grazing[r,c] = 0
      }
    }
  }
  
  return(FRAC_grazing)
  rm(list=c('grass'))
}


loop_corrected_livestock_grazing_fractions = function() {
  
  standard_params <- get_standard_params_list(main_param = 'Animals')
  
  for (i in 1:nrow(standard_params)) {
    
    main_param = standard_params[i,2]
    param = standard_params[i,1]
    
    # update and export grazing fractions
    FRAC_grazing_new = correct_livestock_grazing_fractions(main_param, param)
    export_file(module = 'Nutrients', folder = 'Activity_data', subfolder = 'General_params', subfolderX2 = 'Animals', subfolderX3 = 'Downscaled_distribution', 
                subfolderX4 = 'Grazing', subfolderX5 = main_param, file = FRAC_grazing_new, filename = param)
  }
}


## dairy cow gross manure conditions -----------------------------------------------------------------

grazing_dairy_cows_condition <- function(dairy_graz_frac) {
  # assumption: no grazing in Agrarian Region Entre Douro e Minho
  
  disagg_df = get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Spatial_disaggregation')
  rows_AR_1 = which(disagg_df$agrarian_region_id == '1')
  
  yrs = paste0('X', seq(1987,2017))
  disagg_df[, yrs] = sapply(yrs, function(x) disagg_df[,x] = NA)
  
  disagg_df[-rows_AR_1, yrs] = sapply(yrs, function(x) disagg_df[-rows_AR_1, x] = dairy_graz_frac[-rows_AR_1, x])
  disagg_df[rows_AR_1, yrs] = sapply(yrs, function(x) disagg_df[rows_AR_1, x] = 0)
  
  return(disagg_df)
}


export_updated_FRAC_grazing_dairy_cow = function(main_param = 'Bovine', param = 'Dairy_cows') {
  
  FRAC_grazing = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Animals', subfolderX2 = 'Downscaled_distribution', subfolderX3 = 'Grazing', subfolderX4 = 'Bovine', pattern = 'Dairy_cows')
  FRAC_grazing = grazing_dairy_cows_condition(FRAC_grazing)
  export_file(module = 'Nutrients', folder = 'Activity_data', subfolder = 'General_params', subfolderX2 = 'Animals', subfolderX3 = 'Downscaled_distribution', 
              subfolderX4 = 'Grazing', subfolderX5 = main_param, file = FRAC_grazing, filename = param)
}




# UPDATE HOUSING AND YARD FRACTIONS ------------------------------------------------------------------------



update_FRAC_housing_yards = function(main_param, param) {
  
  FRAC_grazing = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Animals', subfolderX2 = 'Downscaled_distribution', subfolderX3 = 'Grazing', subfolderX4 = main_param, pattern = param)
  FRAC_housing = create_municipality_grazing_FRAC(param, 'Housing')
  FRAC_yards = create_municipality_grazing_FRAC(param, 'Yards')
  
  # update yard and housing fractions
  yrs = paste0('X',seq(1987,2017))
  
  FRAC_yards[, yrs] = sapply(yrs, function(x) round(FRAC_yards[, x] / (FRAC_yards[,x] + FRAC_housing[,x] + FRAC_grazing[,x]), 3))
  FRAC_housing[, yrs] = sapply(yrs, function(x) round(FRAC_housing[, x] / (FRAC_yards[,x] + FRAC_housing[,x] + FRAC_grazing[,x]), 3))
  
  # check
  tot = FRAC_yards
  tot[, yrs] = sapply(yrs, function(x) round(FRAC_yards[,x] + FRAC_housing[,x] + FRAC_grazing[,x], 0))
  
  return(list(FRAC_housing = FRAC_housing, FRAC_yards = FRAC_yards, total_frac = tot))
}


loop_updated_FRAC_housing_yards = function() {
  
  standard_params <- get_standard_params_list(main_param = 'Animals')
  
  for (i in 1:nrow(standard_params)) {
    
    main_param = standard_params[i,2]
    param = standard_params[i,1]
    
    # update and export grazing fractions
    FRAC_housing = update_FRAC_housing_yards(main_param, param)[[1]]
    FRAC_yards = update_FRAC_housing_yards(main_param, param)[[2]]
    
    export_file(module = 'Nutrients', folder = 'Activity_data', subfolder = 'General_params', subfolderX2 = 'Animals', subfolderX3 = 'Downscaled_distribution', 
                subfolderX4 = 'Yards', subfolderX5 = main_param, file = FRAC_yards, filename = param)
    export_file(module = 'Nutrients', folder = 'Activity_data', subfolder = 'General_params', subfolderX2 = 'Animals', subfolderX3 = 'Downscaled_distribution', 
                subfolderX4 = 'Housing', subfolderX5 = main_param, file = FRAC_housing, filename = param)
  }
}
